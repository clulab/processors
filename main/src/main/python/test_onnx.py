from pytorch2onnx import *
import json
import numpy as np
from pytorch.seqScorer import *

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--model_file', type=str, help='Filename of the model.')
    parser.add_argument('--config', type=str, help='Filename of the configuration.')
    parser.add_argument('--seed', type=int, default=1234)
    args = parser.parse_args()

    config = ConfigFactory.parse_file(f'../resources/org/clulab/{args.config}.conf')
    taskManager = TaskManager(config, args.seed)
    constEmbeddings = ConstEmbeddingsGlove.get_ConstLookupParams()

    x2i = json.load(open(args.model_file+".json"))

    c2i = x2i[0]['x2i']['initialLayer']['c2i']
    w2i = x2i[0]['x2i']['initialLayer']['w2i']
    t2i = x2i[1]['x2i']['finalLayer']["t2i"]
    i2t = {i:t for t, i in t2i.items()}

    torch.manual_seed(taskManager.random)
    random.seed(taskManager.random)

    onnx_model = onnx.load("model.onnx")
    onnx.checker.check_model(onnx_model)
    char_model = onnx.load("char.onnx")
    onnx.checker.check_model(char_model)

    ort_session = onnxruntime.InferenceSession("model.onnx")
    ort_char = onnxruntime.InferenceSession("char.onnx")

    scoreCountsByLabel = ScoreCountsByLabel()

    for taskId in range(0, taskManager.taskCount):
        taskName = taskManager.tasks[taskId].taskName
        sentences = taskManager.tasks[taskId].testSentences
        if sentences:
            reader = MetalRowReader()
            for sent in sentences:
                annotatedSentences = reader.toAnnotatedSentences(sent)

                for asent in annotatedSentences:
                    sentence = asent[0]
                    goldLabels = asent[1]

                    words = sentence.words

                    char_embs = []
                    for word in words:
                        char_ids = np.array([c2i.get(c, UNK_EMBEDDING) for c in word])
                        ort_inputs = {ort_char.get_inputs()[i].name: x for i, x in enumerate([char_ids])}
                        ort_outs = ort_char.run(None, ort_inputs)
                        char_embs.append(ort_outs[0])
                    char_embs = np.stack(char_embs)
                    embed_ids = np.array([constEmbeddings.w2i[word] if word in constEmbeddings.w2i else 0 for word in words])
                    embeddings = constEmbeddings.emb(embed_ids)
                    word_ids = np.array([w2i[word] if word in w2i else 0 for word in words])

                    dummy_input = (embeddings, word_ids, char_embs)

                    ort_inputs = {ort_session.get_inputs()[i].name: x for i, x in enumerate(dummy_input)}
                    ort_outs = ort_session.run(None, ort_inputs)

                    emissionScores = ort_outs[0]
                    preds = [i2t[np.argmax(es)] for es in emissionScores]

                    sc = SeqScorer.f1(goldLabels, preds)
                    scoreCountsByLabel.incAll(sc)


    print (f"Accuracy : {scoreCountsByLabel.accuracy()}")
    print (f"Precision : {scoreCountsByLabel.precision()}")
    print (f"Recall on : {scoreCountsByLabel.recall()}")
    print (f"Micro F1 : {scoreCountsByLabel.f1()}")
    for label in scoreCountsByLabel.labels():
        print (f"\tP/R/F1 for label {label} ({scoreCountsByLabel.map[label].gold}): {scoreCountsByLabel.precision(label)} / {scoreCountsByLabel.recall(label)} / {scoreCountsByLabel.f1(label)}")

            