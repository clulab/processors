from pytorch2onnx import *
import json
import numpy as np
from pytorch.seqScorer import *
import time

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
    tag2i = x2i[0]['x2i']['initialLayer']['tag2i']
    n2i = x2i[0]['x2i']['initialLayer']['ne2i']
    useIsPredicate = x2i[0]['x2i']['initialLayer']['useIsPredicate']
    distanceWindowSize = x2i[0]['x2i']['initialLayer']['distanceWindowSize']
    positionEmbeddingSize = x2i[0]['x2i']['initialLayer']['positionEmbeddingSize']

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
    start_time = time.time()
    for taskId in range(0, taskManager.taskCount):
        taskName = taskManager.tasks[taskId].taskName
        sentences = taskManager.tasks[taskId].testSentences
        if sentences:
            reader = MetalRowReader()
            for sent in sentences:
                print (sent)
                annotatedSentences = reader.toAnnotatedSentences(sent)

                for asent in annotatedSentences:
                    sentence = asent[0]
                    goldLabels = asent[1]

                    words = sentence.words
                    tags = sentence.posTags
                    nes = sentence.neTags
                    headPositions = np.array(sentence.headPositions)
                    
                    char_embs = []
                    for word in words:
                        char_ids = np.array([c2i.get(c, UNK_EMBEDDING) for c in word])
                        ort_inputs = {ort_char.get_inputs()[i].name: x for i, x in enumerate([char_ids])}
                        ort_outs = ort_char.run(None, ort_inputs)
                        char_embs.append(ort_outs[0])
                    char_embs = np.stack(char_embs)
                    embed_ids = torch.LongTensor([constEmbeddings.w2i[word] if word in constEmbeddings.w2i else 0 for word in words])
                    embeddings = constEmbeddings.emb(embed_ids).detach().cpu().numpy()
                    word_ids = np.array([w2i[word] if word in w2i else 0 for word in words])
                    tags_ids = np.array([tag2i[tag] if tag in tag2i else 0 for tag in tags])
                    nes_ids = np.array([n2i[ne] if ne in n2i else 0 for ne in nes])
                    pred_embs = torch.FloatTensor([1 if i==predicatePosition else 0 for i, predicatePosition in enumerate(headPositions)]).unsqueeze(1).numpy()
                    dists = [max(i-predicatePosition+distanceWindowSize+1, 0) if i-predicatePosition <= distanceWindowSize else 2 * distanceWindowSize + 2 for i, predicatePosition in enumerate(headPositions)]
                    dists = np.array(dists)
                    dummy_input = (embeddings, word_ids, char_embs, tags_ids, nes_ids, pred_embs, dists, headPositions)
                    ort_inputs = {ort_session.get_inputs()[i].name: x for i, x in enumerate(dummy_input)}
                    ort_outs = ort_session.run(None, ort_inputs)

                    preds = [i2t[i] for i in ort_outs[0]]

                    sc = SeqScorer.f1(goldLabels, preds)
                    scoreCountsByLabel.incAll(sc)


    print (f"Accuracy : {scoreCountsByLabel.accuracy()}")
    print (f"Precision : {scoreCountsByLabel.precision()}")
    print (f"Recall on : {scoreCountsByLabel.recall()}")
    print (f"Micro F1 : {scoreCountsByLabel.f1()}")
    for label in scoreCountsByLabel.labels():
        print (f"\tP/R/F1 for label {label} ({scoreCountsByLabel.map[label].gold}): {scoreCountsByLabel.precision(label)} / {scoreCountsByLabel.recall(label)} / {scoreCountsByLabel.f1(label)}")
    duration = time.time() - start_time
    print (duration)
            