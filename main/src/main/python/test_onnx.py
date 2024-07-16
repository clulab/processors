from pytorch2onnx import *
import json
import numpy as np
from pytorch.seqScorer import *
import time

def viterbi_decode(feats, transitions, t2i):
    backpointers = []

    # Initialize the viterbi variables in log space
    init_vvars = np.full((1, len(t2i)), -10000.)
    init_vvars[0][t2i[START_TAG]] = 0

    # forward_var at step i holds the viterbi variables for step i-1
    forward_var = init_vvars
    for feat in feats:
        bptrs_t = []  # holds the backpointers for this step
        viterbivars_t = []  # holds the viterbi variables for this step

        for next_tag in range(len(t2i)):
            # next_tag_var[i] holds the viterbi variable for tag i at the
            # previous step, plus the score of transitioning
            # from tag i to next_tag.
            # We don't include the emission scores here because the max
            # does not depend on them (we add them in below)
            next_tag_var = forward_var + transitions[next_tag]
            best_tag_id = np.argmax(next_tag_var, 1)[0]
            bptrs_t.append(best_tag_id)
            viterbivars_t.append(next_tag_var[0][best_tag_id].reshape(1))
        # Now add in the emission scores, and assign forward_var to the set
        # of viterbi variables we just computed
        forward_var = (np.concatenate(viterbivars_t) + feat).reshape(1, -1)
        backpointers.append(bptrs_t)

    # Transition to STOP_TAG
    terminal_var = forward_var + transitions[t2i[STOP_TAG]]
    best_tag_id = np.argmax(terminal_var, 1)[0]
    path_score = terminal_var[0][best_tag_id]

    # Follow the back pointers to decode the best path.
    best_path = [best_tag_id]
    for bptrs_t in reversed(backpointers):
        best_tag_id = bptrs_t[best_tag_id]
        best_path.append(best_tag_id)
    # Pop off the start tag (we dont want to return that to the caller)
    start = best_path.pop()
    assert start == t2i[START_TAG]  # Sanity check
    best_path.reverse()
    return path_score, best_path

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
    start_time = time.time()
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
                    embed_ids = torch.LongTensor([constEmbeddings.w2i[word] if word in constEmbeddings.w2i else 0 for word in words])
                    embeddings = constEmbeddings.emb(embed_ids).detach().cpu().numpy()
                    word_ids = np.array([w2i[word] if word in w2i else 0 for word in words])

                    dummy_input = (embeddings, word_ids, char_embs)

                    ort_inputs = {ort_session.get_inputs()[i].name: x for i, x in enumerate(dummy_input)}
                    ort_outs = ort_session.run(None, ort_inputs)

                    _, ids = viterbi_decode(ort_outs[0], ort_outs[1], t2i)

                    preds = [i2t[i] for i in ids]

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
            