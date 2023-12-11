from transformers import DebertaModel, AutoTokenizer
import torch
import numpy as np
from datasets import load_dataset
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('--model_type', dest='model_type', type=str)
parser.add_argument('--seed', dest='seed', type=int)
parser.add_argument('--tokenizer', dest='tokenizer', type=str)
parser.add_argument('--context_size', dest='context_size', type=int)
parser.add_argument('--device', dest='device', type=str)
parser.add_argument('--huggingface_dataset', dest='huggingface_dataset', type=str)
parser.add_argument('--huggingface_subpart', dest='huggingface_subpart', type=str)
parser.add_argument('--number_of_tokens', dest='number_of_tokens', type=int)
parser.add_argument('--output', dest='output', type=str)
args = parser.parse_args()

context = int(args.context_size) # 512
model_type = args.model_type # microsoft/deberta-base
tokenizer = args.tokenizer # microsoft/deberta-base
device = args.device # cuda
huggingface_dataset = args.huggingface_dataset # wikipedia
huggingface_subpart = args.huggingface_subpart # wikipedia
number_of_tokens = int(args.number_of_tokens) # 10000000
output = str(args.output) # deberta_embd
seed = -1 # 42

if args.seed:
    seed = int(args.seed) # deberta_embd

torch.set_default_device(device)

model = DebertaModel.from_pretrained(model_type).to(device)
tokenizer = AutoTokenizer.from_pretrained(tokenizer)

if seed != -1:
    np.random.seed(seed)

if huggingface_dataset:
    if huggingface_subpart:
        dataset = load_dataset(huggingface_dataset, huggingface_subpart).with_format(type="torch")
    else:
        dataset = load_dataset(huggingface_dataset).with_format(type="torch")
else:
    dataset = load_dataset('json', data_files={'train': 'text_data.json'})

tokens_so_far = 0

sum_tensors = {}
crr_tensors = {}

i = 0

print(len(dataset))

while tokens_so_far < number_of_tokens:  # 10 million

    index = np.random.randint(0, len(dataset['train']))

    text = dataset['train'][index]['text']

    tokenized = tokenizer(text, return_tensors='pt')

    tokens = tokenized['input_ids'][0]

    if len(tokens) <= context + 1:

        tokens_so_far += len(tokens)
        sliced_output = {key: value[:, :] for key, value in tokenized.items()}

    else:

        i = np.random.randint(0, len(tokens) - context - 1)
        tokens_so_far += context

        sliced_output = {key: value[:, i:i+context] for key, value in tokenized.items()}

    outputs = model(**sliced_output)

    print(tokens_so_far)

    for token, emb in zip(tokens, outputs.last_hidden_state[0]):
        if token.item() not in sum_tensors:
            sum_tensors[token.item()] = emb.detach()
            crr_tensors[token.item()] = 1
        else:
            sum_tensors[token.item()] += emb.detach()
            crr_tensors[token.item()] += 1

print("Finished!")

number_seen_once = 0
for value in crr_tensors.values():
    if value == 1:
        number_seen_once += 1

print(str(number_seen_once) + " tokens have only been seen once!")

unseen_tokens = []
crr = 0

for unseen in tokenizer.get_vocab():
    crr += 1
    if crr%1000 == 0:
        print(crr)
        break
    if tokenizer.vocab[unseen] not in sum_tensors.keys():
        unseen_tokens.append(unseen)

print("Number of tokens unseen: ", (len(tokenizer.get_vocab())-len(sum_tensors)), '/', len(tokenizer.get_vocab()))

print("Unseen tokens list: ", unseen_tokens)
print("Number of unseen tokens metric 2: ", len(unseen_tokens))

for token in sum_tensors.keys():
    sum_tensors[token] = sum_tensors[token].clone()/crr_tensors[token]

file_embd = open(output + '.tsv', 'w', encoding="utf-8")

file_embd.write(str(len(sum_tensors)) + " " + str(len(sum_tensors[next(iter(sum_tensors))].cpu().detach().numpy())) + "\n")
for token in sum_tensors.keys():
    file_embd.write(str(tokenizer.convert_ids_to_tokens([token])[0]) + " " + (" ".join([str(x) for x in sum_tensors[token].cpu().detach().numpy()])) + "\n")