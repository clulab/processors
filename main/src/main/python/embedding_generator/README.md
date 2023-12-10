# README.md for main.py

## Overview
This Python script is designed to analyze text data using a specified model and tokenizer from the Hugging Face library. It processes a given number of tokens and outputs various statistics about token usage. The script can either use a default dataset (`text_data.json`) or a specified subset from a Hugging Face dataset.

## Prerequisites
- Python environment with necessary libraries installed (see `requirements.txt`).
- Access to Hugging Face models and tokenizers.

## Installation
1. Ensure Python is installed on your system.
2. Install required libraries using the provided `requirements.txt` file:
   ```
   pip install -r requirements.txt
   ```

## Usage
The script is run via the command line, with several required and optional arguments:

### Required Arguments
- `--model_type`: Type of the model (e.g., `microsoft/deberta-base`).
- `--tokenizer`: Tokenizer to use (e.g., `microsoft/deberta-base`).
- `--context_size`: The context size for the model (integer).
- `--device`: The device PyTorch should use (e.g., `cuda`).
- `--number_of_tokens`: Number of tokens to process (integer).
- `--output`: Base name of the output file (without `.tsv` extension).

### Optional Arguments
- `--huggingface_dataset`: The Hugging Face dataset to use (default: `text_data.json`).
- `--huggingface_subpart`: Specific subpart of the Hugging Face dataset (not mandatory).

### Examples
1. Using default dataset:
   ```
   /home/gigi/hdd/DeBERTa/env/bin/python main.py --context_size 512 --model_type 'microsoft/deberta-base' --tokenizer 'microsoft/deberta-base' --device 'cuda' --number_of_tokens 1000
   ```
2. Using a Hugging Face dataset:
   ```
   /home/gigi/hdd/DeBERTa/env/bin/python main.py --context_size 512 --model_type 'microsoft/deberta-base' --tokenizer 'microsoft/deberta-base' --device 'cuda' --number_of_tokens 1000 --huggingface_dataset wikipedia --huggingface_subpart 20220301.en
   ```

## Output
- The script will display the number of tokens processed.
- Upon completion, it will show "Finished!".
- It will report the count of tokens appearing only once.
- The script a count to track performance for each 1000 tokens from the vocabulary.
- The final output includes the count of unseen tokens by two metrics and a list of these tokens.
- An output file named `<output>.tsv` will be generated with the results.