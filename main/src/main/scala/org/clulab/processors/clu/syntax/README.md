## How to train the maltparser model

First, download then maltparser from here: http://www.maltparser.org/download.html

We are currently using version `1.9.0`. If you change the version number, please copy again the corresponding `appdata/` directory from the malt distribution to this location in `processors`: `models/src/main/resources/appdata/`

Use the following commands to train the forward, i.e., left-to-right model:

`mkdir -p output`

`java -jar maltparser-1.9.0/maltparser-1.9.0.jar -w output -c en-forward-nivre -i <COMBINED TRAIN FILE FROM WSJ AND GENIA> -a nivreeager -m learn -l liblinear -llo -s_4_-c_0.1 -d POSTAG -s Input[0] -T 1000 -F NivreEager.xml`

where:
* The combined train file is available on our servers at: `corpora/processors/deps/combined/wsjtrain-wsjdev-geniatrain-geniadev.conllx`
* The `NivreEager.xml` is the one located under `appdata/features/liblinear/conllx/NivreEager.xml`
