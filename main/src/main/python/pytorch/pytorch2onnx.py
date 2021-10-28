import torch

from pytorch.metal import Metal

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--model_file', type=str, help='Filename of the model.')
    args = parser.parse_args()
    modelName = args.model_file
    model = Metal.load(modelName)

    input_names = [ "input" ]
    output_names = [ "output" ]
    
    #In current setting, each layer is a nn.Moudle and we need to export each of them. This is not very elegant...
    for i, layers in enumerate(model):
        if layers.initialLayer is not None:
            #export the initial layer
            dummy_input = (sentence, embeddings)# we need some toy sentence and embeddings here, not sure if onnx is happy with this input though...
            torch.onnx.export(layers.initialLayer, dummy_input_1, "initialLayer_inTask%d.onnx"%i, verbose=True, input_names=input_names, output_names=output_names)
            dummy_input = layers.initialLayer(sentence, embeddings)
        for j, il in enumerate(layers.intermediateLayers):
            #export the intermediate layer layer
            torch.onnx.export(il, dummy_input_2, "intermediateLayer_%d_inTask%d.onnx"%(i,j), verbose=True, input_names=input_names, output_names=output_names)
            dummy_input = il(dummy_input)
        if layers.finalLayer is not None:
            #export the final layer
            torch.onnx.export(layers.finalLayer, dummy_input, "finalLayer_inTask%d.onnx"%i, verbose=True, input_names=input_names, output_names=output_names)