from pyhocon import ConfigFactory
import argparse
from pytorch.taskManager import TaskManager
from pytorch.metal import Metal

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--model_file', type=str, help='Filename of the model.', nargs='+')
    parser.add_argument('--train', action='store_true', help='Set the code to training purpose.')
    parser.add_argument('--test', action='store_true', help='Set the code to testing purpose.')
    parser.add_argument('--shell', action='store_true', help='Set the code to shell mode.')
    parser.add_argument('--config', type=str, help='Filename of the configuration.')
    parser.add_argument('--seed', type=int, default=1234)
    args = parser.parse_args()

    if args.train:
        config = ConfigFactory.parse_file(f'../resources/org/clulab/{args.config}.conf')
        taskManager = TaskManager(config, args.seed)
        modelName = args.model_file[0]

        mtl = Metal(taskManager, None)
        mtl.train(modelName)
    elif args.test:
        config = ConfigFactory.parse_file(f'../resources/org/clulab/{args.config}.conf')
        taskManager = TaskManager(config, args.seed)
        modelName = args.model_file
        if len(modelName)==1:
            model = Metal.load(modelName[0])
        else:
            model = Metal.load_multi(modelName)
        mtl = Metal(taskManager, model)
        mtl.test()
    elif args.shell:
        pass