# Eidos

## Docker for webapp

This directory contains several Dockerfiles which generate containers that run the Eidos web application and web service.

* Dockerfile - This should match the next and remains for compatability purposes
* DockerfileRun - This runs the webapp through sbt in development mode
* DockerfileRunProd - This runs the webapp through sbt in production mode
* DockerfileStage - This runs the webapp directly in production mode

From this directory you can build the container with:

```
docker build -f <Dockerfile> . -t eidos-webservice
```

You can run the container with:

```
docker run -id -p 9000:9000 eidos-webservice
```

This launches the container and exposes port 9000. You can then navigate to `localhost:9000` to access the web application and may submit requests to `localhost:9000/process_text` as described [here](https://github.com/clulab/eidos#web-service).

### Configuration
Currently, this container is built with grounding activated (`useW2V = true`) and Google's pre-trained vectors are used, not GloVe.

