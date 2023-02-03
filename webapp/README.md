# webapp

This subproject of processors houses code that implements a web page that displays
output from processors in HTML format.  There are diagrams, text, and tables.

![Webapp window with text](../docs/webapp_full.png?raw=True")

## Execution

One can start the webapp directly from within `sbt` in development mode with the command `webapp/run`.  The web page will then be accessible at [http://localhost:9000](http://localhost:9000).

## Dockerization

There is also a `docker.sbt` file which allows one to build an image from within `sbt`.  A command alias `dockerizeWebapp` has been set up for it.

To run the resulting image, use a command like
```bash
docker run -d --env secret=<secret> -p 9000:9000 --restart unless-stopped processors-webapp:latest &
```
The secret is the value for `play.http.secret.key` used in
[conf/application.conf](./conf/application.conf) to protect the application.
