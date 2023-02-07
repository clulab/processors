# webapp

This subproject of processors houses code that implements a web page that displays
output from processors in HTML format.  There are diagrams and tables resulting in a display much like the image below.

![Webapp window with text](../docs/webapp_full.png?raw=True")

## Execution

One can start the webapp directly from within `sbt` in development mode with the command `webapp/run`.  The web page will then be accessible at [http://localhost:9000](http://localhost:9000).  If you need to debug the webapp, use `sbt -jvm-debug 5005` and then configure IntelliJ for "Remote JVM Debug".  You should then be able to set breakpoints in `org.clulab.processors.webapp.controllers.HomeController`, for example.

## Configuration

The configuration for Odin used in the `HomeController` is based on the OdinStarter (`org.clulab.odinstarter.OdinStarter`) App.  The NER and rules files for the App are configured in code.  For the webapp, the same files are instead specified in a configuration file `processors.conf` under the keys `customLexiconNer` and `extractorEngine`.  You would change filenames there or change the contents of the Odin files in the directory `main/src/main/resources/org/clulab/odinstarter`.


## Dockerization

There is also a `docker.sbt` file which allows one to build an image from within `sbt`.  A command alias `dockerizeWebapp` has been set up for it.

To run the resulting image, use a command like
```bash
docker run -d --env secret=<secret> -p 9000:9000 --restart unless-stopped processors-webapp:latest &
```
The secret is the value for `play.http.secret.key` used in
[conf/application.conf](./conf/application.conf) to protect the application.

## Limitations

The webapp presently only works for Scala 2.12 because of some library and plug-in conflicts.  The Play framework itself is not ready for Scala 3.  Scala 2.12 is the default version for processors, so things should just work for the most part.  Because of this limitation, however, the webapp is not "aggregated" and will not be published or released with the other projects.  To publish, make sure the version is set as desired and perform a `webapp/publish`.
