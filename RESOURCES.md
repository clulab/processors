# Resources

## Glove

Later generations of this project do not include a text version of glove.  This saves developers a ~1GB download.  Since the old file did not really have anywhere to live that didn't result in it being packaged and distributed to users, not including it here also saves them the download (and unnecessary incorporation into a Docker image, etc.)  The downside is that these additional instructions are necessary to explain what happened so that it can be reproduced.  There may be a better solution soon, but this describes the current situation.

1. The text version can be restored to the project by inserting the line `"org.clulab" % "glove-840b-300d-10f" % "1.0.0",` into `main/build.sbt`.  This artifact is stored on the [CLU Lab Artifactory server](http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release) which should still be listed among the `resolvers`, especially since it also supplies the kryo version.

1. All the code needed to convert the text version into the kryo version is still included with this project.  That is almost a necessity because it is for now the goal to keep the artifact free of code which would have to account for both Scala version and the kryo library version.  These are taken care of by this processors project.

1. The `CompactWordEmbeddingMap` class includes a companion object which can be used as a factory to produce maps from text files (`loadTxt()`), Java serializations (`loadSer()`), and kryo serializations (`loadKryo()`).  The map is responsible for serializing itself in `save(_)` and `saveKryo(_)`.

1. Included in the same `CompactWordEmbeddingMap.scala` file is `CompactWordEmbeddingMapApp`, which reads the text file and produces the two serialized versions.  It is currently configured to read the text from a file, but a boolean value will change it to a resource.  So, either the text file can be manually extracted from the resource, or the filename in the app can be changed to match the text resource (`/org/clulab/glove/glove.840B.300d.10f.txt`).  In the end, though, there should be a new file `../glove.840B.300d.10f.kryo`.

1. The kryo file needs to be packaged for Artifactory.  That is done independently of this project.  At the moment there is no dedicated repo for the job, partly because it is not especially difficult.  There is a template for a project that can be used to release a resource to Artifactory.  It is stored in a [subproject](https://github.com/clulab/research/tree/master/artifactory) of the research repo.  If the kryo file is added, some names changed, and credentials procured, the kryo serialization can be published.
   
1. One bit of customization was added to the present release, and that is the use of an uncompressed jar file.  It takes extra time and effort to extract a zipped resource out of a jar file.  The jar only needs to be downloaded once, while it might need to be uncompressed hundreds of times.  A decision was made to sacrifice download time in exchange for the more direct extraction from the resource.  The code to do this is not yet archived.

## DyNet Models
