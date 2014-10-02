#!/bin/bash

#
# This installs your local checkout of processors into the maven repository
# You need this to access the bleeding edge of processors from other projects
#

VERSION=3.3

mvn clean package; ./scripts/mk_model_jar $VERSION

mvn install:install-file --quiet -DartifactId=processors -DgroupId=edu.arizona.sista -Dversion=$VERSION -Dpackaging=jar -Dfile=target/processors-$VERSION-models.jar -Dclassifier=models
mvn install:install-file --quiet -DartifactId=processors -DgroupId=edu.arizona.sista -Dversion=$VERSION -Dpackaging=jar -Dfile=target/processors-$VERSION.jar

