#!/bin/bash

#
# This installs your local checkout of processors into the maven repository
# You need this to access the bleeding edge of processors from other projects
#

POM_VERSION=2.2
INSTALL_AS_VERSION=2.2

mvn clean package; ./scripts/mk_model_jar $POM_VERSION

mvn install:install-file --quiet -DartifactId=processors -DgroupId=edu.arizona.sista -Dversion=$INSTALL_AS_VERSION -Dpackaging=jar -Dfile=target/processors-$POM_VERSION-models.jar -Dclassifier=models
mvn install:install-file --quiet -DartifactId=processors -DgroupId=edu.arizona.sista -Dversion=$INSTALL_AS_VERSION -Dpackaging=jar -Dfile=target/processors-$POM_VERSION.jar

