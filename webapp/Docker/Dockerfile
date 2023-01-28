FROM ubuntu:16.04

# Install base packages
RUN apt-get update && apt-get install -y \
    build-essential \
    software-properties-common \ 
    dialog \
    git \
    graphviz \
    libgraphviz-dev \
    vim

# Install Java
RUN apt-get -y install software-properties-common
RUN add-apt-repository -y ppa:openjdk-r/ppa
RUN apt-get update
RUN apt-get -y install openjdk-8-jdk
RUN export JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
RUN export PATH="$JAVA_HOME/bin:$PATH"

# Install Scala
WORKDIR /
RUN apt-get remove scala-library scala
RUN apt-get install wget
RUN wget http://scala-lang.org/files/archive/scala-2.12.14.deb
RUN dpkg -i scala-2.12.14.deb
RUN apt-get update && apt-get install scala
RUN apt-get install apt-transport-https -y
RUN echo "deb https://dl.bintray.com/sbt/debian /" |  tee -a /etc/apt/sources.list.d/sbt.list
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
RUN apt-get update && apt-get install sbt -y

# Clone the Eidos repo
RUN git clone https://github.com/clulab/eidos.git
WORKDIR /eidos
RUN ["bash", "-c", "echo -e '\nplay.filters.hosts {allowed = [\".:9000\"]}' >> ./webapp/conf/application.conf"]
RUN sbt assembly

# Run Web Service
EXPOSE 9000
ENTRYPOINT ["sbt", "webapp/run"]
