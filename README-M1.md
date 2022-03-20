# Processors on M1 Macs

This library works natively on M1 Macs due to the fact that our [DyNet wrapper](https://github.com/clulab/fatdynet) includes native support for M1 processors starting with version `0.4.1`.

To install one (or more!) Java Virtual Machines (JVM) on Macs we recommend [sdkman](https://sdkman.io). The steps to follow (instructions by [Gus Hahn-Powell](https://linguistics.arizona.edu/user/gus-hahn-powell)) are the following:

```
# install sdkman from source:
curl -s "https://get.sdkman.io" | bash
# open a new terminal window and run the following:
source "$HOME/.sdkman/bin/sdkman-init.sh"
# test your installation:
sdk version
```

You can configure sdkman using the following command:

```
sdk config
```
which should open `~/.sdkman/etc/config` in your default editor.
For M1 processors, you want to only list ARM64-compatible versions. To this end, alter the following config entry in the config file:
```
sdkman_rosetta2_compatible=false
```

Open a new terminal and try listing ARM64-compatible versions of java:

```
sdk list java
```

To install an ARM64-compatible distribution of Java 11 via `sdkman`, run the following:

```
sdk install java 11.0.14-zulu
```
This version should also work with Intel machines.

To install `sbt` and `scala`, run the following commands:

```
# sbt 1.6
sdk install sbt 1.6.1
# scala 2.12
sdk install scala 2.12.15
```

