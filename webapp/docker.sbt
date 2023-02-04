import NativePackagerHelper._
import com.typesafe.sbt.packager.docker.{Cmd, CmdLike, DockerChmodType, DockerPermissionStrategy}

val topDir = "/processors/webapp"
val appDir = topDir + "/app"
val binDir = appDir + "/bin/" // The second half is determined by the plug-in.  Don't change.
val app = binDir + "processors-webapp"
val port = 9000
val tag = "1.7.0"

Docker / defaultLinuxInstallLocation := appDir
Docker / dockerBaseImage := "openjdk:8"
Docker / daemonUser := "nobody"
Docker / dockerExposedPorts := List(port)
Docker / maintainer := "Keith Alcock <docker@keithalcock.com>"
Docker / mappings := (Docker / mappings).value.filter { case (_, string) =>
  // Only allow the app into the /app/bin directory.  Other apps that
  // might be automatically discovered are to be excluded.
  !string.startsWith(binDir) || string == app
}
Docker / packageName := "processors-webapp"
Docker / version := tag

dockerAdditionalPermissions += (DockerChmodType.UserGroupPlusExecute, app)
dockerChmodType := DockerChmodType.UserGroupWriteExecute
// dockerCmd := Seq(s"-Dhttp.port=$port")
dockerEntrypoint := Seq(app)
dockerEnvVars := Map(
  "_JAVA_OPTIONS" -> "-Xmx10g -Xms10g -Dfile.encoding=UTF-8 -Dconfig.resource=processors.conf"
)
dockerPermissionStrategy := DockerPermissionStrategy.MultiStage
dockerUpdateLatest := true

// Run "show dockerCommands" and use this to edit as appropriate.
dockerCommands := dockerCommands.value.flatMap { dockerCommand: CmdLike =>
  dockerCommand match {
    // Make sure that the appDir can be written for file locking.
    // case Cmd("USER", oldArgs @ _*) if (oldArgs.length == 1 && oldArgs.head == "1001:0") =>
    case Cmd("USER", "1001:0") =>
      Seq(
        Cmd("RUN", "chmod", "775", appDir),
        dockerCommand
      )
    case _ =>
      Seq(dockerCommand)
  }
}

Global / excludeLintKeys += Docker / dockerBaseImage
