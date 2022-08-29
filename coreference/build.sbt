name := "processors-coreference"
description := "processors-coreference"

pomIncludeRepository := { (repo: MavenRepository) =>
  repo.root.startsWith("http://artifactory.cs.arizona.edu")
}

// for processors-models
resolvers += ("Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release").withAllowInsecureProtocol(true)

libraryDependencies ++= {

  Seq(
  )
}
