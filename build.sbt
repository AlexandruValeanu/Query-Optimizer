name := "Query Optimizer"

version := "0.1"

scalaVersion := "2.12.7"

unmanagedJars in Compile += file(Path.userHome+"/Downloads/scala-parser-combinators_2.12-1.0.6.jar")
unmanagedJars in Compile += file(Path.userHome+"/ortools/or-tools-6.7.2/lib/com.google.ortools.jar")
unmanagedJars in Compile += file(Path.userHome+"/ortools/or-tools-6.7.2/lib/protobuf.jar")