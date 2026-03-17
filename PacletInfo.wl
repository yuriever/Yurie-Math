(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "Yurie/Math",
    "Description" -> "Math utilities",
    "Creator" -> "Yurie",
    "SourceControlURL" -> "https://github.com/yuriever/Yurie-Math",
    "License" -> "MIT",
    "PublisherID" -> "Yurie",
    "Version" -> "5.1",
    "WolframVersion" -> "14.3+",
    "PrimaryContext" -> "Yurie`Math`",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          "Yurie`Math`",
          "Yurie`Math`Lie`"
        }
      },
      {
        "Kernel",
        "Root" -> "Utility",
        "Context" -> {
          "Yurie`Math`Info`",
          "Yurie`Math`DLMFDev`"
        }
      },
      {
        "Kernel",
        "Root" -> "Sandbox/Kernel",
        "Context" -> {
          "Yurie`Math`"
        }
      },
      {
        "AutoCompletionData",
        "Root" -> "AutoCompletionData"
      },
      {
        "Asset",
        "Root" -> ".",
        "Assets" -> {
          {"License", "LICENSE"},
          {"ReadMe", "README.md"},
          {"Test", "Test"},
          {"Sandbox", "Sandbox"},
          {"TestSource", "TestSource"}
        }
      }
    }
  |>
]
