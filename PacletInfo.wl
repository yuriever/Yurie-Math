(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "Yurie/Math",
    "Description" -> "Math utilities",
    "Creator" -> "Yurie",
    "SourceControlURL" -> "https://github.com/yuriever/Yurie-Math",
    "License" -> "MIT",
    "PublisherID" -> "Yurie",
    "Version" -> "1.0.0",
    "WolframVersion" -> "14.1+",
    "PrimaryContext" -> "Yurie`Math`",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          "Yurie`Math`"
        }
      },
      {
        "Kernel",
        "Root" -> "Utility",
        "Context" -> {
          "Yurie`Math`Info`",
          "Yurie`Math`DLMFWorkflow`"
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
          {"TestSource", "TestSource"}
        }
      }
    }
  |>
]
