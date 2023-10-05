(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "JerryI/WSP",
    "Description" -> "Wolfram Script Pages",
    "Creator" -> "Kirill Vasin",
    "License" -> "MIT",
    "PublisherID" -> "JerryI",
    "Version" -> "1.0.9",
    "WolframVersion" -> "11+",
    "PrimaryContext" -> "JerryI`WSP`",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {{"JerryI`WSP`", "WSP.wl"}, {"JerryI`WSP`PageModule`", "PageModule.wl"}, {"JerryI`WSP`WLJS`", "WLJS.wl"}},
        "Symbols" -> {}
      },
 
      {
        "Asset",
        "Assets" -> {
          {"ReadMe", "./README.md"},
          {"Examples", "./public"}
        }
      }
    }
  |>
]
