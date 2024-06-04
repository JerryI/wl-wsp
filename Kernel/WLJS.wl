BeginPackage["JerryI`WSP`WLJS`"]

WLJSHeader::usage = "place scripts of WLJS libs"
WLJS::usage = "WLJS[expr] embeds any wolfram expression and executes it using WLJS"

Begin["`Private`"]

System`AttachDOM;
System`FrontEndVirtual;

WLJSHeader[list_String] := With[{},
    (StringRiffle[StringTemplate["<script type=\"module\" src=\"``\"></script>"]/@StringTrim/@StringSplit[list, "\n"], "\n"])<>"<script type=\"module\">core.Offload = core.Hold;</script>"
]

WLJS[expr_, OptionsPattern[] ] := With[{uid = CreateUUID[], class = ""},
        With[{body = FrontEndVirtual[{
            AttachDOM[uid],
            CompoundExpression[expr]
        }]},
            StringTemplate["<div class=\"wljs-object ``\" id=\"``\"></div><script type=\"module\">const global = {}; await interpretate(``, {local:{}, global: global})</script>"]["", uid, ExportString[body, "ExpressionJSON", "Compact"->1] ]
        ]

            ] 

End[]

EndPackage[]
