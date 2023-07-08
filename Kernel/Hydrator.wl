Plant::usage = "Execute code on the server and embedd the result on a page (plant a seed)"
Grow::usage = "Execute code on a client immediantely"


Plants = <||>

Plant[expr_] := With[{id = CreateUUID[]},
    Plants[id] = Global`FrontEndVirtual[{Global`AttachDOM[id], expr}];
    $CurrentRequest["Seeds"][id] = True;
    "<div id=\""<>id<>"\"></div>"
]

Grow[expr_] := With[{id = CreateUUID[]},
    $CurrentRequest["Turnips"][id] = ExportString[Hold[expr], "ExpressionJSON", "Compact"->0];
    ""
]

SetAttributes[Grow, HoldFirst]

Hydrate[id_String] := With[{cli = Global`client},
    Print[StringTemplate["`` got hydrated via ``"][id, cli]];
    WebSocketSend[cli, ExportByteArray[Plants[id], "ExpressionJSON"]];
    Plants[id] = .;
]
