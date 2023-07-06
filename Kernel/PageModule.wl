BeginPackage["JerryI`WSP`PageModule`", {"JerryI`WSP`", "JerryI`Misc`Channels`", "KirillBelov`HTTPHandler`Extensions`"}];

(*

    WSP Extension for dynamically updatable HTML blocks
    - creates a wrapper around using DIV element
    - stores unevaluated expressions in the memory
    - simulates the natural loading routine with a given fake $CurrentRequest and generates result
*)

PageModule::usage = "Wrap any block of code inside .wsp file via PageModule[..., \"uid\"]"
HotReload::usage = "Get the updated dynamic element with a substituted $CurrentRequest. Use HotReload[\"uid\"]"
(* can be improved a lot.... *)

Begin["`Private`"]; 

hashedObjects = <||>;

ClearAll[PageModule]
SetAttributes[PageModule, HoldFirst]

PageModule[expr_, ihash_:Null] := Module[{hash = ihash}, 
    If[!StringQ[hash],
        hash = "wsp-"<>Hash[Hold[expr], "Expression", "DecimalString"];
    ];

    hashedObjects[hash] = <|"hidden"->False, "expr"->Hold[expr], "request"->Global`$CurrentRequest, "public"->Global`$WSPPublic|>;

    {StringTemplate["<`` data-wsp=\"``\">"]["div", hash], ReleaseHold[expr], "</div>"}
];


HotReload[uids_, s_:<||>] := Module[{result},
    result = Block[{},
        
        DeleteMissing[
            Block[{Global`$CurrentRequest = Join[hashedObjects[#, "request"], s], Global`$WSPPublic = hashedObjects[#, "public"]},
                If[hashedObjects[#,"hidden"],
                    hashedObjects[#, "expr"]//ReleaseHold;
                    Missing[]
                ,
                    {#, (ToString /@ Flatten[{hashedObjects[#, "expr"]//ReleaseHold}]) // StringJoin} 
                ]
            
            ]&/@ uids
        ]
    ];

    WebSocketChannel[Automatic]["Push", Global`PageModulesUpdate[result] ];
];

End[];

EndPackage[];