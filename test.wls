Get["Kernel/WSP.wl"]

SetOptions[WSPEngine, "Cache"->False]

LoadPage["index.wsp", {}, "Base"->"public"] // Print;