namespace TreeDrawing

module Conversion =

    open TreeDrawing.Construction

    open GuardedCommands.Util.ParserUtil
    open GuardedCommands.Frontend.AST

    let node      s ts = Node([s],0.0,ts)
    let nodeList es ts = Node(es,0.0,ts)

    let typ = function
        | ITyp   -> "ITyp"
        | BTyp   -> "BTyp"
        | ATyp _ -> "ATyp"
        | PTyp _ -> "PTyp"
        | FTyp _ -> "FTyp"

    /// CE e gives the code for an expression e on the basis of a variable and a function environment
    let rec CE = 
        function
        | N n                 -> node ("Int " + string n) []
        | B b                 -> node ("Bool " + string b) []
        | Access acc          -> node "Access" [CA acc]
        | Addr acc            -> node "Addr" [CA acc]
        | Apply("-", [e])     -> node "Apply" [node "-" []; CE e]
        | Apply("!", [e])     -> node "Apply" [node "!" []; CE e]
        | Apply("&&",[b1;b2]) -> node "Apply" [CE b1; node "&&" []; CE b2]
        | Apply(o,[e1;e2]) when List.exists (fun x -> o=x) ["+"; "-"; "*"; "="; "<>"; "<"; "<="; ">"; ">="]
                              -> node "Apply" [CE e1; node o []; CE e2] 
        | Apply(f,es)         -> node "Apply" (node f []::CEs es)

    and CEs es = List.map CE es        

/// CA acc gives the code for an access acc on the basis of a variable and a function environment
    and CA = function 
        | AVar x        -> node "AVar" [node ("Var " + x) []]
        | AIndex(acc,e) -> node "AIndex" [CA acc; CE e]
        | ADeref e      -> node "ADeref" [CE e]
                      
/// CS s gives the code for a statement s on the basis of a variable and a function environment                          
    let rec CS = function
        | PrintLn e        -> node "PrintLn" [CE e]
        | Ass(acc,e)       -> node "Ass" [CA acc; CE e]
        | Block(decs,stms) -> node "Block" (CDs decs @ CSs stms)
        | Alt(GC list)     -> node "Alt" (CGs list)
        | Do(GC list)      -> node "Do" (CGs list)
        | Return e         -> node "Return" [CE e]
        | Call(f,es)       -> node "Call" (node f [] :: CEs es)

    and CSs stms = List.map CS stms 

    and CD = function
        | VarDec(t,s)                -> node "VarDec" [node s []; node (typ t) []]
        | FunDec(topt, s, decs, stm) -> node "FunDec" (node s [] :: 
                                            match topt with
                                            | Some t -> node (typ t) [] :: CDs decs @ [CS stm]
                                            | None   ->                    CDs decs @ [CS stm])

    and CDs decs = List.map CD decs

    and CGs = function
        | []            -> []
        | (e,stms)::gcs -> nodeList ["Guarded"; "Command"] (CE e::CSs stms) :: CGs gcs

    let CP (P(decs,stms)) = node "Program" (CDs decs @ CSs stms)

    let convertProgram filename = CP (parseFromFile filename)