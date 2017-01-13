type Tree = Node of (string * float) * Tree list
type Extent = (float * float) list

let moveextent (e, x) = List.map (fun (p,q) -> (p+x,q+x)) e

let movetree ((Node((label,x), subtrees)), x') = Node((label, x+x'), subtrees)     

let rec merge = function
    | ([], qs)               -> qs
    | (ps, [])               -> ps
    | ((p,_)::ps, (_,q)::qs) -> (p,q)::merge (ps, qs)

let mergelist es = List.fold (fun acc elem -> merge (acc,elem)) [] es

let rec fit left right =
    match (left, right) with 
    | ((_,p)::ps, (q,_)::qs) -> max (fit ps qs) (p - q + 1.0)
    | _                        -> 0.0

let fitlistl es =
    let rec aux acc = function
        | [] -> []
        | e::es -> let x = fit acc e
                   x :: aux (merge(acc, moveextent (e,x))) es
    aux [] es

let fitlistr es = 
    let rec aux acc = function
        | [] -> []
        | e::es -> let x = -(fit e acc)
                   x :: aux (merge(moveextent (e,x), acc)) es
    List.rev (aux [] (List.rev es))

let mean (x,y) = (x + y) / 2.0

let fitlist es = List.map mean (List.zip (fitlistl es) (fitlistr es))

let rec design' (Node((label,_), subtrees)) =
    let (trees, extents) = List.unzip (List.map design' subtrees)
    let positions = fitlist extents
    let ptrees = List.map movetree (List.zip trees positions)
    let pextents = List.map moveextent (List.zip extents positions)
    let resultextent = (0.0, 0.0)::mergelist pextents
    let resulttree = Node((label,0.0), ptrees)
    (resulttree, resultextent)
    
let design tree = fst (design' tree)

let rec createSubTree breadth depth =
    match (breadth, depth) with
    | _, 0 -> []
    | 0, _ -> []
    | m, n -> let subtrees = createSubTree m (n - 1)
              Seq.toList (seq { for i in 0 .. (m-1) -> Node((string (i,n), 0.0), subtrees)})   
              
let createTree breadth depth = let subtrees = createSubTree breadth (depth - 1)
                               Node(("root",0.0),subtrees)


let nl = System.Environment.NewLine
let FloatMax = System.Double.MaxValue
let FloatMin = System.Double.MinValue
let NodeToLine = -20.0
let LineToNode = -40.0

let drawNode (x,y) (Node((label,_),subtrees)) =
    string (x,y-10.0) + " moveto" + nl +
    string (label) + " dup stringwidth pop 2 div neg 0 rmoveto show" + nl +
    string (x,y-20.0) + " moveto" + nl,
    (x,y-20.0)
    
let drawHorizontal (x,y) subtrees =
    let rec outliers mini maxi = function
        | [] ->  (mini, maxi)
        | Node((_, x), _)::es -> outliers (min x mini) (max x maxi) es
    let (min,max) = outliers FloatMax FloatMin subtrees    
    string(min,y) + " moveto" + nl + 
    string(max,y) + " lineto" + nl

let drawVerticalFromNode (x,y) =
    string(x,y+NodeToLine) + " lineto" + nl, 
    (x,y+NodeToLine)

let rec drawVerticalFromLine (x,y) = function
    | [] -> ""
    | Node((_,d),_)::es -> string(x+d,y) + " moveto" + nl + 
                           string(x+d,y+LineToNode) + " lineto" + nl +
                           drawVerticalFromLine (x,y) es
                           

let rec drawTree (x,y) (Node((label,d),subtrees)) =
    let (nodeStr, nodePt) = drawNode (x,y) (Node((label,d),subtrees))
    match subtrees with
    | []    -> nodeStr
    | _     -> let (fromNodeStr, fromNodePt) = drawVerticalFromNode nodePt
               let horizontalLine = drawHorizontal fromNodePt subtrees
               let fromLineStr = drawVerticalFromLine (fromNodePt) subtrees
               String.concat (nodeStr + fromNodeStr + horizontalLine + fromLineStr + "stroke" + nl)
                    (List.map (drawTree fromNodePt) subtrees)

let createPS tree =
   "%!" + nl +
    "<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice" + nl +
    "1 1 scale" + nl +
    "700 999 translate" + nl +
    "newpath" + nl +
    "/Times-Roman findfont 10 scalefont setfont" + nl +
    drawTree (0.0, 0.0) tree + 
    "showpage"