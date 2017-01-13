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



