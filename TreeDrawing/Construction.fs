namespace TreeDrawing

module Construction =
    type Tree   = Node of string list * float * Tree list
    type Extent = (float * float) list
    
    // Moves an Extent horizontally
    // moveextent: Extent -> float -> Extent
    let moveextent e x = List.map (fun (p,q) -> (p+x,q+x)) e
    
    // Moves a Tree horizontally
    // movetree: Tree -> float -> Tree
    let movetree (Node(label,x, subtrees)) x' = Node(label, x+x', subtrees)     
    
    // Merges two Extents
    // merge: Extent -> Extent -> Extent
    let rec merge left right =
        match (left, right) with
        | ([], qs)               -> qs
        | (ps, [])               -> ps
        | ((p,_)::ps, (_,q)::qs) -> (p,q)::merge ps qs
    
    // Merges a list of Extents
    // mergelst: Extent list -> Extent
    let mergelist es = List.fold (fun acc elem -> merge acc elem) [] es
    
    // Returns a minimum distance between two Extents
    // fit: Extent -> Extent -> float
    let rec fit left right =
        match (left, right) with 
        | ((_,p)::ps, (q,_)::qs) -> max (fit ps qs) (p - q + 40.0)
        | _                      -> 0.0
    
    // Returns a left-biased fitting from a list of Extents
    // fitlistl: Extent list -> float list
    let fitlistl es =
        let rec aux acc = function
            | [] -> []
            | e::es -> let x = fit acc e
                       x::aux (merge acc (moveextent e x)) es
        aux [] es
    
    // Returns a right-biased fitting from a list of Extents
    // fitlistr: Extent list -> float list
    let fitlistr es = 
        let rec aux acc = function
            | [] -> []
            | e::es -> let x = -(fit e acc)
                       x::aux (merge (moveextent e x) acc) es
        List.rev (aux [] (List.rev es))
    
    // Returns the mean
    // mean: float -> float -> float
    let mean x y = (x + y) / 2.0
    
    // Returns a unbiased fitting from a list of Extents
    // fitlist: Extent list -> float list
    let fitlist es = List.map2 mean (fitlistl es) (fitlistr es)
    
    // Uses a bottom-up approach to calculate a resulting tree and extent
    // from its subtrees
    // design': Tree -> Tree * Extent
    let rec design' (Node(label, _, subtrees)) =
        let (trees, extents) = List.unzip (List.map design' subtrees)
        let positions = fitlist extents
        let ptrees = List.map2 movetree trees positions
        let pextents = List.map2 moveextent extents positions
        let resultextent = (0.0, 0.0)::mergelist pextents
        let resulttree = Node(label, 0.0, ptrees)
        (resulttree, resultextent)
        
    // Returns the resulting tree of the auxiliary function design'
    // design: Tree -> Tree
    let design tree = fst (design' tree)
    
    // Creates a subtree with the specified breadth and depth
    // createSubTree: int -> int -> Tree list
    let rec createSubTree breadth depth =
        match (breadth, depth) with
        | _, 0 -> []
        | 0, _ -> []
        | m, n -> let subtrees = createSubTree m (n - 1)
                  [ for i in 0 .. (m-1) -> Node([string (i,n)], 0.0, subtrees) ]   
                  
    // Creates a tree with the specified breadth and depth
    // createTree: int -> int -> Tree
    let createTree breadth depth = let subtrees = createSubTree breadth (depth - 1)
                                   Node(["root"], 0.0,subtrees)
