namespace TreeDrawing

module Drawing =

    open TreeDrawing.Construction
    open System.IO

    let nl = System.Environment.NewLine
    let FloatMax = System.Double.MaxValue
    let FloatMin = System.Double.MinValue
    let NodeToLine = -20.0
    let LineToNode = -40.0
    
    let roundPoint = function
    | n when n > 0.0 && n <  1.0 ->  1
    | n when n < 0.0 && n > -1.0 -> -1
    | n                          -> int (round n)

    let pointToString (x,y) = String.concat " " [string (roundPoint x); string (roundPoint y)]
    
    let drawNode (x,y) (labels,d) = 
        let rec aux (x,y) (labels,d) nodeCount = 
            match labels with
            | []        -> pointToString (x+d,y-10.0) + " moveto" + nl, (x+d,y-10.0), nodeCount-1
            | label::ls -> let (str, newPt, newCount) = aux (x,y-10.0) (ls,d) (nodeCount+1)
                           pointToString (x+d,y-10.0) + " moveto" + nl +
                           "(" + string label + ") dup stringwidth pop 2 div neg 0 rmoveto show" + nl +
                           str, newPt, newCount
        aux (x,y) (labels,d) 0
        
    let drawHorizontal (x,y) subtrees =
        let rec outliers mini maxi = function
            | [] ->  (mini, maxi)
            | Node(_, x, _)::es -> outliers (min x mini) (max x maxi) es
        let (min,max) = outliers FloatMax FloatMin subtrees
        pointToString(x+min,y) + " moveto" + nl + 
        pointToString(x+max,y) + " lineto" + nl
    
    let drawVerticalFromNode (x,y) nodeCount =
        pointToString(x,y+NodeToLine - 10.0 * (float nodeCount)) + " lineto" + nl, 
        (x,y+NodeToLine - 10.0 * (float nodeCount))
    
    let rec drawVerticalFromLine (x,y) = function
        | [] -> ""
        | Node(_,d,_)::es -> pointToString(x+d,y) + " moveto" + nl + 
                               pointToString(x+d,y+LineToNode) + " lineto" + nl +
                               drawVerticalFromLine (x,y) es
                               
    
    let rec drawTree (x,y) (Node(labels,d,subtrees)) =
        let (nodeStr, nodePt, nodeCount) = drawNode (x,y) (labels,d)
        match subtrees with
        | []    -> nodeStr
        | _     -> let (fromNodeStr, fromNodePt) = drawVerticalFromNode nodePt nodeCount
                   let horizontalLine = drawHorizontal fromNodePt subtrees
                   let fromLineStr = drawVerticalFromLine (fromNodePt) subtrees
                   let fromLinePt = fst fromNodePt, snd fromNodePt + LineToNode
                   String.concat "" ([nodeStr; fromNodeStr; horizontalLine; fromLineStr; "stroke"; nl]
                        @ List.map (drawTree fromLinePt) subtrees )
    
    let createPS tree =
       "%!" + nl +
        "<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice" + nl +
        "1 1 scale" + nl +
        "700 500 translate" + nl +
        "newpath" + nl +
        "/Times-Roman findfont 10 scalefont setfont" + nl +
        drawTree (0.0, 0.0) tree + 
        "showpage"

    let writeToFile tree = 
        let path = @"C:\Users\Helge\git\TreeDrawing\PostScript.ps"
        let contents = createPS tree
        File.WriteAllText(path, contents)
