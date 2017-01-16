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
    
    let drawNode (x,y) (Node((label,d),subtrees)) =
        pointToString (x+d,y-10.0) + " moveto" + nl +
        "(" + string label + ") dup stringwidth pop 2 div neg 0 rmoveto show" + nl +
        pointToString (x+d,y-20.0) + " moveto" + nl,
        (x+d,y-20.0)
        
    let drawHorizontal (x,y) subtrees =
        let rec outliers mini maxi = function
            | [] ->  (mini, maxi)
            | Node((_, x), _)::es -> outliers (min x mini) (max x maxi) es
        let (min,max) = outliers FloatMax FloatMin subtrees
        pointToString(x+min,y) + " moveto" + nl + 
        pointToString(x+max,y) + " lineto" + nl
    
    let drawVerticalFromNode (x,y) =
        pointToString(x,y+NodeToLine) + " lineto" + nl, 
        (x,y+NodeToLine)
    
    let rec drawVerticalFromLine (x,y) = function
        | [] -> ""
        | Node((_,d),_)::es -> pointToString(x+d,y) + " moveto" + nl + 
                               pointToString(x+d,y+LineToNode) + " lineto" + nl +
                               drawVerticalFromLine (x,y) es
                               
    
    let rec drawTree (x,y) (Node((label,d),subtrees)) =
        let (nodeStr, nodePt) = drawNode (x,y) (Node((label,d),subtrees))
        match subtrees with
        | []    -> nodeStr
        | _     -> let (fromNodeStr, fromNodePt) = drawVerticalFromNode nodePt
                   let horizontalLine = drawHorizontal fromNodePt subtrees
                   let fromLineStr = drawVerticalFromLine (fromNodePt) subtrees
                   let fromLinePt = fst fromNodePt, snd fromNodePt + LineToNode
                   String.concat "" ([nodeStr; fromNodeStr; horizontalLine; fromLineStr; "stroke"; nl]
                        @ List.map (drawTree fromLinePt) subtrees )
    
    let createPS tree =
       "%!" + nl +
        "<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice" + nl +
        "1 1 scale" + nl +
        "1399 999 translate" + nl +
        "newpath" + nl +
        "/Times-Roman findfont 10 scalefont setfont" + nl +
        drawTree (0.0, 0.0) tree + 
        "showpage"

    let writeToFile tree = 
        let path = @"C:\Users\Helge\git\TreeDrawing\PostScript.ps"
        let contents = createPS tree
        File.WriteAllText(path, contents)
