
#r @"C:\Users\Helge\git\GuardedCommands\GuardedCommands\bin\Debug\GuardedCommands.dll";

#load "Construction.fs"
#load "Drawing.fs"
#load "Conversion.fs"

open TreeDrawing.Construction
open TreeDrawing.Drawing
open TreeDrawing.Conversion

System.IO.Directory.SetCurrentDirectory @"C:\Users\Helge\git\GuardedCommands\GuardedCommands\";;

writeToFile (design (convertProgram "Ex1.gc"));;

let (Node(_,subtrees)) = design (convertProgram "Ex1.gc");;

drawHorizontal (0.0,0.0) subtrees;;