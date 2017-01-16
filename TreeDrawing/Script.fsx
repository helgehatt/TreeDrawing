
#r @"C:\Users\Helge\git\GuardedCommands\GuardedCommands\bin\Debug\GuardedCommands.dll";

#load "Construction.fs"
#load "Drawing.fs"
#load "Conversion.fs"

open TreeDrawing.Construction
open TreeDrawing.Drawing
open TreeDrawing.Conversion

System.IO.Directory.SetCurrentDirectory @"C:\Users\Helge\git\GuardedCommands\GuardedCommands\";;

let path = @"C:\Users\Helge\git\TreeDrawing\PostScript.ps"

writeToFile path (design (convertProgram "QuickSortV2.gc"));;