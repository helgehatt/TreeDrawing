
#r @"C:\Users\Helge\git\GuardedCommands\GuardedCommands\bin\Debug\GuardedCommands.dll";

#load "Construction.fs"
#load "Drawing.fs"
#load "Conversion.fs"

open TreeDrawing.Construction
open TreeDrawing.Drawing
open TreeDrawing.Conversion

System.IO.Directory.SetCurrentDirectory @"C:\Users\Helge\git\GuardedCommands\GuardedCommands\";;

let path = @"C:\Users\Helge\git\TreeDrawing\"

let exec prog = writeToFile (path + prog + ".ps") (design (convertProgram (prog + ".gc")));;

#time "on"
exec "Ex1";;
exec "A1";;
exec "par2";;
exec "QuickSortV2";;
exec "QuickSortV1";;