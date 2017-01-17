
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

let tree1 = createTree 3 3;; //      27 = Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0:   0, gen1:  0, gen2: 0
let tree2 = createTree 4 4;; //     256 = Real: 00:00:00.001, CPU: 00:00:00.000, GC gen0:   0, gen1:  0, gen2: 0
let tree3 = createTree 5 5;; //    3125 = Real: 00:00:00.005, CPU: 00:00:00.000, GC gen0:   0, gen1:  0, gen2: 0
let tree4 = createTree 6 6;; //   46656 = Real: 00:00:00.060, CPU: 00:00:00.062, GC gen0:   8, gen1:  0, gen2: 0
let tree5 = createTree 7 7;; //  823543 = Real: 00:00:00.974, CPU: 00:00:00.967, GC gen0: 189, gen1:  5, gen2: 0
let tree6 = createTree 8 7;; // 2097152 = Real: 00:00:02.879, CPU: 00:00:02.870, GC gen0: 324, gen1: 75, gen2: 1

#time "on"
exec "Ex1";;
exec "A1";;
exec "par2";;
exec "QuickSortV1";;
exec "QuickSortV2";;

writeToFile (path + "test1.ps") (design tree1);;
writeToFile (path + "test2.ps") (design tree2);;
writeToFile (path + "test3.ps") (design tree3);;
writeToFile (path + "test4.ps") (design tree4);;
writeToFile (path + "test5.ps") (design tree5);;
writeToFile (path + "test6.ps") (design tree6);;







