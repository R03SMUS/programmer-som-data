module RunProgram

open ParseAndRun

// Run dotnet fsi -r ~/fsharp/FsLexYacc.Runtime.dll Util.fs Absyn.fs FunPar.fs FunLex.fs Parse.fs Fun.fs ParseAndRun.fs 4_1_AND_4_2.fs
// Then scroll almost all the way to the top

// 4.1
let runPrograms programs =
    for program in programs do
        let result = run (fromString program)
        printfn "Program: '%s' resulted in: %A" program result

let programs4_1 = [
    "let x = 1 in let y = 42 in x*y + 2 end end"
    "1 + 2 + 3 + 4 + 6"
]
printfn "______________ 4.1 ______________"
runPrograms programs4_1

// 4.2
let programs4_2 = [
    "let f n = if n=0 then 0 else n + f (n-1) in f 1000 end"
    "let f n = if n=0 then 1 else 3 * f (n-1) in f 11 end"
    "let f n = if n=0 then 1 else 3 * f (n-1) in let g n = if n=0 then 1 else f n + g (n-1) in g 11 end end"
    "let g n = if n=0 then 0 else let i = n in let f k = if k=0 then 1 else i * f (k-1) in f 8 + g (n-1) end end in g 10 end"
]
printfn "______________ 4.2 ______________"
runPrograms programs4_2