module Tests
open System.Collections.Generic
open Interpreter.Symbols
open Interpreter.FiniteAutomata
open Interpreter.Interpreter
open Expecto

let makeAtm regexp =
    regexp
    |> regexToNFA
    |> epsClosure

let fstAtm =
    // (1|2)*
    (Star (Alt (RSmb '1', RSmb '2'))) |> makeAtm

let sndAtm =
    // (1|2)*3
    (Seq (Star (Alt (RSmb '1', RSmb '2')), RSmb '3')) |> makeAtm

let thirdAtm =
    // (a|b)*
    (Star (Alt (RSmb 'a', RSmb 'b'))) |> makeAtm

let fourthAtm =
    // (a|b)*c
    (Seq (Star (Alt (RSmb 'a', RSmb 'b')), RSmb 'c')) |> makeAtm   

let prepareStr (str: string) =
    str.ToCharArray() |> List.ofArray

let testString1 = "12122" |> prepareStr

let testString2 = "21213" |> prepareStr

let testString3 = "ababa" |> prepareStr

let testString4 = "babac" |> prepareStr


[<Tests>]
let testATM =
    testList "ATMs functions" [
        testCase "accept 12122 by (1, 2)*" <| fun _ ->
            Expect.equal (accept fstAtm testString1) true ""

        testCase "accept 12122 by (1, 2)* + 3" <| fun _ ->
            Expect.equal (accept sndAtm testString1) false ""

        testCase "accept 21213 by (1, 2)*" <| fun _ ->
            Expect.equal (accept fstAtm testString2) false ""

        testCase "accept 21213 by (1, 2)* + 3" <| fun _ ->
            Expect.equal (accept sndAtm testString2) true ""
                    
        testCase "accept ababa by (a, b)*" <| fun _ ->
            Expect.equal (accept thirdAtm testString3) true ""

        testCase "accept ababa by (a, b)* + c" <| fun _ ->
            Expect.equal (accept fourthAtm testString3) false ""

        testCase "accept babac by (a, b)*" <| fun _ ->
            Expect.equal (accept thirdAtm testString4) false ""

        testCase "accept babac by (a, b)* + c" <| fun _ ->
            Expect.equal (accept fourthAtm testString4) true ""
    ]

let calculate (ast: AST.Stmt list) =
    match ast.[0] with
    | AST.VDecl (_, e) -> processExpression (Dictionary<_,_>()) e
    | _ -> failwith "unexpected statement"

[<Tests>]
let testInterpreter =
    testList "Interpreter tests" [
        testCase "True isAcceptable test" <| fun _ ->
            let x = "let [x] = isAcceptable \"12121\" (1|2)*"
            Expect.equal "True" (calculate (parseProgramToAST x)).ToString ""
        
        testCase "False isAcceptable test" <| fun _ ->
            let x = "let [x] = isAcceptable \"12521\" (1|2)*"
            Expect.equal "False" (calculate (parseProgramToAST x)).ToString ""
            
        testCase "First findAll test" <| fun _ ->
            let x = "let [x] = findAll \"yyx\" x|y"
            Expect.equal "[(0, 1); (1, 2); (2, 3)]" (calculate (parseProgramToAST x)).ToString ""
            
        testCase "Second findAll test" <| fun _ ->
            let x = "let [x] = findAll \"byx\" a|y"
            Expect.equal "[(1, 2)]" (calculate (parseProgramToAST x)).ToString ""
            
        testCase "Empty findAll test" <| fun _ ->
            let x = "let [x] = findAll \"abb\" x|y"
            Expect.equal "[]" (calculate (parseProgramToAST x)).ToString ""
    ]
