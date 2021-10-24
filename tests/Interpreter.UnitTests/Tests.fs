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
    
let fifthAtm =
    // (1*)&(1|0)
    (Intersect (Star (RSmb '1'), Alt (RSmb '1', RSmb '0'))) |> makeAtm
    
let sixthAtm =
    // (1|0)&(1|2)
    (Intersect (Alt (RSmb '1', RSmb '0'), Alt (RSmb '1', RSmb '2'))) |> makeAtm

let prepareStr (str: string) =
    str.ToCharArray() |> List.ofArray

let testString1 = "12122" |> prepareStr

let testString2 = "21213" |> prepareStr

let testString3 = "ababa" |> prepareStr

let testString4 = "babac" |> prepareStr

let testString5 = "1" |> prepareStr
let testString6 = "12" |> prepareStr


[<Tests>]
let testATM =
    testList "ATMs functions" [
        testCase "accept 12122 by (1, 2)*" <| fun _ ->
            Expect.isTrue (accept fstAtm testString1) ""

        testCase "accept 12122 by (1, 2)* + 3" <| fun _ ->
            Expect.isFalse (accept sndAtm testString1) ""

        testCase "accept 21213 by (1, 2)*" <| fun _ ->
            Expect.isFalse (accept fstAtm testString2) ""

        testCase "accept 21213 by (1, 2)* + 3" <| fun _ ->
            Expect.isTrue (accept sndAtm testString2) ""
                    
        testCase "accept ababa by (a, b)*" <| fun _ ->
            Expect.isTrue (accept thirdAtm testString3) ""

        testCase "accept ababa by (a, b)* + c" <| fun _ ->
            Expect.isFalse (accept fourthAtm testString3) ""

        testCase "accept babac by (a, b)*" <| fun _ ->
            Expect.isFalse (accept thirdAtm testString4) ""

        testCase "accept babac by (a, b)* + c" <| fun _ ->
            Expect.isTrue (accept fourthAtm testString4) ""
        
        testCase "accept 1 by (1*)&(1|0)" <| fun _ ->
            Expect.isTrue (accept fifthAtm testString5) ""
            
        testCase "accept 12 by (1*)&(1|0)" <| fun _ ->
            Expect.isFalse (accept fifthAtm testString6) ""
            
        testCase "accept 1 by (1|0)&(1|2)" <| fun _ ->
            Expect.isTrue (accept sixthAtm testString5) ""
            
        testCase "accept 12 by (1|0)&(1|2)" <| fun _ ->
            Expect.isFalse (accept sixthAtm testString6) ""
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
