open Expecto
open Quadtrees.UnitTests.MainTests

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args tests
