namespace Tests

open NUnit.Framework
open Parser
open LispTypes

[<TestClass>]
type TestClass () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.Test 1 =
        let exp = "(+ 1 1)"
        let res = LispList [LispPrimitive Add; LispNumber 1; LispNumber 1]
        Assert.AreEqual (parser exp, res)
