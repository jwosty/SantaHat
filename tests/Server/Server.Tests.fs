module Server.Tests

open Expecto

open Shared
open Server

let server = testList "Server" [
    testCase "hello" <| fun _ ->
        Expect.equal 42 42 "result should be 42"

    // note:  cannot run one test in SAFE expect-o test from IDE..
    testCase "shuffle" <| fun _ ->
        let rand = System.Random()
        let hat = [|"foo";"bar"|]
        let hat' = hat |> Array.map id
        Array.shuffleInPlace rand hat    //! mutates?
        Expect.notEqual hat hat' "should be different order"

    //testCase "Adding valid Todo" <| fun _ ->
    //    let storage = Storage()
    //    let validTodo = Todo.create "TODO"
    //    let expectedResult = Ok ()

    //    let result = storage.AddTodo validTodo

    //    Expect.equal result expectedResult "Result should be ok"
    //    Expect.contains (storage.GetTodos()) validTodo "Storage should contain new todo"
]

let all =
    testList "All"
        [
            Shared.Tests.shared
            server
        ]

[<EntryPoint>]
let main _ = runTests defaultConfig all