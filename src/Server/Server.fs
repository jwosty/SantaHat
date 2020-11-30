module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared
open System

type GiveTo = string * (string * string)

module SantaHatThings =
    let people = ["Alan"; "Paula"; "Paul"; "Allison"; "Sarah"; "Peter"; "Rebecca"; "John"]

    let pairSantas people =
        let rand = new Random()
        let nPeople = List.length people
        Seq.initInfinite (fun _ ->
            [ for gifter in people ->
                let recipient1 = people.[rand.Next nPeople]
                let recipient2 = people.[rand.Next nPeople]
                gifter, (recipient1, recipient2)
            ]
        )

    let giveToOthers ((giver, (recipient1, recipient2))) =
        recipient1 <> giver && recipient2 <> giver

    let allGiveToOthers (xs: GiveTo list) = xs |> List.forall giveToOthers

type Storage (people: string list) =
    let todos = ResizeArray<_>()

    let results =
        SantaHatThings.pairSantas people |> Seq.filter SantaHatThings.allGiveToOthers |> Seq.head
        |> Map.ofList
    
    member __.GetTodos () =
        List.ofSeq todos

    member __.AddTodo (todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok ()
        else Error "Invalid todo"

    member __.GetPeople () = people
    member __.TryGetResults (person: string) =
        match Map.tryFind person results with
        | Some results -> Ok results
        | None -> Error "No such person"

let storage = Storage(SantaHatThings.people)

storage.AddTodo(Todo.create "Create new SAFE project") |> ignore
storage.AddTodo(Todo.create "Write your app") |> ignore
storage.AddTodo(Todo.create "Ship it !!!") |> ignore

let todosApi =
    { getTodos = fun () -> async { return storage.GetTodos() }
      addTodo =
        fun todo -> async {
            match storage.AddTodo todo with
            | Ok () -> return todo
            | Error e -> return failwith e
        }
      getPeople =
        fun () -> async {
            return storage.GetPeople ()
        }
      getResults =
        fun person -> async {
            match storage.TryGetResults person with
            | Ok results -> return results
            | Error e -> return failwith e
        } }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue todosApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
