namespace Shared

open System

type Todo =
    { Id : Guid
      Description : string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ITodosApi =
    { getTodos : unit -> Async<Todo list>
      addTodo : Todo -> Async<Todo> }


module SantaHatThings =
    open System

    let people = ["Alan"; "Paula"; "Paul"; "Allison"; "Sarah"; "Peter"; "Rebecca"; "John"]

    let pairSantas people =
        let rand = new Random()
        let nPeople = List.length people
        [ for gifter in people ->
            let recipient1 = people.[rand.Next nPeople]
            let recipient2 = people.[rand.Next nPeople]
            gifter, (recipient1, recipient2)
        ]

    let result = pairSantas people



