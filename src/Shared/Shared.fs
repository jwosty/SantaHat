namespace Shared

open System

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ISantaHatApi =
    { getPeople : unit -> Async<string list>
      getResults : string -> Async<string*string> }
