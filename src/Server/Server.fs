module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared
open System

module Array =
    let shuffleInPlace (rand: Random) (array: 'a[]) =
        for n in array.Length - 1 .. -1 .. 1 do
            let k = rand.Next n
            let tmp = array.[n]
            array.[n] <- array.[k]
            array.[k] <- tmp

module List =
    let shuffle (rand: Random) (list: 'a list) =
        let arr = List.toArray list
        Array.shuffleInPlace rand arr
        List.ofArray arr

type GiveTo = string * (string list)

module SantaHatThings =
    let spouses = [("Alan", "Paula"); ("Paul", "Allison"); ("Peter", "Rebecca")]
    let singles = ["Sarah"; "John"]

    let people = [
        for (a,b) in spouses do yield a; yield b
        for a in singles do yield a
    ]

    let pairSantas n (people: string list) =
        let rand = new Random()

        let hat = people |> Seq.replicate n |> Seq.concat |> Seq.toArray

        Seq.initInfinite (fun _ ->
            Array.shuffleInPlace rand hat
            let recipientSet = List.chunkBySize n (Array.toList hat)
            List.zip people recipientSet
        )

    let giveToOthers (giver, recipients) =
        not (List.contains giver recipients)

    let allGiveToOthers (xs: GiveTo list) = xs |> List.forall giveToOthers
    let isMarriedTo spouses person1 person2  =
        spouses |> Seq.exists (fun (him, her) ->
            (person1 = him && person2 = her) || (person2 = him && person1 = her))
    let notMarriedTo spouses (giver, recipients) =
        recipients |> List.forall (fun r -> not (isMarriedTo spouses giver r))
        //not (isMarriedTo spouses giver r1) && not (isMarriedTo spouses giver r2)

    let allGiveToOtherThanSpouse spouses (xs: GiveTo list) =
        xs |> List.forall (notMarriedTo spouses)

    let filteredSantaPairs spouses people =
        pairSantas 2 people
        |> Seq.filter allGiveToOthers
        |> Seq.filter (allGiveToOtherThanSpouse spouses)
        |> Seq.head
        |> Map.ofList

type Storage(people, spouses) =
    let results = SantaHatThings.filteredSantaPairs spouses people

    member __.GetPeople () = people
    member __.TryGetResults (person: string) =
        match Map.tryFind person results with
        | Some results -> Ok results
        | None -> Error "No such person"

let storage = Storage(SantaHatThings.people, SantaHatThings.spouses)

let santaHatApi =
    { getPeople =
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
    |> Remoting.fromValue santaHatApi
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
