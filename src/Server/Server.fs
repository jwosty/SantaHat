module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared
open System

type GiveTo = string * (string * string)

module SantaHatThings =
    let people = ["Alan"; "Paula"; "Paul"; "Allison"; "Sarah"; "Peter"; "Rebecca"; "John"]
    let spouses = ["Alan", "Paula"; "Paul", "Allison"; "Peter", "Rebecca"]
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
    let isMarriedTo spouses person1 person2  =
        spouses |> Seq.exists (fun (him, her) ->
            (person1 = him && person2 = her) || (person2 = him && person1 = her))
    let notMarriedTo spouses (giver,(r1,r2))  = not (isMarriedTo spouses giver r1) && not (isMarriedTo spouses giver r2)

    let allGiveToOtherThanSpouse spouses (xs: GiveTo list) =
        xs |> List.forall (notMarriedTo spouses)

    let filteredSantaPairs spouses people =
        pairSantas people
        |> Seq.filter allGiveToOthers
        |> Seq.filter (allGiveToOtherThanSpouse spouses)
        |> Seq.head
        |> Map.ofList

    //isMarriedTo "Paula" "Alan" spouses = true
    //isMarriedTo "Alan" "Paula" spouses = true
    //isMarriedTo "Alan" "Rebecca" spouses = false
    //isMarriedTo "Paula" "Paula" spouses = false

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
