module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model =
    { MyName: string option
      People: string list
      Results: string list }

type Msg =
    | Identify of string
    | GotPeople of string list
    | GotResults of string list

let santaHatApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ISantaHatApi>

let init(): Model * Cmd<Msg> =
    let model =
        { MyName = None
          People = []
          Results = [] }
    let cmd = Cmd.OfAsync.perform santaHatApi.getPeople () GotPeople
    model, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Identify myName ->
        { model with MyName = Some myName }, Cmd.OfAsync.perform santaHatApi.getResults myName GotResults
    | GotPeople people ->
        { model with People = people }, Cmd.none
    | GotResults rs ->
        { model with Results = rs }, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

let viewNavBrand =
    Navbar.Brand.div [ ] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/favicon.png"
                Alt "Logo"
            ]
        ]
    ]

let viewContainerBox (model : Model) (dispatch : Msg -> unit) =
    Box.box' [ ] [
        Content.content [ ] [
            match model.MyName with
            | Some me ->
                p [ ] [ str (sprintf "Welcome, %s! You are secret santa to:" me) ]
                Content.Ol.ol [ ] [
                    for recipient in model.Results ->
                        li [ ] [ str recipient ]
                ]
            | None ->
                Control.div [ ] [
                    Control.p [ ] [ str "Who are you?" ]
                    Control.div [ ] [
                        for person in model.People do
                            Button.a [
                                Button.OnClick (fun _ -> dispatch (Identify person))
                            ] [
                                str person
                            ]
                    ]
                ]
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    Hero.hero [
        Hero.Color IsPrimary
        Hero.IsFullHeight
        Hero.Props [
            Style [
                Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
                BackgroundSize "cover"
            ]
        ]
    ] [
        Hero.head [ ] [
            Navbar.navbar [ ] [
                Container.container [ ] [ viewNavBrand ]
            ]
        ]

        Hero.body [ ] [
            Container.container [ ] [
                Column.column [
                    Column.Width (Screen.All, Column.Is6)
                    Column.Offset (Screen.All, Column.Is3)
                ] [
                    Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "SantaHat" ]
                    viewContainerBox model dispatch
                ]
            ]
        ]
    ]
