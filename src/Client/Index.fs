module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model =
    { Todos: Todo list
      Input: string
      MyName: string option
      People: string list
      Results: string*string }

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo
    | Identify of string
    | GotPeople of string list
    | GotResults of string * string

let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init(): Model * Cmd<Msg> =
    let model =
        { Todos = []
          Input = ""
          MyName = None
          People = []
          Results = "","" }
    let cmd = Cmd.OfAsync.perform todosApi.getPeople () GotPeople
    model, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | GotTodos todos ->
        { model with Todos = todos }, Cmd.none
    | SetInput value ->
        { model with Input = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Input
        let cmd = Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo
        { model with Input = "" }, cmd
    | AddedTodo todo ->
        { model with Todos = model.Todos @ [ todo ] }, Cmd.none
    | Identify myName ->
        { model with MyName = Some myName }, Cmd.OfAsync.perform todosApi.getResults myName GotResults
    | GotPeople people ->
        { model with People = people }, Cmd.none
    | GotResults (r1, r2) ->
        { model with Results = r1,r2 }, Cmd.none

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
                let a, b = model.Results
                Content.Ol.ol [ ] [
                    for recipient in [a;b] ->
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
        //Field.div [ Field.IsGrouped ] [
        //    Control.p [ Control.IsExpanded ] [
        //        Input.text [
        //          Input.Value model.Input
        //          Input.Placeholder "What needs to be done?"
        //          Input.OnChange (fun x -> SetInput x.Value |> dispatch) ]
        //    ]
        //    Control.p [ ] [
        //        Button.a [
        //            Button.Color IsPrimary
        //            Button.Disabled (Todo.isValid model.Input |> not)
        //            Button.OnClick (fun _ -> dispatch AddTodo)
        //        ] [
        //            str "Add"
        //        ]
        //    ]
        //]
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
