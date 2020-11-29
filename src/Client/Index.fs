module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model =
    { Todos: Todo list
      Input: string
      MyName: string option
      People: string list
      Results: list<string*(string*string)> }

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo
    | Identify of string

let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init(): Model * Cmd<Msg> =
    let model =
        { Todos = []
          Input = ""
          MyName = None
          People = SantaHatThings.people
          Results = SantaHatThings.result }
    let cmd = Cmd.OfAsync.perform todosApi.getTodos () GotTodos
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
        { model with MyName = Some myName }, Cmd.none

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
                let a, b = model.Results |> Map.ofList |> Map.find me
                Content.Ol.ol [ ] [
                    for recipient in [a;b] ->
                        li [ ] [ str recipient ]
                ]
            | None ->
                Control.div [ ] [
                    str "I am:"
                    Input.input [
                        Input.Value model.Input
                        Input.OnChange (fun x -> SetInput x.Value |> dispatch)
                    ]
                    Button.a [
                        Button.OnClick (fun _ -> dispatch (Identify model.Input))
                    ] [
                        str "OK"
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
