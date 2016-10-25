module Main

open Fable.Core
open Fable.Import
open Fable.Core.JsInterop
open Elmish

importDefault("core-js/shim")
importDefault("todomvc-common/base.js")
importDefault("todomvc-common/base.css")
importDefault("todomvc-app-css/index.css")

// Rendering views with React
module R = Fable.Helpers.React

// Local storage interface
module S =
    let private STORAGE_KEY = "elmish-react-todomvc"
    let load<'T> (): 'T option =
        Browser.localStorage.getItem(STORAGE_KEY)
        |> unbox 
        |> Core.Option.map (JS.JSON.parse >> unbox<'T>)

    let save<'T> (model: 'T) =
        Browser.localStorage.setItem(STORAGE_KEY, JS.JSON.stringify model)

type Item = {
    description : string
    id : int
}

type Model = {
    items : Item list
    uid : int
    value : string
}

let emptyModel = {
    items = []
    uid = 0
    value = ""
}

let init = function
  | Some savedModel -> savedModel, []
  | _ -> emptyModel, []

type Msg = 
    | Add
    | UpdateField of string

let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =
    match msg with
    | Add ->
        let xs = if System.String.IsNullOrEmpty model.value then
                    model.items
                 else
                    model.items @ [{ description = model.value; id = model.uid }]
        { model with
            value = ""
            items = xs }, []
    | UpdateField newValue ->
        { model with value = newValue } , []

open Fable.Helpers.React.Props


let viewEntry (dispatch : Msg -> unit) (item : Item) = 
    R.input 
        [ DefaultValue (U2.Case1 item.description) ]
        []

let internal onEnter msg dispatch =
    function
    | (ev:React.KeyboardEvent) when ev.keyCode = 13. ->
        ev.preventDefault()
        dispatch msg
    | _ -> ()
    |> OnKeyDown

let view (model:Model) dispatch =
    R.div
        []
        [ 
            R.input
                [
                    Placeholder "Put something here bitch"
                    Value (U2.Case1 model.value)
                    OnChange ((fun (ev:React.FormEvent) -> ev.target?value) >> unbox >> UpdateField >> dispatch)
                    onEnter Add dispatch
                ]
                []
            R.p 
                []
                [unbox "Hello"]
            R.ul
                []
                (model.items
                |> List.map(viewEntry dispatch))
        ]

open Elmish.React
// App
Program.mkProgram (S.load >> init) update view
|> Program.withConsoleTrace
|> Program.toHtml Program.run "main"