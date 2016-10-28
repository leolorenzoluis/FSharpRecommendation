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


let newEntry desc id =
  { description = desc
    id = id }

let init = function
  | Some savedModel -> savedModel, []
  | _ -> emptyModel, []

type Msg = 
    | Add
    | UpdateField of string
    | Delete of int

let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =
    match msg with
    | Add ->
        let xs = if System.String.IsNullOrEmpty model.value then
                    model.items
                 else
                    model.items @ [{ description = model.value; id = model.uid }]
        
        { model with
            uid = model.uid + 1
            value = ""
            items = xs }, []
    | UpdateField newValue ->
        { model with value = newValue } , []
    | Delete id ->
        { model with items = List.filter(fun x -> x.id <> id) model.items }, []

open Fable.Helpers.React.Props


let internal onEnter msg dispatch =
    function
    | (ev:React.KeyboardEvent) when ev.keyCode = 13. ->
        ev.preventDefault()
        dispatch msg
    | _ -> ()
    |> OnKeyDown
let viewEntry (dispatch : Msg -> unit) (item : Item) = 
    R.div 
        []
        [   
            R.p
                []
                [ unbox item.id ]
            
            R.label 
                [
                    Style [ BackgroundColor "red" ]
                ]
                [
                    unbox item.description
                    
                ]
            R.input 
                [ 
                    DefaultValue (U2.Case1 item.description) 
                ]
                []
            R.button 
                [
                    OnClick (fun _ -> Delete item.id |> dispatch)
                ]
                [ unbox "Delete me" ]
        ]


let view (model:Model) dispatch =
    R.div
        []
        [ 
            R.input
                [
                    Placeholder "Put something here"
                    Value (U2.Case1 model.value)
                    OnChange ((fun (ev:React.FormEvent) -> ev.target?value) >> unbox >> UpdateField >> dispatch)
                    onEnter Add dispatch
                    AutoFocus true
                ]
                []
            R.p 
                []
                [unbox "Hello"]
            R.div
                []
                (model.items
                |> List.map(viewEntry dispatch))
        ]

open Elmish.React
// App
Program.mkProgram (S.load >> init) update view
|> Program.withConsoleTrace
|> Program.toHtml Program.run "main"