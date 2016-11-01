module Main

open Fable.Core
open Fable.Import
open Fable.Core.JsInterop
open Elmish
open Elmish.Browser.Navigation
open Fable.Import.Browser
open Elmish.UrlParser

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
    editMode : bool
}

type Page = Home | Blog of int | Search of string

type Model = {
    items : Item list
    uid : int
    value : string
    page : Page
}
// Types


let toHash = 
    function
    | Home -> "#home"
    | Blog id -> "#blog/" + (string id)
    | Search query -> "#search/" + query
(* If the URL is valid, we just update our model or issue a command. 
If it is not a valid URL, we modify the URL to whatever makes sense.
*)
let urlUpdate (result:Result<Page,string>) model =
  match result with
  | Error e ->
      Browser.console.error("Error parsing url:", e)  
      ( model, Navigation.modifyUrl (toHash model.page) )

/// The URL is turned into a Result.
let pageParser : Parser<Page->_,_> =
  oneOf
    [ format Home (s "home")
      format Blog (s "blog" </> i32)
      format Search (s "search" </> str) ]

let hashParser (location:Location) =
  UrlParser.parse id pageParser (location.hash.Substring 1)

let traceUrlUpdate (result:Result<Page,string>) m = 
    console.log("UrlUpdate:", result)
    urlUpdate result m
let emptyModel = {
    items = []
    uid = 0
    value = ""
    page = Page.Home
}

let newEntry desc id =
  { description = desc
    id = id 
    editMode = false
    }

let init = function
  | Some savedModel -> savedModel, []
  | _ -> emptyModel, []

type Msg = 
    | Add
    | UpdateField of string
    | Delete of int
    | Edit of int*string

let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =
    match msg with
    | Add ->
        let xs = if System.String.IsNullOrEmpty model.value then
                    model.items
                 else
                    model.items @ [{ description = model.value; id = model.uid;
                    editMode = false }]
        
        { model with
            uid = model.uid + 1
            value = ""
            items = xs }, []
    | UpdateField newValue ->
        { model with value = newValue } , []
    | Delete id ->
        { model with items = List.filter(fun x -> x.id <> id) model.items }, []
    | Edit (id, newValue) ->
        let updateEntry (item : Item) =
            if item.id = id then { item with editMode = not item.editMode; description=newValue } else item
        { model with items = List.map(updateEntry) model.items }, []

open Fable.Helpers.React.Props


let internal classList classes =
    classes 
    |> List.fold (fun complete -> function | (name,true) -> complete + " " + name | _ -> complete) ""
    |> ClassName


let toHash = 
    function
    | Home -> "#home"
    | Blog id -> "#blog/" + (string id)
    | Search query -> "#search/" + query

let viewLink page description =
    R.a 
        [ Href (toHash page) ]
        [ unbox description ]

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
                    classList ["edit-mode", item.editMode; "",not item.editMode]
                ]
                [
                    unbox item.description
                                    ]
            R.input 
                [ 
                    classList ["", item.editMode; "edit-mode",not item.editMode]
                    DefaultValue (U2.Case1 item.description) 
                    OnBlur (fun ev -> Edit (item.id,unbox ev.target?value) |> dispatch)
                ]
                []
            R.button 
                [
                    OnClick (fun _ -> Edit (item.id, item.description) |> dispatch)
                ]
                [ unbox "Edit me" ]
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
            viewLink Home "Home"
            viewLink (Blog 42) "Test 1"
            viewLink (Blog 30) "Test 2"
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
|> Program.toHtml (Program.runWithNavigation hashParser traceUrlUpdate) "main"