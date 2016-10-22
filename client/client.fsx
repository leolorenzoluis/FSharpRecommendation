#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-react/Fable.Import.React.fs"
#load "node_modules/fable-import-react/Fable.Helpers.React.fs"
#load "components.fsx"
#load "domain.fsx"
#load "manager.fsx"
#load "redux.fsx"

open Fable.Import
open Fable.Core.JsInterop
open Components
open Domain
open Manager
open Redux

module R = Fable.Helpers.React

importDefault("core-js/shim")
importDefault("todomvc-common/base.js")
importDefault("todomvc-common/base.css")
importDefault("todomvc-app-css/index.css")

ReactDom.render(
    R.com<App,_,_> { Store=store } [],
    Browser.document.getElementsByClassName("todoapp").[0]
) |> ignore