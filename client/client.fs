module Main

open Fable.Import
open Fable.Core.JsInterop
open Components
open Domain
open ReduxManager
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