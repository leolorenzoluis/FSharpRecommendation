(**
## Redux helper

Fable's support for Redux is experimental and there're no bindings available.
In order to use Redux in you Fable apps, just copy the code of the module below
and be sure to add Redux as a JS dependency.
*)
module ReduxManager

open System
open Fable.Import
open Fable.Core
open Fable.Core.JsInterop

type IStore<'TState, 'TAction> = interface end

let [<Import("createStore","redux")>] private createStore' = obj()

let createStore (reducer: 'TState->'TAction->'TState) (initState: 'TState) =
    // Check if the action is a union type before applying the reducer
    let reducer = fun state action ->
        match box action?Case with
        | :? string -> reducer state action
        | _ -> state
    match unbox Browser.window?devToolsExtension with
    | Some ext -> createStore'$(Func<_,_,_> reducer, initState, ext$())
    | None -> createStore'$(Func<_,_,_> reducer, initState)
    |> unbox<IStore<'TState, 'TAction>>

let dispatch (store: IStore<'TState, 'TAction>) (x: 'TAction) =
    let x = toPlainJsObj x
    x?``type`` <- x?Case
    store?dispatch(x) |> ignore

let inline subscribe (store: IStore<'TState, 'TAction>) (f: unit->unit) =
    store?subscribe(f) |> ignore

let inline getState (store: IStore<'TState, 'TAction>) =
    store?getState() |> unbox<'TState>
