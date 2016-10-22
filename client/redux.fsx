#load "node_modules/fable-import-react/Fable.Import.React.fs"
#load "components.fsx"
#load "./domain.fsx"
#load "./manager.fsx"


open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

module R = Fable.Helpers.React
open R.Props
open Fable.Import

open Domain
open Manager
open Components

(**
## Subscription to Redux store

The component on top of our UI hierarchy subscribes to the Redux store
and will automatically invalidate children rendering as needed. React
checks when DOM updates are actually necessary, so we don't need to worry
about the performance hit of too frequent updates.
*)

type AppProps = { Store: IStore<Todo[], TodoAction> }

type App(props, ctx) as this =
    inherit React.Component<AppProps, MainSectionProps>(props, ctx)
    let dispatch = dispatch props.Store
    let b = { Todos = getState props.Store; Dispatch=dispatch }
    let getState() = b
    do this.state <- getState()
    do subscribe props.Store (getState >> this.setState)

    member this.render() =
        R.div [] [
            R.fn Header { AddTodo = AddTodo >> dispatch } []
            R.com<MainSection,_,_> this.state []
        ]

(**
## Reducer

The reducer is a single function (which can be composed of other smaller function)
with the responsibility of updating the state in reaction to the actions
dispatched to the Redux store. F# union types and pattern matching makes
it really easy to identify and extract the data from the received actions
in a type-safe manner. The compiler will even warn us if we forget to handle
any of the possible `TodoAction` cases.
*)

// Implement the missing operations
let reducer (state: Todo[]) = function
    | AddTodo text ->
        let id =
            (-1, state)
            ||> Array.fold(fun id todo -> max id todo.Id)
            |> (+) 1
        state
        |> Array.append [|{Id=id; Completed=false; Text=text}|]
    | DeleteTodo id ->
        state
        |> Array.filter(fun todo -> todo.Id <> id) 
    | EditTodo(id, text) ->
        state
        |> Array.map(fun todo ->
            if todo.Id = id
            then { todo with Text=text }
            else todo)
    | CompleteTodo id ->
        state
        |> Array.map(fun todo ->
            if todo.Id = id
            then { todo with Completed=not todo.Completed }
            else todo)
    | CompleteAll ->
        let areAllMarked =
            state |> Array.forall(fun todo -> todo.Completed)
        state
        |> Array.map(fun todo -> { todo with Completed=not areAllMarked})
    | ClearCompleted ->
        state
        |> Array.filter(fun todo -> not todo.Completed)

let store =
    { Text="Use Fable + React + Redux"; Completed=false; Id=0}
    |> Array.singleton
    |> createStore reducer
