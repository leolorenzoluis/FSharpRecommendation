(**
## JavaScript bindings

Fable includes [React bindings and helpers](https://www.npmjs.com/package/fable-import-react)
to make interaction with the tool more idiomatic in F#.
*)

module Components

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Domain

module R = Fable.Helpers.React
open R.Props

(**
## React components

Define React components in the same way as explained in [React guide](https://facebook.github.io/react/docs/reusable-components.html#es6-classes)
but with statically checked `props` and `state` types and the DSL from the [Fable React helper](https://www.npmjs.com/package/fable-import-react).
Check the `render` method of each component and the [Fable React TodoMVC sample](https://fable-compiler.github.io/samples/react-todomvc/index.html#/) for details,
or scroll down to see how to subscribe React components to the Redux store.
*)

type TodoTextInputProps =
    abstract OnSave: string->unit
    abstract Text: string option
    abstract Placeholder: string
    abstract Editing: bool
    abstract NewTodo: bool

type TodoTextInputState = { Text: string }

type TodoTextInput(props, ctx) as this =
    inherit React.Component<TodoTextInputProps, TodoTextInputState>(props, ctx)
    do this.state <- { Text = defaultArg props.Text "" } 

    member this.HandleSubmit(e: React.KeyboardEvent) =
        if e.which = ENTER_KEY then
            let text = (unbox<string> e.target?value).Trim()
            this.props.OnSave(text)
            if this.props.NewTodo then
                this.setState({ Text = "" })

    member this.HandleChange(e: React.SyntheticEvent) =
        this.setState({ Text=unbox e.target?value })

    member this.HandleBlur(e: React.SyntheticEvent) =
        if not this.props.NewTodo then
            this.props.OnSave(unbox e.target?value)

    member this.render() =
        R.input [
            ClassName(
                classNames [
                    "edit", this.props.Editing
                    "new-todo", this.props.NewTodo
                ])
            Type "text"
            OnBlur this.HandleBlur
            OnChange this.HandleChange
            OnKeyDown this.HandleSubmit
            AutoFocus (this.state.Text.Length > 0)
            Placeholder this.props.Placeholder
        ] []

type TodoItemProps =
    abstract Todo: Todo
    abstract EditTodo: int * string -> unit
    abstract DeleteTodo: int -> unit
    abstract CompleteTodo: int -> unit

type TodoItemState = { Editing: bool }

type TodoItem(props, ctx) as this =
    inherit React.Component<TodoItemProps, TodoItemState>(props, ctx)
    do this.state <- { Editing = false } 

    member this.HandleSave(id, text: string) =
        if text.Length = 0
        then this.props.DeleteTodo(id)
        else this.props.EditTodo(id, text)
        this.setState({ Editing = false })

    member this.HandleDoubleClick(_) =
        this.setState({ Editing = true })

    member this.render() =
        let element =
            if this.state.Editing
            then R.com<TodoTextInput,_,_>
                    { new TodoTextInputProps with
                        member __.OnSave(text: string) =
                            this.HandleSave(this.props.Todo.Id, text)
                        member __.Editing = this.state.Editing
                        member __.Text = Some this.props.Todo.Text
                        member __.Placeholder = ""
                        member __.NewTodo = false } []
            else R.div [ClassName "view"] [
                    R.input [
                        ClassName "toggle"
                        Type "checkbox"
                        Checked this.props.Todo.Completed
                        OnChange (fun _ ->
                            this.props.CompleteTodo(this.props.Todo.Id))
                    ] []
                    R.label [
                        OnDoubleClick this.HandleDoubleClick 
                    ] [ R.str this.props.Todo.Text ]
                    R.div [
                        ClassName "destroy"
                        OnClick (fun _ -> this.props.DeleteTodo(this.props.Todo.Id))
                    ] []
                ]
        R.li [ClassName(
                classNames [
                    "completed", this.props.Todo.Completed
                    "editing", this.state.Editing])]
             [element]

type HeaderProps = { AddTodo: string->unit }

// When the React component has no mutable state, we can simply
// use a function (equivalent to the `render` method).

let Header (props: HeaderProps) =
    R.header [ClassName "header"] [
        R.h1 [] [R.str "todos"]
        R.com<TodoTextInput,_,_>
            { new TodoTextInputProps with
                member __.OnSave(text: string) = props.AddTodo text
                member __.Placeholder = "What needs to be done?"
                member __.Text = None
                member __.Editing = false
                member __.NewTodo = true } []
    ]

type FooterProps =
    abstract ActiveCount: int
    abstract CompletedCount: int
    abstract Filter: TodoFilter
    abstract OnShow: TodoFilter->unit
    abstract OnClearCompleted: React.SyntheticEvent->unit

let Footer =
    let filterTitles =
        dict [
            TodoFilter.ShowAll, "All"
            TodoFilter.ShowActive, "Active"
            TodoFilter.ShowCompleted, "Completed"
        ]
    let renderTodoCount activeCount =
        R.span [ClassName "todo-count"] [
            sprintf "%s item%s left"
                (if activeCount > 0 then string activeCount else "No")
                (if activeCount <> 1 then "s" else "")
            |> R.str
        ]
    let renderFilterLink filter selectedFilter onShow =
        R.a [
            ClassName (classNames ["selected", filter = selectedFilter])
            Style [unbox("cursor", "pointer")]
            OnClick (fun _ -> onShow filter)
        ] [R.str filterTitles.[filter]]
    let renderClearButton completedCount onClearCompleted =
        if completedCount > 0
        then R.button [
                ClassName "clear-completed"
                OnClick onClearCompleted
             ] [R.str "Clear completed"] |> Some
        else None
    fun (props: FooterProps) ->
        let listItems =
            [ TodoFilter.ShowAll
              TodoFilter.ShowActive
              TodoFilter.ShowCompleted ]
            |> List.map (fun filter ->
                [renderFilterLink filter props.Filter props.OnShow]
                |> R.li [Key (string filter)])
        R.footer [ClassName "footer"] [
            renderTodoCount props.ActiveCount
            R.ul [ClassName "filters"] listItems
            R.opt(renderClearButton props.CompletedCount props.OnClearCompleted)
        ]

type MainSectionProps = { Todos: Todo[]; Dispatch: TodoAction->unit }
type MainSectionState = { Filter: TodoFilter }

type MainSection(props, ctx) as this =
    inherit React.Component<MainSectionProps, MainSectionState>(props, ctx)
    let todoFilters =
        dict [
            TodoFilter.ShowAll, fun _ -> true
            TodoFilter.ShowActive, fun (todo: Todo) -> not todo.Completed
            TodoFilter.ShowCompleted, fun todo -> todo.Completed
        ]
    do this.state <- { Filter = TodoFilter.ShowAll } 

    member this.HandleClearCompleted() =
        this.props.Dispatch(ClearCompleted)

    member this.HandleShow(filter) =
        this.setState({ Filter = filter })

    member this.renderToggleAll(completedCount) =
        if this.props.Todos.Length > 0
        then R.input [
                ClassName "toggle-all"
                Type "checkbox"
                Checked (completedCount = this.props.Todos.Length)
                OnChange (fun _ -> this.props.Dispatch(CompleteAll))
             ] [] |> Some
        else None

    member this.renderFooter(completedCount) =
        if this.props.Todos.Length > 0
        then R.fn Footer
                { new FooterProps with
                    member __.ActiveCount =
                        this.props.Todos.Length - completedCount
                    member __.CompletedCount = completedCount
                    member __.Filter = this.state.Filter
                    member __.OnShow filter = this.HandleShow filter
                    member __.OnClearCompleted _ =
                        this.HandleClearCompleted() } [] |> Some
        else None

    member this.render() =
        let filteredTodos =
            props.Todos
            |> Array.filter todoFilters.[this.state.Filter]
            |> Array.toList
        let completedCount =
            (0, this.props.Todos) ||> Array.fold (fun count todo ->
                if todo.Completed then count + 1 else count)
        R.section [ClassName "main"] [
            R.opt(this.renderToggleAll completedCount)
            R.ul [ClassName "todo-list"]
                (filteredTodos
                |> List.map (fun todo ->
                    R.com<TodoItem,_,_>
                        { new TodoItemProps with
                            member __.Todo = todo
                            member __.EditTodo(id, text) =
                                this.props.Dispatch(EditTodo(id, text))
                            member __.DeleteTodo(id) =
                                this.props.Dispatch(DeleteTodo id)
                            member __.CompleteTodo(id) =
                                this.props.Dispatch(CompleteTodo id) } []))
            R.opt(this.renderFooter completedCount)
        ]