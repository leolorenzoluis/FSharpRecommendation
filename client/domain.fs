
(**
## Domain models

Open the necessary namespaces, define our domain models (`Todo`, `TodoAction`, `TodoFilter`)
and some utilities (key codes, `classNames`).
*)
module Domain

type Todo = { Text: string; Completed: bool; Id: int } 

type TodoAction =
    | AddTodo of text:string
    | DeleteTodo of id:int
    | EditTodo of id:int * text:string
    | CompleteTodo of id:int
    | CompleteAll
    | ClearCompleted

type TodoFilter =
    | ShowAll = 0
    | ShowActive = 1
    | ShowCompleted = 2

let [<Literal>] ESCAPE_KEY = 27.
let [<Literal>] ENTER_KEY = 13.

let classNames =
    List.choose (fun (txt,add) -> if add then Some txt else None)
    >> String.concat " "