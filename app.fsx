#I "packages/Suave/lib/net40"
#r "packages/Suave/lib/net40/Suave.dll"
#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "System.Data.dll"
#r "System.Data.Linq.dll"
#r "packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll"
#load "Secrets.fsx"

open FSharp.Data.Sql
open System
open System.IO
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open FSharp.Data
open Secrets

// -------------------------------------------------------------------------------------------------
// Agent for keeping the state of the drawing
// -------------------------------------------------------------------------------------------------

// Using JSON provider to get a type for rectangles with easy serialization
type Rect = JsonProvider<"""{"x1":0.0,"y1":0.0,"x2":10.0,"y2":10.0}""">

// We can add new rectangle or request a list of all rectangles
type Message =
  | AddRect of Rect.Root
  | GetRects of AsyncReplyChannel<list<Rect.Root>>

// Agent that keeps the state and handles 'Message' requests
let agent = MailboxProcessor.Start(fun inbox ->
  let rec loop rects = async {
    let! msg = inbox.Receive()
    match msg with
    | AddRect(r) -> return! loop (r::rects)
    | GetRects(repl) ->
        repl.Reply(rects)
        return! loop rects }
  loop [] )

// -------------------------------------------------------------------------------------------------
// The web server - REST api and static file hosting
// -------------------------------------------------------------------------------------------------

let webRoot = Path.Combine(__SOURCE_DIRECTORY__, "web")
let clientRoot = Path.Combine(__SOURCE_DIRECTORY__, "client")

let noCache =
  Writers.setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  >=> Writers.setHeader "Pragma" "no-cache"
  >=> Writers.setHeader "Expires" "0"

let getRectangles ctx = async {
  let! rects = agent.PostAndAsyncReply(GetRects)
  let json = JsonValue.Array [| for r in rects -> r.JsonValue |]
  return! ctx |> Successful.OK(json.ToString()) }

let addRectangle ctx = async {
  use ms = new StreamReader(new MemoryStream(ctx.request.rawForm))
  agent.Post(AddRect(Rect.Parse(ms.ReadToEnd())))
  return! ctx |> Successful.OK "added" }



type Sql = SqlDataProvider<Common.DatabaseProviderTypes.MSSQLSERVER, Secrets.ConnectionString>

type DbContext = Sql.dataContext
type Product = DbContext.``dbo.DimProductEntity``

let getContext() = Sql.GetDataContext()
let getProducts (ctx : DbContext) : Product list =
    ctx.Dbo.DimProduct |> Seq.toList 


//let a = getContext() |> getGenres |> 
//        Seq.take 10 |> 
//        Seq.iter (fun x -> printfn "My name is %A %A and I  have %A children and I am a %A bike buyer"
                            //x.FirstName x.LastName x.TotalChildren x.BikeBuyer)

let readImage (value : byte[]) = 
    Convert.ToBase64String(value)
    //let stream = new MemoryStream(value)
    //let ms = new MemoryStream()
    //stream.CopyTo(ms)
    //System.Drawing.Image.FromStream(ms)

let t = getContext() |> getProducts 



let details (products : seq<Product>) = 
    let a = new System.Text.StringBuilder()
    for album in products do
        let x = readImage(album.LargePhoto)
        a.AppendFormat("""<img src="data:image/png;base64,{0}"/>""", x) |> ignore
    a.ToString()

let app =
  choose [
    // REST API for adding/getting rectangles
    GET >=> path "/getrects" >=> getRectangles
    POST >=> path "/addrect" >=> addRectangle

    path "/blah" >=> OK (details(t))

    // Serving the generated JS and source maps
    path "/out/bundle.js" >=> noCache >=> Files.browseFile clientRoot (Path.Combine("out", "bundle.js"))
    path "/out/bundle.js.map" >=> noCache >=> Files.browseFile clientRoot (Path.Combine("out", "bundle.js.map"))
    pathScan "/node_modules/%s.js" (sprintf "/node_modules/%s.js" >> Files.browseFile clientRoot)

    pathScan "/node_modules/%s.css" (sprintf "/node_modules/%s.css" >> Files.browseFile clientRoot)
    pathScan "/node_modules/%s.min.js.map" (sprintf "/node_modules/%s.min.js.map" >> Files.browseFile clientRoot)
    pathScan "/lib/%s" (sprintf "/lib/%s" >> Files.browseFile clientRoot)
    // Serving index and other static files
    path "/" >=> Files.browseFile webRoot "index.html"
    path "/" >=> Files.browseFile webRoot "some.css"
    Files.browse webRoot
  ]
