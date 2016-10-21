// --------------------------------------------------------------------------------------
// A simple FAKE build script that:
//  1) Hosts Suave server locally & reloads web part that is defined in 'app.fsx'
//  2) Deploys the web application to Azure web sites when called with 'build deploy'
// --------------------------------------------------------------------------------------

#r "packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
#r "packages/Suave/lib/net40/Suave.dll"
#r "packages/FAKE/tools/FakeLib.dll"
#load "app.fsx"
open Fake
open App

open System
open System.IO
open Suave
open Suave.Web
open Microsoft.FSharp.Compiler.Interactive.Shell

// --------------------------------------------------------------------------------------
// The following uses FileSystemWatcher to look for changes in 'app.fsx'. When
// the file changes, we run `#load "app.fsx"` using the F# Interactive service
// and then get the `App.app` value (top-level value defined using `let app = ...`).
// The loaded WebPart is then hosted at localhost:8083.
// --------------------------------------------------------------------------------------

let sbOut = new Text.StringBuilder()
let sbErr = new Text.StringBuilder()

let fsiSession =
  let inStream = new StringReader("")
  let outStream = new StringWriter(sbOut)
  let errStream = new StringWriter(sbErr)
  let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
  let argv = Array.append [|"/fake/fsi.exe"; "--quiet"; "--noninteractive"; "-d:DO_NOT_START_SERVER"|] [||]
  FsiEvaluationSession.Create(fsiConfig, argv, inStream, outStream, errStream)

let reportFsiError (e:exn) =
  traceError "Reloading app.fsx script failed."
  traceError (sprintf "Message: %s\nError: %s" e.Message (sbErr.ToString().Trim()))
  sbErr.Clear() |> ignore

// --------------------------------------------------------------------------------------
// Suave server that redirects all request to currently loaded version
// --------------------------------------------------------------------------------------

let getLocalServerConfig port =
  { defaultConfig with
      homeFolder = Some __SOURCE_DIRECTORY__
      logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Debug
      bindings = [ HttpBinding.mkSimple HTTP  "127.0.0.1" port ] }

Target "run" (fun _ ->
  let port = 8083
  startWebServer(getLocalServerConfig port) app
)

// --------------------------------------------------------------------------------------
// Targets for running build script in background (for Atom)
// --------------------------------------------------------------------------------------

open System.Diagnostics

let runningFileLog = __SOURCE_DIRECTORY__ @@ "build.log"
let runningFile = __SOURCE_DIRECTORY__ @@ "build.running"

Target "spawn" (fun _ ->
  if File.Exists(runningFile) then
    failwith "The build is already running!"

  let ps =
    ProcessStartInfo
      ( WorkingDirectory = __SOURCE_DIRECTORY__,
        FileName = __SOURCE_DIRECTORY__  @@ "packages/FAKE/tools/FAKE.exe",
        Arguments = "run --fsiargs build.fsx",
        RedirectStandardOutput = true,
        RedirectStandardError = true,
        UseShellExecute = false )
  use fs = new FileStream(runningFileLog, FileMode.Create, FileAccess.ReadWrite, FileShare.Read)
  use sw = new StreamWriter(fs)
  let p = Process.Start(ps)
  p.ErrorDataReceived.Add(fun data -> printfn "%s" data.Data; sw.WriteLine(data.Data); sw.Flush())
  p.OutputDataReceived.Add(fun data -> printfn "%s" data.Data; sw.WriteLine(data.Data); sw.Flush())
  p.EnableRaisingEvents <- true
  p.BeginOutputReadLine()
  p.BeginErrorReadLine()

  File.WriteAllText(runningFile, string p.Id)
  while File.Exists(runningFile) do
    System.Threading.Thread.Sleep(500)
  p.Kill()
)

Target "attach" (fun _ ->
  if not (File.Exists(runningFile)) then
    failwith "The build is not running!"
  use fs = new FileStream(runningFileLog, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
  use sr = new StreamReader(fs)
  while File.Exists(runningFile) do
    let msg = sr.ReadLine()
    if not (String.IsNullOrEmpty(msg)) then
      printfn "%s" msg
    else System.Threading.Thread.Sleep(500)
)

Target "stop" (fun _ ->
  if not (File.Exists(runningFile)) then
    failwith "The build is not running!"
  File.Delete(runningFile)
)

// --------------------------------------------------------------------------------------
// Minimal Azure deploy script - just overwrite old files with new ones
// --------------------------------------------------------------------------------------

Target "deploy" (fun _ ->
  let sourceDirectory = __SOURCE_DIRECTORY__
  let wwwrootDirectory = __SOURCE_DIRECTORY__ @@ "../wwwroot"
  CleanDir wwwrootDirectory
  CopyRecursive sourceDirectory wwwrootDirectory false |> ignore
)

// --------------------------------------------------------------------------------------
//
// --------------------------------------------------------------------------------------

let npm command args workingDir =
  let args = sprintf "%s %s" command (String.concat " " args)
  let cmd, args = if EnvironmentHelper.isUnix then "npm", args else "cmd", ("/C npm " + args)
  let ok =
    execProcess (fun info ->
      info.FileName <- cmd
      info.WorkingDirectory <- workingDir
      info.Arguments <- args) TimeSpan.MaxValue
  if not ok then failwith (sprintf "'%s %s' task failed" cmd args)

let node command args workingDir =
  let args = sprintf "%s %s" command (String.concat " " args)
  let cmd, args = if EnvironmentHelper.isUnix then "node", args else "cmd", ("/C node " + args)
  async { 
    execProcess (fun info ->
      info.FileName <- cmd
      info.WorkingDirectory <- workingDir
      info.Arguments <- args) TimeSpan.MaxValue |> ignore } |> Async.Start

Target "fable" (fun _ ->
  __SOURCE_DIRECTORY__ </> "client" |> npm "install" []
  __SOURCE_DIRECTORY__ </> "client" |> npm "run build" []
)

"fable" ==> "run"

RunTargetOrDefault "run"