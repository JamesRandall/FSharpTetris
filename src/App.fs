module App
open App.Model
open Browser.Dom
let canvas = document.querySelector(".game-canvas") :?> Browser.Types.HTMLCanvasElement
let setCanvasSize _ =
  canvas.width <- window.innerWidth
  canvas.height <- window.innerHeight
window.onresize <- setCanvasSize
setCanvasSize ()

let gameLoop,controlStateHandler,initialGameState = App.Game.init canvas
let mutable previousTimestamp = 0.
let mutable currentGameState = initialGameState
//let mutable controlState = ControlState.None
let rec wrappedGameLoop timestamp =
  let timeInFrame = timestamp - previousTimestamp
  previousTimestamp <- timestamp
  currentGameState <- gameLoop currentGameState (timeInFrame * 1.<ms>)
  if not currentGameState.IsInCollision then (window.requestAnimationFrame wrappedGameLoop) |> ignore

window.onkeydown <- (fun ke ->
  let controlState =
    match ke.code with
    | "ArrowLeft" -> ControlState.MoveLeft
    | "ArrowRight" -> ControlState.MoveRight
    | "ArrowDown" -> ControlState.MoveDown
    | "KeyZ" -> ControlState.RotateLeft
    | "KeyX" -> ControlState.RotateRight
    | _ -> ControlState.None
  if controlState <> ControlState.None then ke.preventDefault()
  currentGameState <- controlStateHandler currentGameState controlState
  console.log ke.code
)
window.requestAnimationFrame wrappedGameLoop |> ignore
