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
  window.requestAnimationFrame wrappedGameLoop |> ignore

let private controlStateFromKeyCode keyCode =
  match keyCode with
  | "ArrowLeft" -> ControlState.MoveLeft
  | "ArrowRight" -> ControlState.MoveRight
  | "ArrowDown" -> ControlState.MoveDown
  | "KeyZ" -> ControlState.RotateLeft
  | "KeyX" -> ControlState.RotateRight
  | "Enter" -> ControlState.NewGame
  | _ -> ControlState.None

window.onkeydown <- (fun ke ->
  // we handle key repeat using our own timer as we need it to be faster than the standard repeat speed
  if not ke.repeat then
    let controlState = ke.code |> controlStateFromKeyCode
    if controlState <> ControlState.None then ke.preventDefault()
    currentGameState <- controlStateHandler currentGameState controlState
    console.log ke.code
)
window.onkeyup <- (fun ke ->
  if currentGameState.ControlState = (ke.code |> controlStateFromKeyCode) then
    currentGameState <- controlStateHandler currentGameState ControlState.None
)
window.requestAnimationFrame wrappedGameLoop |> ignore
