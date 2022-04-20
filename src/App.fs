module App
open App.Model
open Browser.Dom
let canvas = document.querySelector(".game-canvas") :?> Browser.Types.HTMLCanvasElement
let setCanvasSize _ =
  canvas.width <- window.innerWidth
  canvas.height <- window.innerHeight
window.onresize <- setCanvasSize
setCanvasSize ()

let gameLoop = App.Game.init canvas
let mutable previousTimestamp = 0.
let rec wrappedGameLoop timestamp =
  let timeInFrame = timestamp - previousTimestamp
  previousTimestamp <- timestamp
  let shouldContinue = gameLoop (timeInFrame * 1.<ms>)
  if shouldContinue then (window.requestAnimationFrame wrappedGameLoop) |> ignore

window.requestAnimationFrame wrappedGameLoop |> ignore
