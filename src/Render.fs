module App.Render

open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open App.Model

let toColorString color =
  let r = (color >>> 16) 
  let g= (color >>> 8) &&& 0xFF
  let b = color &&& 0xFF
  $"rgb({r},{g},{b})"

let fill (context:CanvasRenderingContext2D) color left top width height =
  context.fillStyle <- (color |> toColorString |> U3.Case1)
  context.fillRect (left, top, width, height)
  
let clearCanvas (context:CanvasRenderingContext2D) =
  fill context 0 0. 0. context.canvas.width context.canvas.height
  
let overlay (context:CanvasRenderingContext2D) =
  context.fillStyle <- ("rgba(0,0,0,0.66)" |> U3.Case1)
  context.fillRect (0., 0., context.canvas.width, context.canvas.height)
  
let fillText (context:CanvasRenderingContext2D) text x y =
  context.textAlign <- "start"
  context.fillStyle <- ("#e0e0e0" |> U3.Case1)
  context.fillText (text,x,y)
  
let centerText (context:CanvasRenderingContext2D) text offsetX offsetY =
  context.textAlign <- "center"
  context.fillStyle <- ("#e0e0e0" |> U3.Case1)
  context.fillText (text, context.canvas.width/2. + offsetX, context.canvas.height/2. - 15. + offsetY)
  
let stroke (context:CanvasRenderingContext2D) strokeSize color x1 y1 x2 y2 =
  context.strokeStyle <- (color |> toColorString |> U3.Case1)
  context.lineWidth <- strokeSize
  context.beginPath()
  context.moveTo (x1,y1)
  context.lineTo (x2,y2)
  context.stroke()
  
let strokeSingle (context:CanvasRenderingContext2D) color x1 y1 x2 y2 =
  stroke context 1. color x1 y1 x2 y2
  
let drawSquareAtCanvasCoords context (color:BlockColor) left top size =
  fill context color.Fill left top size size
  let bottom = top + size
  let right = left + size
  strokeSingle context color.Right right top right bottom
  strokeSingle context color.Bottom left bottom right bottom
  strokeSingle context color.Left left bottom left top
  strokeSingle context color.Top left top right top