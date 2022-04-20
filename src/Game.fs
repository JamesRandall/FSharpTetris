module App.Game

open App.Model
open Browser.Types
open Fable.Core.JsInterop
open App.Render

let private processTurn frameTime game =
  let (|NoTimeRemaining|TimeRemaining|) timeRemaining = if timeRemaining <= 0.<ms> then NoTimeRemaining else TimeRemaining
  let collides cells block =
    false
  let createNewBlockInPlay () =
    let block = Blocks.getRandomBlock ()
    let constraints = Blocks.getConstraints block
    { Block = Blocks.getRandomBlock() ; X = 5 ; Y = -constraints.MinY }
  
  let newTimeRemaining = game.TimeUntilDrop - frameTime
  let blockInPlay =
    match newTimeRemaining,game.BlockInPlay with
    | TimeRemaining,_ -> game.BlockInPlay
    | NoTimeRemaining,Some blockInPlay ->
      //Some { blockInPlay with Y = blockInPlay.Y + 1 }
      //keep moving until collision
      game.BlockInPlay
    | NoTimeRemaining,None ->
      // TODO: create a new block and place it at the top of the screen - or try to anyway!
      createNewBlockInPlay () |> Some
  
  { game with
      BlockInPlay = blockInPlay
      IsInCollision = blockInPlay |> collides game.Cells
  }

let init (canvas:HTMLCanvasElement) =
  
  // we want a 10x18 hole but we oversize it so we can have the left, right and bottom for the pit walls
  let pitWidth = 12 // left and right are for walls
  let pitHeight = 19 // bottom row is for wall
  
  let context = canvas.getContext_2d()
  context?imageSmoothingEnabled <- false
  context.lineCap <- "square"
  context.translate(0.5,0.5)
  let pitColor =
    { Fill = 0x1F2937
      Top = 0x4B5563
      Left = 0x4B5563
      Right = 0x111827
      Bottom = 0x111827
    }
  let mutable game =
    { Cells =
        {0..(pitHeight-1)}
        |> Seq.map(fun row ->
          {0..(pitWidth-1)}
          |> Seq.map(fun col ->
            if col = 0 || col = pitWidth-1 || row = pitHeight-1 then Cell.Block pitColor else Cell.Empty
          )
          |> Seq.toArray
        )
        |> Seq.toArray
      NextBlock = Blocks.getRandomBlock ()
      BlockInPlay = None
      Score = 0
      Speed = 1000.<ms>
      TimeUntilDrop = 1000.<ms>
      IsInCollision = false
    }
  
  let gameLoop (timestamp:float<ms>) =
    // doing the sizing in the game loop allows us to respond to canvas resizes
    let width = canvas.width
    let height = canvas.height
    let blockSize = (height - (pitHeight |> float)) / (pitHeight |> float) |> floor
    let spacingSize = blockSize + 1.
    let pitCanvasLeft = width/2. - (blockSize*(pitWidth |> float)/2.)
    let pitCanvasTop = 0.
    
    let newGameState = processTurn timestamp game
    
    // row 0 is at the top
    let drawSquare (color:BlockColor) (column:int) (row:int) =
      let x = pitCanvasLeft + (spacingSize * (column |> float))
      let y = pitCanvasTop + (spacingSize * (row |> float))
      drawSquareAtCanvasCoords context color x y blockSize
    let drawBlock (blockInPlay:BlockInPlay) =
      blockInPlay.Block.Current
      |> Array.iteri(fun rowIndex row ->
        row
        |> Array.iteri(fun colIndex cell ->
          if cell > 0 then drawSquare blockInPlay.Block.Color (blockInPlay.X+colIndex) (blockInPlay.Y+rowIndex)
        )
      )
    
    clear context  
    // draw the pit
    newGameState.Cells
    |> Array.iteri(fun row rowContent ->
      rowContent
      |> Array.iteri(fun col cell ->
        match cell with
        | Cell.Empty -> ()
        | Cell.Block blockColor -> drawSquare blockColor col row
      )  
    )
    match newGameState.BlockInPlay with | Some blockInPlay -> drawBlock blockInPlay | None -> ()
    
    game <- newGameState
    true
  gameLoop
  