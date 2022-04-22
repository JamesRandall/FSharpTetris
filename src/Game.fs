module App.Game

open App.Model
open Browser.Types
open Fable.Core.JsInterop
open App.Render
open Browser

// we want a 10x18 hole but we oversize it so we can have the left, right and bottom for the pit walls
let private pitWidth = 12 // left and right are for walls
let private pitHeight = 19 // bottom row is for wall
let private pitColor = { Fill = 0x1F2937 ; Top = 0x4B5563 ; Left = 0x4B5563 ;Right = 0x111827 ; Bottom = 0x111827 }
let private emptyRow =
  {0..pitWidth-1}
  |> Seq.map (fun col -> if col = 0 || col = pitWidth-1 then Cell.Wall else Cell.Empty)
  |> Seq.toList
let private rowRemovalTimeMs = 200.<ms>
let private keyRepeatTimeMs = 150.<ms>
let private speedClampedKeyRepeatTimeMs = min keyRepeatTimeMs

let private isInCollision blockArray x y (pit:Cell list list) =
  blockArray
  |> List.mapi(fun rowIndex row ->
    let pitRow = if (rowIndex+y) >= pit.Length || (rowIndex+y) < 0 then [] else pit.[rowIndex+y]
    row
    |> List.mapi(fun colIndex cell ->
      // if either cell is empty then their is no collision
      if (colIndex+x) >= pitRow.Length || (colIndex+x) < 0 then true else pitRow.[colIndex+x] = Cell.Empty || cell = 0 
    )
    |> List.skipWhile id
  )
  |> List.concat
  |> List.tryHead
  |> Option.isSome

let private updateForRowRemoval game =
  let shouldBeRemoved row =
    // if we have as many blocks as the pit is wide (minus the walls) then we can remove the row
    let blockCount = row |> List.filter(function | Cell.Block _ -> true | _ -> false) |> List.length
    blockCount = pitWidth-2
    
  let rowIndexes =
    game.Cells
    |> List.mapi(fun rowIndex row ->
      if row |> shouldBeRemoved then [rowIndex] else []
    )
    |> List.concat
  if rowIndexes |> List.isEmpty then
    game
  else
    { game with
        GameMode = rowIndexes |> ErasingRowMode.BlankingRows |> GameMode.ErasingRows
        TimeUntilNextGameAction = rowRemovalTimeMs }
    
let private updateWithNextBlock game =
  match game.GameMode with
  | GameMode.Normal ->
    let nextBlock = game.NextBlock
    let nextBlockConstraints = Blocks.getConstraints nextBlock
    { game with
        BlockInPlay = { Block = nextBlock ; X = 5 ;  Y = -nextBlockConstraints.MinY } |> Some
        NextBlock = Blocks.getRandomBlock ()
    }
  | _ -> game
  
let private eraseRows rowIndexes game =
  let newCells =
    game.Cells
    |> List.mapi (fun rowIndex row ->
      if rowIndexes |> List.contains rowIndex then
        emptyRow
      else
        row
    )
  { game with
      Cells = newCells
      GameMode = ErasingRowMode.FallingRows |> GameMode.ErasingRows
      Score = game.Score + (rowIndexes.Length * (10<points> + (game.Level |> int) * 2<points>))
      RowsDeleted = game.RowsDeleted + (rowIndexes.Length * 1<rows>)
      TimeUntilNextGameAction = rowRemovalTimeMs
  }
  
  
let private fallRows game =
  // we take a fairly simple approach to making rows fall - we find the deepest empty row in the pit and everything
  // above it falls by one row, we repeat this until nothing more falls (which we detect by the pit being unchanged)
  // its by no means the most optimal way of doing it but has the advantage of being simple!
  
  let _,_,newCells =
    let reversedCells = game.Cells |> List.rev
    reversedCells
    |> List.fold (fun (isFalling,rowIndex,newCells) currentRow ->
      if rowIndex = 0 then
        // the first row is the bottom of the pit - so we skip it, but we need to count it so we can keep the indexes tracked
        (isFalling,rowIndex+1,newCells |> List.append [currentRow])
      else
        let rowToAppendWhenFalling = if rowIndex = pitHeight-1 then emptyRow else reversedCells.[rowIndex+1]
        if isFalling then
          (isFalling,rowIndex+1,rowToAppendWhenFalling :: newCells)
        else
          let isNowFalling = currentRow = emptyRow
          (isNowFalling,rowIndex+1,(if isNowFalling then rowToAppendWhenFalling else currentRow) :: newCells)
    ) (false,0,[])
  let newCellsAsArray = newCells
  let noChange = newCellsAsArray = game.Cells
  { game with
      GameMode = if noChange then GameMode.Normal else ErasingRowMode.FallingRows |> GameMode.ErasingRows
      Cells = newCellsAsArray
      TimeUntilNextGameAction = rowRemovalTimeMs
  }
 
let private populatePitWithBlockInPlay blockInPlay game =
  // F# arrays are mutable but keeping things immutably functional in style - chose arrays because I'm doing a lot
  // of indexed access
  let newCells =
    game.Cells
    |> List.mapi(fun rowIndex pitRow ->
      let blockY = rowIndex - blockInPlay.Y
      if blockY >= 0 && blockY < blockInPlay.Current.Length then
        let blockRow = blockInPlay.Current.[blockY]
        pitRow
        |> List.mapi(fun colIndex cell ->
          let blockX = colIndex - blockInPlay.X
          if blockX >= 0 && blockX < blockRow.Length then 
            if blockRow.[blockX] <> 0 then Cell.Block blockInPlay.Block.Color else cell
          else
            cell
        )
      else
        pitRow
    )
  { game with Cells = newCells ; BlockInPlay = None }
  
let private handleControlState game =
  let rotateLeft block =
    { block with
        CurrentRotation =
          if block.CurrentRotation - 1 < 0 then block.Rotations.Length-1 else block.CurrentRotation-1
    }
  let rotateRight block =
    { block with
        CurrentRotation =
          if block.CurrentRotation + 1 >= block.Rotations.Length then 0 else block.CurrentRotation+1
    }
  let updateGame blockInPlay = { game with BlockInPlay = Some blockInPlay }
  let resetClock gameToReset = { gameToReset with TimeUntilNextGameAction = gameToReset.Speed }
  
  game.BlockInPlay
  |> Option.map (fun blockInPlay ->
    let candidateNewGameState  =
      match game.ControlState with
      | ControlState.MoveLeft -> { blockInPlay with X = blockInPlay.X - 1 } |> updateGame
      | ControlState.MoveRight -> { blockInPlay with X = blockInPlay.X + 1 } |> updateGame
      | ControlState.MoveDown -> { blockInPlay with Y = blockInPlay.Y + 1 } |> updateGame |> resetClock
      | ControlState.RotateLeft -> { blockInPlay with Block = blockInPlay.Block |> rotateLeft } |> updateGame
      | ControlState.RotateRight -> { blockInPlay with Block = blockInPlay.Block |> rotateRight } |> updateGame
      | _ -> game
    candidateNewGameState.BlockInPlay
    |> Option.map(fun candidateBlockInPlay ->
      if isInCollision candidateBlockInPlay.Current candidateBlockInPlay.X candidateBlockInPlay.Y game.Cells then
        game
      else
        candidateNewGameState
    )
    |> Option.defaultValue game
  )
  |> Option.defaultValue game
  
let private handleControlStateRepeat frameTime game =
  let keyboardTimeRemaining = game.TimeUntilKeyRepeat - frameTime
  if keyboardTimeRemaining <= 0.<ms> then
    { (game |> handleControlState) with TimeUntilKeyRepeat = (speedClampedKeyRepeatTimeMs game.Speed) + frameTime }
  else
    { game with TimeUntilKeyRepeat = keyboardTimeRemaining }

let private processTurn frameTime game =
  let (|NoTimeRemaining|TimeRemaining|) timeRemaining = if timeRemaining <= 0.<ms> then NoTimeRemaining else TimeRemaining
  
  let newTimeRemaining = game.TimeUntilNextGameAction - frameTime
  
  match game.GameMode with
  | GameMode.ErasingRows erasureMode ->
    match newTimeRemaining with
    | NoTimeRemaining ->
      match erasureMode with
      | ErasingRowMode.BlankingRows rowIndexes ->
        game |> eraseRows rowIndexes
      | ErasingRowMode.FallingRows ->
        game |> fallRows
    | TimeRemaining ->
      { game with TimeUntilNextGameAction = newTimeRemaining } 
  | GameMode.GameOver -> game
  | GameMode.Normal ->
    let newGameState =
      match newTimeRemaining,game.BlockInPlay with
      | TimeRemaining,_ -> game
      | NoTimeRemaining,Some blockInPlay ->
        let wouldCollide = isInCollision blockInPlay.Block.Current blockInPlay.X (blockInPlay.Y+1) game.Cells
        if wouldCollide then
          game
          |> populatePitWithBlockInPlay blockInPlay
          |> updateForRowRemoval
          |> updateWithNextBlock
        else
          { game with BlockInPlay = { blockInPlay with Y = blockInPlay.Y + 1 } |> Some }
      | NoTimeRemaining,None ->
        game |> updateWithNextBlock
    
    { newGameState with
        IsInCollision =
          newGameState.BlockInPlay
          |> Option.map (fun blockInPlay -> isInCollision blockInPlay.Block.Current blockInPlay.X blockInPlay.Y game.Cells)
          |> Option.defaultValue false
        TimeUntilNextGameAction =
          match game.GameMode with
          | GameMode.Normal -> if newTimeRemaining < 0.<ms> then game.Speed else newTimeRemaining
          | _ -> game.TimeUntilNextGameAction
    }

let init (canvas:HTMLCanvasElement) =
  let context = canvas.getContext_2d()
  // setting up the context only needs to be done once - these settings let us making things square and pixely
  context?imageSmoothingEnabled <- false
  context.lineCap <- "square"
  context.translate(0.5,0.5)
  
  let initialGameState =
    { Cells =
        {0..(pitHeight-1)}
        |> Seq.map(fun row ->
          {0..(pitWidth-1)}
          |> Seq.map(fun col ->
            if col = 0 || col = pitWidth-1 || row = pitHeight-1 then Cell.Wall else Cell.Empty
          )
          |> Seq.toList
        )
        |> Seq.toList
      NextBlock = Blocks.getRandomBlock ()
      BlockInPlay = None
      Score = 0<points>
      TimeUntilNextGameAction = 1000.<ms>
      IsInCollision = false
      GameMode = GameMode.Normal
      RowsDeleted = 10<rows>
      ControlState = ControlState.None
      TimeUntilKeyRepeat = keyRepeatTimeMs
    }
  
  let gameLoop game (frameTime:float<ms>) =
    // doing the sizing in the game loop allows us to respond to canvas resizes while the game is in progress
    let width = canvas.width
    let height = canvas.height
    let blockSize = (height - (pitHeight |> float)) / (pitHeight |> float) |> floor
    let spacingSize = blockSize + 1.
    let pitCanvasLeft = width/2. - (blockSize*(pitWidth |> float)/2.)
    let pitCanvasTop = 0.
    
    // game rendering
    let drawSquare (color:BlockColor) (column:int) (row:int) =
      let x = pitCanvasLeft + (spacingSize * (column |> float))
      let y = pitCanvasTop + (spacingSize * (row |> float))
      drawSquareAtCanvasCoords context color x y blockSize
    let drawBlock (blockInPlay:BlockInPlay) =
      blockInPlay.Block.Current
      |> List.iteri(fun rowIndex row ->
        row
        |> List.iteri(fun colIndex cell ->
          if cell > 0 then drawSquare blockInPlay.Block.Color (blockInPlay.X+colIndex) (blockInPlay.Y+rowIndex)
        )
      )
    let drawPit cells blockInPlay =
      cells
      |> List.iteri(fun row rowContent ->
        rowContent
        |> List.iteri(fun col cell ->
          match cell with
          | Cell.Empty -> ()
          | Cell.Wall -> drawSquare pitColor col row
          | Cell.Block blockColor -> drawSquare blockColor col row
        )  
      )
      match blockInPlay with | Some blockInPlay -> drawBlock blockInPlay | None -> ()
    let drawScore () =
      context.font <- "30px Consolas, Menlo, monospace"
      fillText context $"Score {game.Score}" 16. 32.
      fillText context $"Level {game.Level}" 16. 64.
    
    let newGameState =
      game
      |> handleControlStateRepeat frameTime
      |> processTurn frameTime
    
    clearCanvas context  
    drawPit newGameState.Cells newGameState.BlockInPlay
    drawBlock { Block = game.NextBlock ; X = pitWidth+2 ; Y = 2 }
    drawScore ()
    
    newGameState
    
  let updateControlState game controlState =
    handleControlState
      { game with ControlState = controlState ; TimeUntilKeyRepeat = speedClampedKeyRepeatTimeMs game.Speed }

  gameLoop,updateControlState,initialGameState
  