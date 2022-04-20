module App.Blocks
open App.Model
open System
let private random = System.Random()  

let getConstraints (block:Block) =
  block.Current
  |> Seq.fold (fun (c1,_,rowIndex) row ->
    row
    |> Seq.fold(fun (c2,colIndex,_) cell ->
      let newConstraints =
        { c2 with
            MinX = if cell > 0 && colIndex < c2.MinX then colIndex else c2.MinX
            MinY = if cell > 0 && rowIndex < c2.MinY then rowIndex else c2.MinY
            MaxX = if cell > 0 && colIndex > c2.MaxX then colIndex else c2.MaxX
            MaxY = if cell > 0 && rowIndex > c2.MaxY then rowIndex else c2.MaxY
        }
      newConstraints,colIndex+1,rowIndex
    ) (c1,0,rowIndex)
  ) ({ MinX = Int32.MaxValue ; MinY = Int32.MaxValue ; MaxX = Int32.MinValue ; MaxY = Int32.MinValue },0,0)
  |> (fun (c,_,_) -> c)

let getRandomBlock() =
  let color3d fill light dark =
    { Fill = fill ; Top = light ; Left = light ; Right = dark ; Bottom = dark }
  
  // colors come from tailwind - https://tailwindcss.com/docs/customizing-colors
  let colors = [|
    color3d 0xDC2626 0xF87171 0x991B1B // red
    color3d 0x16A34A 0x4ADE80 0x166534 // green
    color3d 0x2563EB 0x60A5FA 0x1E40AF // blue
    color3d 0xCA8A04 0xFACC15 0x854D0E // yellow
    color3d 0x4B5563 0x94A3B8 0x1E293B // gray
  |]
  
  let empty = [|
    [| 0 ; 0 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let v1 = [|
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 1 ; 0 ; 0 |]
  |]

  let v2 = [|
    [| 0 ; 0 ; 0 ; 0 |]
    [| 1 ; 1 ; 1 ; 1 |]
    [| 0 ; 0 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let t1 = [|
    [| 0 ; 0 ; 0 ; 0 |]
    [| 1 ; 1 ; 1 ; 0 |]
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let t2 = [|
    [| 0 ; 1 ; 0 ; 0 |]
    [| 1 ; 1 ; 0 ; 0 |]
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let t3 = [|
    [| 0 ; 1 ; 0 ; 0 |]
    [| 1 ; 1 ; 1 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let t4 = [|
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 1 ; 1 ; 0 |]
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let l1 = [|
    [| 0 ; 0 ; 0 ; 0 |]
    [| 1 ; 1 ; 1 ; 0 |]
    [| 1 ; 0 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let l2 = [|
    [| 1 ; 1 ; 0 ; 0 |]
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let l3 = [|
    [| 0 ; 0 ; 1 ; 0 |]
    [| 1 ; 1 ; 1 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let l4 = [|
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 1 ; 1 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let flipped_l1 = [|
    [| 1 ; 0 ; 0 ; 0 |]
    [| 1 ; 1 ; 1 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let flipped_l2 = [|
    [| 0 ; 1 ; 1 ; 0 |]
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let flipped_l3 = [|
    [| 0 ; 0 ; 0 ; 0 |]
    [| 1 ; 1 ; 1 ; 0 |]
    [| 0 ; 0 ; 1 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let flipped_l4 = [|
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 1 ; 0 ; 0 |]
    [| 1 ; 1 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let s1 = [|
    [| 0 ; 0 ; 0 ; 0 |]
    [| 1 ; 1 ; 0 ; 0 |]
    [| 0 ; 1 ; 1 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let s2 = [|
    [| 0 ; 0 ; 1 ; 0 |]
    [| 0 ; 1 ; 1 ; 0 |]
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let flipped_s1 = [|
    [| 0 ; 0 ; 0 ; 0 |]
    [| 0 ; 1 ; 1 ; 0 |]
    [| 1 ; 1 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let flipped_s2 = [|
    [| 0 ; 1 ; 0 ; 0 |]
    [| 0 ; 1 ; 1 ; 0 |]
    [| 0 ; 0 ; 1 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]

  let o = [|
    [| 0 ; 1 ; 1 ; 0 |]
    [| 0 ; 1 ; 1 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
    [| 0 ; 0 ; 0 ; 0 |]
  |]
  
  let blocks = [|
    [| v1 ; v2 |]
    [| t1 ; t2 ; t3 ; t4 |]
    [| l1 ; l2 ; l3 ; l4 |]
    [| flipped_l1 ; flipped_l2 ; flipped_l3 ; flipped_l4 |]
    [| s1 ; s2 |]
    [| flipped_s1 ; flipped_s2 |]
    [| o |]
  |]
  
  let rotations = blocks.[random.Next(0,blocks.Length)]
  { Rotations = rotations
    CurrentRotation = random.Next(0,rotations.Length)
    Color = colors.[random.Next(0,colors.Length)]
  }