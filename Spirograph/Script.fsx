open System.Drawing
open System.IO
open System

type Coordinates = {
    x: int
    y: int
}

type Plotter = {
    position: Coordinates
    color: Color
    direction: float
    bitmap: Bitmap
}

let naiveLine (x1,y1) (plotter: Plotter)=
    let {x=x0;y=y0} = plotter.position
    let color = plotter.color

    let newPlotter = {plotter with position={x=x1; y=y1}}
    let xLen = float (x1 - x0)
    let yLen = float (y1 - y0)

    let x0,y0,x1,y1 = if x0 > x1 then x1,y1,x0,y0 else x0,y0,x1,y1
    if xLen <> 0.0 then
        for x in x0..x1 do
            let proportion = float (x-x0) / xLen
            let y = int (Math.Round(proportion * yLen)) + y0
            printfn "%i" y
            newPlotter.bitmap.SetPixel(x, y, color)

    let x0,y0,x1,y1 = if y0 > y1 then x1,y1,x0,y0 else x0,y0,x1,y1
    if yLen <> 0.0 then
        for y in y0..y1 do
            let proportion = float (y-y0) / yLen
            let x = int (Math.Round(proportion * xLen)) + x0
            printfn "%i" x
            newPlotter.bitmap.SetPixel(x, y, color)
    
    newPlotter

let turn deg plotter =
    let newDir = plotter.direction + deg
    let updatedPlotter = {plotter with direction=newDir}
    printfn "%A" updatedPlotter 
    updatedPlotter

let move dist plotter =
    let {x=startX;y=startY} = plotter.position
    let angle = plotter.direction
    let rads = (angle - 90.0) * Math.PI/180.0
    let endX = (float startX) + (float dist) * cos rads
    let endY = (float startY) + (float dist) * sin rads
    let plotted = naiveLine (int endX, int endY) plotter
    printfn "%A" plotted
    plotted

let polygon (sides: int) length plotter =
    let angle = Math.Round(360.0/float sides)
    Seq.fold (fun s i  -> turn  angle (move length s)) plotter [1.0..(float sides)]


let semiCircle (sides: int) length plotter =
    let angle = Math.Round(360.0/float sides)
    Seq.fold (fun s i  -> turn  angle (move length s)) plotter [1.0..(float sides/2.0)]


let thirdCircle (sides: int) length plotter =
    let angle = Math.Round(360.0/float sides)
    Seq.fold (fun s i  -> turn  angle (move length s)) plotter [1.0..(float sides/3.0)]

let fifthCircle (sides: int) length plotter =
    let angle = Math.Round(360.0/float sides)
    Seq.fold (fun s i  -> turn  angle (move length s)) plotter [1.0..(float sides/5.0)]

let fifthteenthCircle (sides: int) length plotter =
    let angle = Math.Round(360.0/float sides)
    Seq.fold (fun s i  -> turn  angle (move length s)) plotter [1.0..(float sides/15.0)]

let moveTo (x1,y1) plotter =
    {plotter with position={x=x1;y=y1}}

let changeColor color plotter =
    {plotter with color=color}

let saveAs name plotter =
    let path = Path.Combine(__SOURCE_DIRECTORY__, name)
    plotter.bitmap.Save(path)

let generate cmdStripe times fromPlotter =
    let cmdsGen =
        seq {
            while true do
                yield! cmdStripe }
    let cmds = cmdsGen |> Seq.take (times*(List.length cmdStripe))
    cmds |> Seq.fold (fun plot cmd -> cmd plot) fromPlotter

let initialPlotter = {
    position = {x=1000; y=1000}
    color = Color.Orange
    direction = 0.0
    bitmap = new Bitmap(2000, 2000)
}

let cmdStripe =
    [
        changeColor Color.DarkGoldenrod
        move 45
        turn 45.5
        move 100
        fifthCircle 45 45
        changeColor Color.Red
        turn 90.0
        move 45
        fifthteenthCircle 45 45
        turn 45.0
        move 145
        turn 180.4
        move 50
        changeColor Color.Blue
        fifthCircle 50 50
        moveTo (1000, 1000)
    ]

generate cmdStripe 50 initialPlotter |> saveAs "Experiment.png"