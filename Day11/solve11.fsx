open System.IO
open System

type Movement = {
    direction: string
    steps: int
}

type TotalMovement = {
    NE: int
    N: int
    NW: int
}

type State = {
    tm: TotalMovement
    distance: int
    maxDistance: int
}

let splitOnComma (s:string) = s.Split([|','|]) 

let simplify_NW_NE_N_Movements (tm:TotalMovement) =
    if tm.NW > 0 && tm.NE > 0 then
        {tm with NE = tm.NE - 1; NW = tm.NW - 1; N = tm.N + 1}
    else
        tm

{ NE = 1; N = 0; NW = 1 } |> simplify_NW_NE_N_Movements

let simplify_N_SE_NE_Movements (tm:TotalMovement) =
    if tm.NW < 0 && tm.N > 0 then
        { tm with NW = tm.NW + 1; N = tm.N - 1; NE = tm.NE + 1 }
    else
        tm

{ NE = 0; N = 1; NW = -1 } |> simplify_N_SE_NE_Movements
let simplify_NE_S_SE_Movements (tm:TotalMovement) =
    if tm.NE > 0 && tm.N < 0 then
        { tm with NE = tm.NE - 1; N = tm.N + 1; NW = tm.NW - 1 }
    else
        tm

{ NE = 1; N = -1; NW = 0 } |> simplify_NE_S_SE_Movements

let simplify_SE_SW_S_Movements (tm:TotalMovement) =
    if tm.NW < 0 && tm.NE < 0 then
        { tm with NE = tm.NE + 1; NW = tm.NW + 1; N = tm.N - 1 }
    else
        tm

{ NE = -1; N = 0; NW = -1 } |> simplify_SE_SW_S_Movements

let simplify_NW_S_SW_Movements (tm:TotalMovement) =
    if tm.NW > 0 && tm.N < 0 then
        { tm with NW = tm.NW - 1; N = tm.N + 1; NE = tm.NE - 1 }
    else
        tm

{ NE = 0; N = -1; NW = 1 } |> simplify_NW_S_SW_Movements

let simplify_N_SW_NW_Movements (tm:TotalMovement) =
    if tm.NE < 0 && tm.N > 0 then
        { tm with NE = tm.NE + 1; N = tm.N - 1; NW = tm.NW + 1 }
    else
        tm

{ NE = -1; N = 1; NW = 0 } |> simplify_N_SW_NW_Movements

let simplify =
    simplify_N_SE_NE_Movements
    >> simplify_NE_S_SE_Movements
    >> simplify_SE_SW_S_Movements
    >> simplify_NW_S_SW_Movements
    >> simplify_N_SW_NW_Movements
    >> simplify_NW_NE_N_Movements

let abs (i:int) = Math.Abs i

let summarize (tm:TotalMovement) = (abs tm.N) + (abs tm.NE) + (abs tm.NW) 

let move (tm:TotalMovement) (movement:string) =
    match movement with 
    | "n" -> {tm with N = tm.N + 1}
    | "ne" -> {tm with NE = tm.NE + 1}
    | "se" -> {tm with NW = tm.NW - 1}
    | "s" -> {tm with N = tm.N - 1}
    | "sw" -> {tm with NE = tm.NE - 1}
    | "nw" -> {tm with NW = tm.NW + 1}
    | _ -> failwithf "WTF? Error in input: %s" movement

let folder (state:State) (movement:string) =
    let newTm = move state.tm movement |> simplify
    let distance = newTm |> summarize
    let maxDistance = if distance > state.maxDistance then distance else state.maxDistance
    { tm = newTm; distance = distance; maxDistance = maxDistance }

let startState = {
    tm = { NW = 0; N = 0; NE = 0 }
    distance = 0
    maxDistance = 0
}

Path.Combine(__SOURCE_DIRECTORY__, "input11.txt")
|> File.ReadAllText
|> splitOnComma
|> Array.fold folder startState