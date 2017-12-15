let input = 277678

type Direction =
| E
| N
| W
| S

type Position = {
    x: int
    y: int
}

type State = {
    maxE: int
    maxN: int
    minW: int
    minS: int
    position: Position
    direction: Direction
}

let start = {
    maxE = 0
    maxN = 0
    minW = 0
    minS = 0
    position = {x=0;y=0}
    direction = E}

let move (state:State) _ =
    match state.direction with
    | E -> 
        if state.position.x > state.maxE then
            {state with maxE = state.position.x; direction = N; position = {x = state.position.x; y = state.position.y + 1}}
        else
            {state with position = {x = state.position.x + 1; y = state.position.y}}
    | N -> 
        if state.position.y > state.maxN then
            {state with maxN = state.position.y; direction = W; position = {x = state.position.x - 1; y = state.position.y}}
        else
            {state with position = {x = state.position.x; y = state.position.y + 1}}
    | W -> 
        if state.position.x < state.minW then
            {state with minW = state.position.x; direction = S; position = {x = state.position.x; y = state.position.y - 1}}
        else
            {state with position = {x = state.position.x - 1; y = state.position.y}}
    | S -> 
        if state.position.y < state.minS then
            {state with minS = state.position.y; direction = E; position = {x = state.position.x + 1; y = state.position.y}}
        else
            {state with position = {x = state.position.x; y = state.position.y - 1}}

let abs (i:int) = System.Math.Abs(i)

let distance state =
    (abs state.position.x) + (abs state.position.y)

seq {2 .. input} 
|> Seq.fold move start 
|> distance
