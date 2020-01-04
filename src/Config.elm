module Config exposing (Config, values)


type alias Config =
    { maxStates : Int
    , numberOfAnts : Int
    , gridWidth : Int
    , gridHeight : Int
    , cellwidth : Int
    }


values : Config
values =
    { maxStates = 100
    , numberOfAnts = 10
    , gridWidth = 50
    , gridHeight = 50
    , cellwidth = 10
    }
