module Logger exposing (start, log)

import Pi exposing (Pi, Pid)

type alias State =
    { self : Pid
    }


start : Pi out -> ( Pid, Pi out )
start pi =
    let
        loop self =
            Pi.receive <| \message pi0 ->
            ( loop self, Pi.sendOut message pi0 )
    in
    Pi.spawn (\self -> loop { self = self }) pi


log string logger pi =
    Pi.send string logger pi
