module Pi exposing (Pi, Pid, make, reply, receive, run, send, sendOut, spawn)

import Dict exposing (Dict)


type Pid
    = Pid Int


type alias Message =
    String


type alias MessageBox =
    List Message


type alias Error =
    String


type ProcessState out
    = Initializing
    | Receive (Message -> Pi out -> ( ProcessState out, Pi out ))
    | Reply (Pi out -> Pi out)


type alias Process out =
    { messageBox : MessageBox
    , state : ProcessState out
    }


type alias Pi out =
    { next : Int
    , onOut : Message -> out
    , outMessages : List Message
    , processes : Dict Int (Process out)
    }


make : { onOut : Message -> out } -> Pi out
make { onOut } =
    { next = 0
    , onOut = onOut
    , outMessages = []
    , processes = Dict.empty
    }


receive =
    Receive


reply =
    Reply


run : Pi out -> ( List out, Pi out )
run pi =
    let
        foldProcess rawPid process ( processes, pi0 ) =
            case process.state of
                Receive operation ->
                    case process.messageBox of
                        [] ->
                            ( Dict.insert rawPid process processes, pi0 )

                        message :: rest ->
                            let
                                ( newState, pi1 ) =
                                    operation message pi0

                                newProcess =
                                    { state = newState, messageBox = rest }
                            in
                            ( Dict.insert rawPid newProcess processes, pi1 )

                Reply operation ->
                    ( processes, operation pi0 )

                _ ->
                    ( Dict.insert rawPid process processes, pi0 )

        ( newProcesses, pi2 ) =
            Dict.foldl foldProcess ( Dict.empty, pi ) pi.processes

        outMessages =
            List.map pi2.onOut pi2.outMessages

        newPi =
            { pi2 | processes = newProcesses, outMessages = [] }
    in
    ( outMessages
    , newPi
    )


send : Message -> Pid -> Pi out -> Result Error (Pi out)
send message (Pid pid) pi =
    case Dict.get pid pi.processes of
        Just process ->
            let
                newProcess =
                    { process | messageBox = message :: process.messageBox }

                newProcesses =
                    Dict.insert pid newProcess pi.processes
            in
            Ok { pi | processes = newProcesses }

        Nothing ->
            Err "process not found"


sendOut : Message -> Pi out -> Pi out
sendOut message pi =
    { pi | outMessages = pi.outMessages ++ [ message ] }


spawn : (Pid -> ProcessState out) -> Pi out -> ( Pid, Pi out )
spawn proc pi1 =
    let
        next =
            pi1.next

        pid =
            Pid next

        process =
            { messageBox = []
            , state = Initializing
            }

        pi2 =
            { pi1
                | next = next + 1
                , processes = Dict.insert next process pi1.processes
            }

        procState =
            proc pid

        pi3 =
            { pi2
                | processes =
                    Dict.insert
                        next
                        { process | state = procState }
                        pi2.processes
            }
    in
    ( pid
    , pi3
    )
