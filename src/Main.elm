module Main exposing (main)

import Browser
import Browser.Events as Events
import Html exposing (Html)
import JsLogger
import Json.Decode as Decode
import Logger
import Pi


type Message
    = Event String
    | OutEvent String


type alias Pi =
    Pi.Pi Message


type alias Model =
    { logger : Pi.Pid
    , pi : Pi
    }


init flags =
    let
        pi =
            Pi.make { onOut = OutEvent }

        -- probably should be Result spawnError ( Pid, Pi )
        ( logger, pi0 ) =
            Logger.start pi
    in
    ( { logger = logger
      , pi = pi0
      }
    , Cmd.none
    )


updates : List Message -> Model -> ( Model, Cmd Message )
updates messages model =
    let
        foldMessage message ( subModel, command ) =
            let
                ( newModel, nextCommand ) =
                    update message subModel
            in
            ( newModel, Cmd.batch [ command, nextCommand ])
    in
    List.foldl foldMessage ( model, Cmd.none ) messages


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Event string ->
            case Logger.log string model.logger model.pi of
                Ok pi ->
                    let
                        ( out, pi0 ) =
                            Pi.run pi
                    in
                    updates out { model | pi = pi0 }

                Err reason ->
                    ( model
                    , JsLogger.log reason
                    )

        OutEvent string ->
            ( model
            , JsLogger.log string
            )


subscriptions : Model -> Sub Message
subscriptions model =
    Events.onKeyDown (Decode.map Event (Decode.field "key" Decode.string))


view : Model -> Html Message
view model =
    Html.div
        []
        [ Html.text "pi"
        ]


main : Program () Model Message
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
