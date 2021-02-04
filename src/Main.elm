module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Events exposing (..)
import Url



-- ---------------------------
-- MODEL
-- ---------------------------


type alias User =
    { id : String
    , name : String
    }


type alias Model =
    { url : Url.Url
    , key : Navigation.Key
    , user : User
    }


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model url key (User "0120" "tom")
    , Cmd.none
    )



-- ---------------------------
-- UPDARTE
-- ---------------------------


type Msg
    = UrlRequest Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequest request ->
            case request of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Browser.Document Msg
view model =
    { title = "Book Shelf"
    , body = [ div [] [ text model.user.name ] ]
    }



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequest
        }
