module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Url
import Url.Parser as Parser exposing ((</>), Parser)



-- ---------------------------
-- ROUTE
-- ---------------------------


type Route
    = Top
    | MyPage
    | NotFound


route : Parser (Route -> a) a
route =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map MyPage (Parser.s "user")
        , Parser.map MyPage (Parser.s "mypage")
        ]


routeLocal : Parser (Route -> a) a
routeLocal =
    Parser.s "src" </> Parser.s "Main.elm" </> route


urlToRoute : Url.Url -> Route
urlToRoute url =
    Parser.parse routeLocal url |> Maybe.withDefault NotFound



-- ---------------------------
-- URL
-- ---------------------------


localUrlPrefix : String
localUrlPrefix =
    "/src/Main.elm"


urlOf : String -> String
urlOf =
    (++) localUrlPrefix



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
    case urlToRoute model.url of
        NotFound ->
            { title = "404"
            , body =
                [ div [] [ text "404 | not found" ]
                ]
            }

        Top ->
            { title = "Book Shelf"
            , body =
                [ div [] [ text <| "ようこそ" ++ model.user.name ++ "さん (" ++ Url.toString model.url ++ ")" ]
                , ul []
                    [ li [] [ a [ urlOf "/mypage" |> href ] [ text "mypage" ] ]
                    , li [] [ a [ urlOf "/hogehoge" |> href ] [ text "hogehoge" ] ]
                    , li [] [ a [ href "http://www.google.com" ] [ text "google" ] ]
                    ]
                ]
            }

        MyPage ->
            { title = "my page"
            , body = []
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
