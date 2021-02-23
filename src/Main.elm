module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Url
import Url.Parser as Parser exposing ((</>), Parser)



-- ---------------------------
-- ROUTE
-- ---------------------------


type Route
    = Top
    | UserPage String
    | NotFound


route : Parser (Route -> a) a
route =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map UserPage (Parser.s "user" </> Parser.string)
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


type alias Shelf =
    { id : String
    , name : String
    , user : User
    , books : List String
    }


type alias Model =
    { url : Url.Url
    , key : Navigation.Key
    , user : User
    , users : List User
    , shelves : List Shelf
    }


getUser : Cmd Msg
getUser =
    Task.perform GetUser <|
        Task.succeed <|
            User "0001" "Tomoya"


getUsers : Cmd Msg
getUsers =
    Task.perform GetUsers <|
        Task.succeed <|
            [ User "0001" "Tomoya"
            , User "0002" "Hanako"
            , User "0003" "Tarou"
            ]


getShelves : Cmd Msg
getShelves =
    Task.perform GetShelves <|
        Task.succeed <|
            [ Shelf "0001"
                "tana1"
                { id = "0002", name = "Hanako" }
                [ "桃太郎"
                , "金太郎"
                , "かぐや姫"
                ]
            , Shelf "0002"
                "shelf4"
                { id = "0002", name = "Hanako" }
                [ "ウサギとカメ"
                , "赤ずきん"
                ]
            ]


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model url key (User "" "") [] []
    , Cmd.batch [ getUser, getUsers, getShelves ]
    )



-- ---------------------------
-- UPDARTE
-- ---------------------------


type Msg
    = UrlRequest Browser.UrlRequest
    | UrlChanged Url.Url
    | GetUser User
    | GetUsers (List User)
    | GetShelves (List Shelf)


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
            let
                nextModel =
                    { model | url = url }
            in
            case urlToRoute url of
                Top ->
                    ( nextModel
                    , getShelves
                    )

                _ ->
                    ( nextModel
                    , Cmd.none
                    )

        GetUser user ->
            ( { model | user = user }
            , Cmd.none
            )

        GetUsers users ->
            ( { model | users = users }
            , Cmd.none
            )

        GetShelves shelves ->
            ( { model | shelves = shelves }
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
                [ ul [] <|
                    List.map
                        (\user ->
                            li []
                                [ a [ "/user/" ++ user.id |> urlOf |> href ]
                                    [ text (user.name ++ "(" ++ user.id ++ ")") ]
                                ]
                        )
                        model.users
                ]
            }

        UserPage _ ->
            { title = "user"
            , body =
                [ ul [] <|
                    List.map
                        (\shelf ->
                            li []
                                [ text (shelf.name ++ "(" ++ shelf.user.name ++ ")")
                                , ul [] <| List.map (\book -> li [] [ text book ]) shelf.books
                                ]
                        )
                        model.shelves
                ]
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
