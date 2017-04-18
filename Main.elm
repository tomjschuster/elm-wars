module Main exposing (..)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (Decoder, string, maybe, list, int)
import Json.Decode.Pipeline exposing (required, decode, optional)
import Task


type alias Model =
    { query : String, person : Maybe Person, language : Language }


init : ( Model, Cmd Msg )
init =
    ( Model "" Nothing English
    , Cmd.none
    )


type alias QueryResult =
    { count : Int
    , people : List Person
    }


type alias Person =
    { url : String
    , name : String
    , gender : String
    , planetUrl : String
    , planet : Maybe Planet
    }


type alias Planet =
    { climate : String
    }


type Language
    = English
    | Wookiee


type Msg
    = UpdateQuery String
    | QueryPerson
    | GetPerson (Result Http.Error (Maybe Person))
    | TranslatePerson Language


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery query ->
            { model | query = query } ! []

        QueryPerson ->
            if model.query == "" then
                ( { model | person = Nothing }, Cmd.none )
            else
                ( model, queryPerson model.query )

        GetPerson (Ok person) ->
            ( { model | person = person }, Cmd.none )

        GetPerson (Err _) ->
            model ! []

        TranslatePerson language ->
            ( { model | language = language }, getPersonInLanguage language model.person )


queryResultDecoder : Language -> Decoder QueryResult
queryResultDecoder language =
    decode QueryResult
        |> required "count" int
        |> required "results" (list (personDecoder language))


personDecoder : Language -> Decoder Person
personDecoder language =
    case language of
        English ->
            decode Person
                |> required "url" string
                |> required "name" string
                |> required "gender" string
                |> required "homeworld" string
                |> optional "planet" (maybe (planetDecoder English)) Nothing

        Wookiee ->
            decode Person
                |> required "hurcan" string
                |> required "whrascwo" string
                |> required "rrwowhwaworc" string
                |> required "acooscwoohoorcanwa" string
                |> optional "planet" (maybe (planetDecoder Wookiee)) Nothing


planetDecoder : Language -> Decoder Planet
planetDecoder language =
    case language of
        English ->
            decode Planet
                |> required "climate" string

        Wookiee ->
            decode Planet
                |> required "oaanahscraaowo" string


getQueryResult : String -> Task.Task Http.Error QueryResult
getQueryResult name =
    let
        url =
            getLanguageUrl English "http://swapi.co/api/people/" ++ "&search=" ++ name
    in
        Http.get url (queryResultDecoder English)
            |> Http.toTask


getFirstPerson : QueryResult -> Maybe Person
getFirstPerson { people } =
    List.head people


getPlanet : Language -> Maybe Person -> Task.Task Http.Error (Maybe Person)
getPlanet language maybePerson =
    case maybePerson of
        Just person ->
            Http.get (getLanguageUrl language person.planetUrl) (planetDecoder language)
                |> Http.toTask
                |> Task.map (\x -> Just ({ person | planet = (Just x) }))

        Nothing ->
            Task.succeed Nothing


queryPerson : String -> Cmd Msg
queryPerson name =
    getQueryResult name
        |> Task.map getFirstPerson
        |> Task.andThen (getPlanet English)
        |> Task.attempt GetPerson


getPersonInLanguage : Language -> Maybe Person -> Cmd Msg
getPersonInLanguage language maybePerson =
    case maybePerson of
        Just person ->
            let
                languageUrl =
                    getLanguageUrl language person.url

                personInLanguageTask =
                    Http.get languageUrl (personDecoder language)
                        |> Http.toTask

                personPlanetInLanguageTask =
                    Http.get (getLanguageUrl language person.planetUrl) (planetDecoder language)
                        |> Http.toTask
                        |> Task.map (\x -> { person | planet = Just x })
            in
                Task.sequence [ personInLanguageTask, personPlanetInLanguageTask ]
                    |> Task.map
                        (\tasks ->
                            case tasks of
                                [ personInLanguage, { planet } ] ->
                                    Just (translateFields personInLanguage planet person)

                                _ ->
                                    Nothing
                        )
                    |> Task.attempt GetPerson

        Nothing ->
            Task.succeed Nothing
                |> Task.attempt GetPerson


getLanguageUrl : Language -> String -> String
getLanguageUrl language url =
    let
        format =
            case language of
                English ->
                    ""

                Wookiee ->
                    "wookiee"
    in
        url ++ "?format=" ++ format


translateFields : Person -> Maybe Planet -> Person -> Person
translateFields personInLanguage planetInLanguage person =
    { person
        | name = personInLanguage.name
        , gender = personInLanguage.gender
        , planet = planetInLanguage
    }


translateButton : Maybe Person -> Language -> Html Msg
translateButton maybePerson fromLanguage =
    case maybePerson of
        Just person ->
            let
                toLanguage =
                    case fromLanguage of
                        English ->
                            Wookiee

                        Wookiee ->
                            English
            in
                input
                    [ type_ "button"
                    , toString toLanguage |> String.append "Translate To " |> value
                    , TranslatePerson toLanguage |> onClick
                    ]
                    []

        Nothing ->
            text ""


view : Model -> Html Msg
view model =
    div
        []
        [ div
            []
            [ input
                [ type_ "text", onInput UpdateQuery ]
                []
            , input
                [ type_ "button", value "Query", onClick QueryPerson ]
                []
            , translateButton model.person model.language
            ]
        , div
            []
            []
        , model |> toString |> text
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
