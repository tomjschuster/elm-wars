module Main exposing (..)

import Html exposing (Html, div, input, button, text, i, table, tr, td, h1)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (Decoder, maybe, list)
import Json.Decode.Pipeline exposing (required, decode, optional)
import UrlParser exposing ((</>), s, string, parseHash)
import Task


type alias Model =
    { query : String
    , queryResult : QueryResult
    , person : Maybe Person
    , language : Language
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" (emptyQueryResult "") Nothing English
    , Cmd.none
    )


type alias QueryResult =
    { people : List Person
    , queryString : String
    , count : Int
    , next : String
    , previous : String
    }


emptyQueryResult : String -> QueryResult
emptyQueryResult queryString =
    { people = []
    , queryString = queryString
    , count = 0
    , next = ""
    , previous = ""
    }


type alias Person =
    { url : String
    , name : String
    , gender : String
    , mass : String
    , height : String
    , birthYear : String
    , planetUrl : String
    , planet : Maybe Planet
    }


type alias Planet =
    { name : String
    , climate : String
    }


type Language
    = English
    | Wookiee


type Msg
    = UpdateQuery String
    | QueryPerson
    | GetResults (Result Http.Error QueryResult)
    | SelectResult Person
    | GetPerson (Result Http.Error (Maybe Person))
    | Translate Language


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery query ->
            { model | query = query } ! []

        QueryPerson ->
            let
                cmd =
                    if model.query == "" then
                        Cmd.none
                    else
                        queryByName model.query
            in
                ( { model | person = Nothing }, cmd )

        GetResults (Ok queryResult) ->
            ( { model | queryResult = queryResult }, Cmd.none )

        GetResults (Err _) ->
            ( { model
                | person = Nothing
                , queryResult = emptyQueryResult model.query
              }
            , Cmd.none
            )

        SelectResult person ->
            ( model, getPerson model.language person )

        GetPerson (Ok person) ->
            ( { model | person = person }, Cmd.none )

        GetPerson (Err _) ->
            model ! []

        Translate language ->
            let
                cmd =
                    case model.person of
                        Just person ->
                            getPerson language person

                        Nothing ->
                            Cmd.none
            in
                ( { model | language = language }, cmd )


queryResultDecoder : Language -> Decoder QueryResult
queryResultDecoder language =
    decode QueryResult
        |> required "results" (JD.list (personDecoder language))
        |> optional "queryString" JD.string ""
        |> required "count" JD.int
        |> optional "next" JD.string ""
        |> optional "previous" JD.string ""


personDecoder : Language -> Decoder Person
personDecoder language =
    case language of
        English ->
            decode Person
                |> required "url" JD.string
                |> required "name" JD.string
                |> required "gender" JD.string
                |> required "mass" JD.string
                |> required "height" JD.string
                |> required "birth_year" JD.string
                |> required "homeworld" JD.string
                |> optional "planet" (JD.maybe (planetDecoder English)) Nothing

        Wookiee ->
            decode Person
                |> required "hurcan" JD.string
                |> required "whrascwo" JD.string
                |> required "rrwowhwaworc" JD.string
                |> required "scracc" JD.string
                |> required "acwoahrracao" JD.string
                |> required "rhahrcaoac_roworarc" JD.string
                |> required "acooscwoohoorcanwa" JD.string
                |> optional "planet" (JD.maybe (planetDecoder Wookiee)) Nothing


planetDecoder : Language -> Decoder Planet
planetDecoder language =
    case language of
        English ->
            decode Planet
                |> required "name" JD.string
                |> required "climate" JD.string

        Wookiee ->
            decode Planet
                |> required "whrascwo" JD.string
                |> required "oaanahscraaowo" JD.string


queryByName : String -> Cmd Msg
queryByName name =
    let
        url =
            "http://swapi.co/api/people/?search=" ++ name
    in
        getQueryResults url


getQueryResults : String -> Cmd Msg
getQueryResults url =
    Http.get url (queryResultDecoder English)
        |> Http.toTask
        |> Task.map (\results -> { results | queryString = "" })
        |> Task.attempt GetResults


getPlanet : Language -> Person -> Task.Task Http.Error Person
getPlanet language person =
    Http.get (getLanguageUrl language person.planetUrl) (planetDecoder language)
        |> Http.toTask
        |> Task.map (\x -> ({ person | planet = (Just x) }))


getPerson : Language -> Person -> Cmd Msg
getPerson language person =
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
                button
                    [ class "ui black button"
                    , Translate toLanguage |> onClick
                    ]
                    [ toString toLanguage |> String.append "Translate To " |> text ]

        Nothing ->
            text ""


personCard : Maybe Person -> Html Msg
personCard maybePerson =
    case maybePerson of
        Just person ->
            div
                [ class "ui card" ]
                [ div
                    [ class "content" ]
                    [ div
                        [ class "ui dividing header" ]
                        [ text person.name ]
                    , div
                        [ class "meta" ]
                        [ div
                            [ class "ui description" ]
                            [ table
                                [ class "ui celled table" ]
                                [ tr
                                    []
                                    [ td [] [ text "Name" ], td [] [ text person.name ] ]
                                , tr
                                    []
                                    [ td [] [ text "Gender" ], td [] [ text person.gender ] ]
                                , tr
                                    []
                                    [ td [] [ text "Mass" ], td [] [ person.mass ++ " kg" |> text ] ]
                                , tr
                                    []
                                    [ td [] [ text "Height" ], td [] [ person.height ++ " cm" |> text ] ]
                                , tr
                                    []
                                    [ td [] [ text "Birth Year" ], td [] [ text person.birthYear ] ]
                                , tr
                                    []
                                    [ td [] [ text "Planet" ], td [] [ planetText person.planet |> text ] ]
                                ]
                            ]
                        ]
                    ]
                ]

        Nothing ->
            text ""


resultsRow : Person -> Html Msg
resultsRow person =
    button [ class "ui button attached", SelectResult person |> onClick ] [ text person.name ]


resultsList : QueryResult -> Html Msg
resultsList queryResult =
    let
        description =
            if queryResult.queryString == "" then
                ""
            else
                toString queryResult.count
                    ++ " found for \""
                    ++ queryResult.queryString
                    ++ "\""
    in
        div
            [ class "ui card" ]
            ((div [ class "content" ] [ div [ class "header" ] [ text "Results" ] ])
                :: (div [ class "content" ] [ div [ class "description" ] [ text description ] ])
                :: List.map resultsRow queryResult.people
            )


planetText : Maybe Planet -> String
planetText maybePlanet =
    case maybePlanet of
        Just planet ->
            String.concat [ planet.name, " (", planet.climate, ")" ]

        Nothing ->
            ""


view : Model -> Html Msg
view model =
    div
        [ class "ui container" ]
        [ div
            [ class "item" ]
            [ h1 [ class "ui dividing header" ] [ text "ElmWars" ]
            , div
                [ class "ui action left icon input" ]
                [ i [ class "search icon" ] []
                , input
                    [ type_ "text", onInput UpdateQuery, value model.query ]
                    []
                , button
                    [ class "ui button", onClick QueryPerson ]
                    [ text "Query" ]
                , translateButton model.person model.language
                ]
            ]
        , div [ class "ui divider" ] []
        , div [ class "ui two cards" ]
            [ resultsList model.queryResult
            , personCard model.person
            ]
        , div [ class "ui divider" ] []
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
