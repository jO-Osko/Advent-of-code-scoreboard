module App exposing (Msg(..), main, update, view)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Browser exposing (element)
import DateTime exposing (DateTime)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Models exposing (..)


main : Program () Model Msg
main =
    element
        { init =
            \_ ->
                ( initialModel
                , Cmd.batch [ initData ]
                )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


initData =
    Http.get { url = "static/data.json", expect = Http.expectJson GotJson dataDecoder }


dataDecoder : Decoder (List User)
dataDecoder =
    Decode.list decodeUser



--decodeUserOld : Decoder User
{--
decodeUserOld =
    Decode.succeed User
        |> required "name" Decode.string
        |> required "surname" Decode.string
        |> optional "github_link" (Decode.map Just Decode.string) Nothing
        |> optional "github_repo_link" (Decode.map Just Decode.string) Nothing
        |> optional "aoc_id" (Decode.map Just Decode.int) Nothing
        |> required "solution_data"
            (Decode.andThen
                (\a ->
                    Decode.succeed
                        (Dict.fromList
                            (List.map
                                (\( f, s ) ->
                                    let
                                        _ =
                                            Debug.log "Log" s
                                    in
                                    ( Maybe.withDefault 0 (String.toInt f), s )
                                )
                                (Dict.toList a)
                            )
                        )
                )
                (Decode.dict decodeDictDay)
            )
--}


decodeUser : Decoder User
decodeUser =
    Decode.andThen
        (\usr ->
            let
                sumSome =
                    \getter ->
                        List.foldl assigment_plus zeroPoints (List.map getter (Dict.values usr.dayResults))

                bestPoints =
                    sumSome .bestPoints

                confirmedSolutionPoints =
                    sumSome .confirmedSolutionPoints
            in
            Decode.succeed { usr | bestPoints = bestPoints, confirmedSolutionPoints = confirmedSolutionPoints }
        )
        (Decode.succeed User
            |> required "name" Decode.string
            |> required "surname" Decode.string
            |> optional "github_link" (Decode.map Just Decode.string) Nothing
            |> optional "github_repo_link" (Decode.map Just Decode.string) Nothing
            |> optional "aoc_id" (Decode.map Just Decode.int) Nothing
            |> hardcoded invalidPoints
            |> hardcoded invalidPoints
            |> required "solution_data"
                (Decode.andThen
                    (\a ->
                        Decode.succeed
                            (Dict.fromList
                                (List.map
                                    (\( f, s ) ->
                                        let
                                            day =
                                                Maybe.withDefault 0 (String.toInt f)
                                        in
                                        ( day, { s | day = day } )
                                    )
                                    (Dict.toList a)
                                )
                            )
                    )
                    (Decode.dict decodeNekej)
                )
        )


decodeNekej : Decoder DayResult
decodeNekej =
    Decode.andThen
        (\dict ->
            let
                day1 =
                    Maybe.map (assigmentFromIntermediate >> score) (Dict.get "1" dict)

                day2 =
                    Maybe.map (assigmentFromIntermediate >> score) (Dict.get "2" dict)

                ( confirmed, best ) =
                    scoreInfo day1 day2
            in
            Decode.succeed { star1 = day1, star2 = day2, bestPoints = best, confirmedSolutionPoints = confirmed, day = 0 }
        )
        (Decode.dict decodeDayResult)



--    { star1 = Nothing, star2 = Nothing, bestPoints = zeroPoints, confirmedSolutionPoints = zeroPoints, day = 0 }


decodeBurek : Decoder DayResult
decodeBurek =
    Decode.succeed (DayResult Nothing Nothing zeroPoints zeroPoints 0)



{--
decodeDictDay : Decoder (Dict.Dict Int String)
decodeDictDay =
    Decode.succeed Dict.fromList
        |> Decode.dict decodeDayResult
--}


decodeDayResult : Decoder IntermediateDayResult
decodeDayResult =
    Decode.succeed IntermediateDayResult
        |> optional "language" (Decode.map Just Decode.string) Nothing
        |> required "solution_confirmed" Decode.bool
        |> required "solved" Decode.bool
        |> optional "solved_time" (Decode.map Just Decode.int) Nothing



-- { star1 = Nothing, star2 = Nothing, bestPoints = zeroPoints, confirmedSolutionPoints = zeroPoints, day = 0 }


type Msg
    = Refresh
    | GotJson (Result Http.Error (List User))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( model, Cmd.batch [ initData ] )

        GotJson x ->
            let
                users =
                    case x of
                        Ok r ->
                            r

                        Err err ->
                            let
                                _ =
                                    Debug.log "Error" err
                            in
                            []
            in
            ( { model | users = users }, Cmd.none )


view : Model -> Html Msg
view model =
    Grid.container []
        ([ Html.h1 [] [ text "Sprotni rezultati točkovanj advent of code " ]
         , Html.h3 [] [ a [ href "https://adventofcode.com/2020/leaderboard/private/view/7040", target "_blank" ] [ text "https://adventofcode.com/2020/leaderboard/private/view/7040" ] ]
         , Grid.row [ Row.attrs [ class "font-weight-bold" ] ]
            [ Grid.col [] [ text "Ime" ]
            , Grid.col [] [ text "Github" ]
            , Grid.col [] [ text "Točke" ]
            , Grid.col [] [ text "Preverjene točke" ]
            ]
         ]
            ++ List.map
                (\user ->
                    Grid.row []
                        [ Grid.col [] [ text (user.name ++ " " ++ user.surname) ]
                        , Grid.col []
                            [ case user.githubLink of
                                Nothing ->
                                    text ""

                                Just link ->
                                    a [ href link, target "_blank" ] [ text "link" ]
                            ]
                        , Grid.col [] [ text (showAssigmentPoint user.bestPoints) ]
                        , Grid.col [] [ text (showAssigmentPoint user.confirmedSolutionPoints) ]
                        ]
                )
                (List.reverse (List.sortBy (assigmentToInt << .bestPoints) (List.filter (\user -> user.name /= "Filip") model.users)))
        )



-- elm-live src/App.elm --open -- --debug --output=js/app/app.js
