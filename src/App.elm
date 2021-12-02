module App exposing (Msg(..), main, update, view)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Browser exposing (element)
import DateTime exposing (DateTime)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, href, style, target)
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
    | SelectUser (Maybe User)


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

        SelectUser maybeUser ->
            ( { model | selectedUser = maybeUser }, Cmd.none )


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
            ++ List.concat
                (List.map
                    (\user ->
                        [ Grid.row [ Row.attrs [ onClick (SelectUser (Just user)) ] ]
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
                        ]
                            ++ userDetailRow user model.selectedUser
                    )
                    (model.users
                        |> List.sortBy (assigmentToInt << .bestPoints)
                        |> List.reverse
                     {--|> List.filter (\user -> user.aocId /= Just 7040) --}
                    )
                )
        )


userDetailRow : User -> Maybe User -> List (Html Msg)
userDetailRow user selectedUser =
    case selectedUser of
        Nothing ->
            []

        Just u ->
            if user == u then
                [ fullUserDetailRow user ]

            else
                []


fullUserDetailRow : User -> Html msg
fullUserDetailRow user =
    let
        joinedResults =
            removePairs
                ((List.map (\( d, u ) -> ( d, Just u )) (Dict.toList user.dayResults) ++ List.map (\x -> ( x, Nothing )) (List.range 1 25))
                    |> List.sortBy (\( x, _ ) -> x)
                )

        removePairs l =
            case l of
                ( d1, v1 ) :: ( d2, v2 ) :: xs ->
                    if d1 == d2 then
                        let
                            rest =
                                removePairs xs
                        in
                        case ( v1, v2 ) of
                            ( Just v, _ ) ->
                                ( d1, Just v ) :: rest

                            ( _, Just v ) ->
                                ( d2, Just v ) :: rest

                            _ ->
                                rest

                    else
                        ( d1, v1 ) :: removePairs (( d2, v2 ) :: xs)

                xs ->
                    xs
    in
    Grid.row []
        (List.map
            showDayCol
            joinedResults
        )


getColor : AssigmentResult -> String
getColor s =
    if s.solved then
        "green"

    else
        "silver"


showStar : Int -> Maybe ScoredAssigmentResult -> Grid.Column msg
showStar day mScAsRe =
    case mScAsRe of
        Nothing ->
            Grid.col [ Col.attrs [ style "background-color" "red" ] ] [ text "0" ]

        Just s ->
            case
                s.assigmentResult.solved
            of
                True ->
                    case isSoonEnough day s.assigmentResult.solvedTime of
                        True ->
                            Grid.col [ Col.attrs [ style "background-color" "green" ] ] [ text "0.5" ]

                        False ->
                            Grid.col [ Col.attrs [ style "background-color" "orange" ] ] [ text "prepozno" ]

                False ->
                    Grid.col [ Col.attrs [ style "background-color" "red" ] ] [ text "0" ]


startTime =
    1638334800


dayTimestamp =
    24 * 60 * 60


isSoonEnough : Int -> Int -> Bool
isSoonEnough dayNum solvedTime =
    startTime + (dayNum + 1) * dayTimestamp >= solvedTime


showDayCol : ( Int, Maybe DayResult ) -> Grid.Column msg
showDayCol ( day, results ) =
    case results of
        Nothing ->
            Grid.col [ Col.md2 ] [ text <| "Dan:" ++ String.fromInt day ]

        Just r ->
            Grid.col [ Col.md2 ]
                [ text <| "Dan:" ++ String.fromInt day ++ " " ++ showAssigmentPoint r.bestPoints
                , Grid.row [] [ showStar day r.star1 ]
                , Grid.row [] [ showStar day r.star2 ]
                ]



-- elm-live src/App.elm --open -- --debug --output=js/app/app.js
