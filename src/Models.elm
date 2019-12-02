module Models exposing (AssigmentPoint, AssigmentResult, DayResult, IntermediateDayResult, Language(..), Model, ModelStatus(..), ScoredAssigmentResult, User, assigmentFromIntermediate, assigmentToInt, assigment_plus, functionalPoint, goldStartAcquiredPoint, initialModel, invalidPoints, isFunctional, normalPoint, score, scoreInfo, showAssigmentPoint, silverStartAcquiredPoint, zeroPoints)

import DateTime exposing (DateTime)
import Dict exposing (Dict)



-- 1/8 of exam point


type AssigmentPoint
    = AssigmentPoint Int


invalidPoints : AssigmentPoint
invalidPoints =
    AssigmentPoint -1


zeroPoints : AssigmentPoint
zeroPoints =
    AssigmentPoint 0


normalPoint : AssigmentPoint
normalPoint =
    AssigmentPoint 1


functionalPoint : AssigmentPoint
functionalPoint =
    AssigmentPoint 2


silverStartAcquiredPoint : AssigmentPoint
silverStartAcquiredPoint =
    AssigmentPoint 4


goldStartAcquiredPoint : AssigmentPoint
goldStartAcquiredPoint =
    AssigmentPoint (8 + 4)


showAssigmentPoint : AssigmentPoint -> String
showAssigmentPoint (AssigmentPoint p) =
    String.fromFloat (toFloat p / 8) ++ " Točk"


assigment_plus : AssigmentPoint -> AssigmentPoint -> AssigmentPoint
assigment_plus (AssigmentPoint p1) (AssigmentPoint p2) =
    AssigmentPoint (p1 + p2)


assigmentToInt (AssigmentPoint p) =
    p


type Language
    = Unset
    | Unknown
    | Python
    | OCaml
    | Haskell


isFunctional : Language -> Bool
isFunctional lang =
    case lang of
        OCaml ->
            True

        Haskell ->
            True

        _ ->
            False


score : AssigmentResult -> ScoredAssigmentResult
score assigmentResult =
    let
        ( bestPoints, confirmedSolutionPoints ) =
            case assigmentResult.solved of
                False ->
                    ( zeroPoints, zeroPoints )

                True ->
                    let
                        fPart =
                            if isFunctional assigmentResult.language then
                                functionalPoint

                            else
                                normalPoint
                    in
                    if assigmentResult.solutionConfirmed then
                        ( fPart, fPart )

                    else
                        ( fPart, zeroPoints )
    in
    { assigmentResult = assigmentResult
    , bestPoints = bestPoints
    , confirmedSolutionPoints = confirmedSolutionPoints
    }


type alias AssigmentResult =
    { language : Language
    , solutionConfirmed : Bool
    , solved : Bool
    , solvedTime : Maybe Int
    }


languageFromString : Maybe String -> Language
languageFromString str =
    case str of
        Nothing ->
            Unset

        Just s ->
            case String.toLower s of
                "ocaml" ->
                    OCaml

                "python" ->
                    Python

                "haskell" ->
                    Haskell

                _ ->
                    Unknown


assigmentFromIntermediate : IntermediateDayResult -> AssigmentResult
assigmentFromIntermediate inter =
    { language = languageFromString inter.language, solutionConfirmed = inter.solutionConfirmed, solved = inter.solved, solvedTime = inter.solvedTime }


scoreInfo : Maybe ScoredAssigmentResult -> Maybe ScoredAssigmentResult -> ( AssigmentPoint, AssigmentPoint )
scoreInfo sA1 sA2 =
    case ( sA1, sA2 ) of
        ( Nothing, Nothing ) ->
            ( zeroPoints, zeroPoints )

        ( Just s1, Nothing ) ->
            ( s1.confirmedSolutionPoints, s1.bestPoints )

        ( Nothing, Just s2 ) ->
            ( s2.confirmedSolutionPoints, s2.bestPoints )

        ( Just s1, Just s2 ) ->
            ( assigment_plus s1.confirmedSolutionPoints s2.confirmedSolutionPoints
            , assigment_plus s1.bestPoints s2.bestPoints
            )


type alias ScoredAssigmentResult =
    { assigmentResult : AssigmentResult
    , bestPoints : AssigmentPoint -- Without considering confirmations
    , confirmedSolutionPoints : AssigmentPoint
    }


type alias DayResult =
    { star1 : Maybe ScoredAssigmentResult
    , star2 : Maybe ScoredAssigmentResult
    , bestPoints : AssigmentPoint
    , confirmedSolutionPoints : AssigmentPoint
    , day : Int
    }


type alias User =
    { name : String
    , surname : String
    , githubLink : Maybe String
    , githubRepoLink : Maybe String
    , aocId : Maybe Int
    , bestPoints : AssigmentPoint
    , confirmedSolutionPoints : AssigmentPoint
    , dayResults : Dict Int DayResult
    }


type alias IntermediateDayResult =
    { language : Maybe String
    , solutionConfirmed : Bool
    , solved : Bool
    , solvedTime : Maybe Int
    }


type ModelStatus
    = Refreshing
    | Stable


type alias Model =
    { users : List User
    , status : ModelStatus
    , dataS : String
    }


initialModel : Model
initialModel =
    { users = []
    , status = Stable
    , dataS = ""
    }
