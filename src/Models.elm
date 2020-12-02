module Models exposing (AssigmentPoint, AssigmentResult, DayResult, IntermediateDayResult, Model, ModelStatus(..), ScoredAssigmentResult, User, assigmentFromIntermediate, assigmentToInt, assigment_plus, initialModel, invalidPoints, score, scoreInfo, showAssigmentPoint, zeroPoints)

import DateTime exposing (DateTime)
import Dict exposing (Dict)



-- 1/8 of exam point


halfPoint : number
halfPoint =
    1


fullPoint : number
fullPoint =
    halfPoint + halfPoint


pointMultiplier : number
pointMultiplier =
    fullPoint


type AssigmentPoint
    = AssigmentPoint Int


invalidPoints : AssigmentPoint
invalidPoints =
    AssigmentPoint -1


zeroPoints : AssigmentPoint
zeroPoints =
    AssigmentPoint 0


starAcquiredPoint : AssigmentPoint
starAcquiredPoint =
    AssigmentPoint halfPoint


maxPoints =
    10.0


showAssigmentPoint : AssigmentPoint -> String
showAssigmentPoint (AssigmentPoint p) =
    (min (toFloat p / pointMultiplier) maxPoints
        |> String.fromFloat
    )
        ++ " Točk"


assigment_plus : AssigmentPoint -> AssigmentPoint -> AssigmentPoint
assigment_plus (AssigmentPoint p1) (AssigmentPoint p2) =
    AssigmentPoint (p1 + p2)


assigmentToInt (AssigmentPoint p) =
    p


score : AssigmentResult -> ScoredAssigmentResult
score assigmentResult =
    let
        ( bestPoints, confirmedSolutionPoints ) =
            case assigmentResult.solved of
                False ->
                    ( zeroPoints, zeroPoints )

                True ->
                    if assigmentResult.solutionConfirmed then
                        ( starAcquiredPoint, starAcquiredPoint )

                    else
                        ( starAcquiredPoint, zeroPoints )
    in
    { assigmentResult = assigmentResult
    , bestPoints = bestPoints
    , confirmedSolutionPoints = confirmedSolutionPoints
    }


type alias AssigmentResult =
    { solutionConfirmed : Bool
    , solved : Bool
    , solvedTime : Int
    }


assigmentFromIntermediate : IntermediateDayResult -> AssigmentResult
assigmentFromIntermediate inter =
    { solutionConfirmed = inter.solutionConfirmed, solved = inter.solved, solvedTime = Maybe.withDefault 0 inter.solvedTime }


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
    , selectedUser : Maybe User
    }


initialModel : Model
initialModel =
    { users = []
    , status = Stable
    , dataS = ""
    , selectedUser = Nothing
    }
