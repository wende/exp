module Exp exposing
    ( Bound(..)
    , DNumber(..)
    , Domain(..)
    , DomainError(..)
    , ListChunk(..)
    , Meta
    , NumOrInf(..)
    , WithMeta
    , defaultMeta
    , solveDomains
    )

-- 1. Can n % 2 == 0 branch be expressed? (Non standard step f)
-- 2. Print a code of the function by its name


type alias Meta =
    { id : Int
    , line : Int
    }


type DomainError
    = DomainError String (List Meta)
    | NotImplementedYet (List Meta)


{-| Expresses a number or an infinity (used for range)
-}
type NumOrInf n
    = Infinity
    | MinusInfinity
    | Num n


type Bound x
    = Open x
    | Closed x


{-| Number domain
-}
type DNumber n
    = ExactNumber n
    | Range (Bound (NumOrInf n)) (Bound (NumOrInf n))
    | AnyNumber


type ListChunk n
    = ExactChunk (List n)
    | InexactChunk (DNumber Int) Domain


type alias WithMeta =
    { meta : Meta, domain : Domain }


type Domain
    = Any
    | Or (List WithMeta)
    | And (List WithMeta)
    | Not WithMeta
      -- DChar is DInt (Range (Closed 0) (Closed 255))?
    | DInt (DNumber Int)
    | DFloat (DNumber Float)
    | DList (List (ListChunk WithMeta))
    | DBinary (List (ListChunk DNumber))
      -- DBoolean is Or (DAtom _ 4 "true") (DAtom _ 5 "false")?
    | DAtom (DNumber Int) (DNumber Int)
    | DTuple (DNumber Int) WithMeta
    | DPid ( DNumber Int, DNumber Int, DNumber Int )
    | DReference
      -- | DFunction
      -- | DStruct
    | Self Int


defaultMeta : Int -> Meta
defaultMeta id =
    { id = id, line = 0 }


solveDomains : WithMeta -> WithMeta -> Result DomainError WithMeta
solveDomains d1 d2 =
    case ( d1.domain, d1.meta, d2.domain, d2.meta ) of
        -- Merge Any
        ( Any, meta, _, _ ) ->
            Ok <| WithMeta meta Any

        ( _, _, Any, meta ) ->
            Ok <| WithMeta meta Any

        -- Merge Or / And
        ( Or left, meta, Or right, _ ) ->
            Ok <| WithMeta meta <| Or (left ++ right)

        ( And left, meta, And right, _ ) ->
            Ok <| WithMeta meta <| And (left ++ right)

        -- Int comparison
        ( DInt left, lm, DInt right, rm ) ->
            solveDNumber lm left rm right
                |> Result.map (WithMeta lm << DInt)

        -- Not implemented yet
        ( left, lm, right, rm ) ->
            Err <| NotImplementedYet [ lm, rm ]



-- Err <| DomainError ("Could not unify:\n\n    " ++ toString left ++ "\n\nand\n\n    " ++ toString right) [ lm, rm ]


solveDNumber : Meta -> DNumber t -> Meta -> DNumber t -> Result DomainError (DNumber t)
solveDNumber metaL l metaR r =
    case ( l, r ) of
        ( ExactNumber x, ExactNumber y ) ->
            if x == y then
                Ok <| ExactNumber x

            else
                Err <| DomainError (toString x ++ " and " ++ toString y ++ "are different") [ metaL, metaR ]

        _ ->
            Err <| NotImplementedYet [ metaL, metaR ]


query : WithMeta -> WithMeta -> Bool
query l r =
    solveDomains l r |> Result.toMaybe |> (/=) Nothing



-- Private functions


onAllSuccess : List (Result a b) -> Result a (List b)
onAllSuccess list =
    case list of
        [] ->
            Ok []

        (Err e) :: _ ->
            Err e

        (Ok a) :: rest ->
            Result.map (flip (++) [ a ]) (onAllSuccess rest)
