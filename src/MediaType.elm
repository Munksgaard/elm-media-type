module MediaType exposing
    ( MediaType, Type(..)
    , fromString, parser
    )

{-| Parse and handle media types in Elm.


# Types

@docs MediaType, Type


# Parsing

@docs fromString, parser

-}

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , backtrackable
        , chompUntil
        , chompUntilEndOr
        , chompWhile
        , commit
        , getChompedString
        , keyword
        , loop
        , map
        , oneOf
        , run
        , spaces
        , succeed
        , symbol
        , variable
        )
import Set


{-| A media type.
-}
type alias MediaType =
    { type_ : Type
    , registrationTree : Maybe String
    , subtype : String
    , suffix : Maybe String
    , parameters : Dict String String
    }


{-| Media type _types_.

I didn't pick the name...

Based on the list from <https://en.wikipedia.org/w/index.php?title=Media_type&oldid=881155761#Naming>

-}
type Type
    = Application
    | Audio
    | Example
    | Font
    | Image
    | Message
    | Model
    | Multipart
    | Text
    | Video


type_ : Parser Type
type_ =
    oneOf
        [ map (always Application) (keyword "application")
        , map (always Audio) (keyword "audio")
        , map (always Example) (keyword "example")
        , map (always Font) (keyword "font")
        , map (always Image) (keyword "image")
        , map (always Message) (keyword "message")
        , map (always Model) (keyword "model")
        , map (always Multipart) (keyword "multipart")
        , map (always Text) (keyword "text")
        , map (always Video) (keyword "video")
        ]


registrationTree : Parser String
registrationTree =
    backtrackable
        (variable
            { start = Char.isAlphaNum
            , inner = (/=) '.'
            , reserved = Set.empty
            }
            |. symbol "."
        )
        |> andThen commit


subtype : Parser String
subtype =
    variable
        { start = Char.isAlphaNum
        , inner = not << flip List.member [ ',', '+', ';' ]
        , reserved = Set.empty
        }


{-| Media types can be a part of another data schema, like the [data URI scheme](https://en.wikipedia.org/wiki/Data_URI_scheme), so it can be helpful to access the internal parser.
-}
parser : Parser MediaType
parser =
    succeed MediaType
        |= type_
        |. symbol "/"
        |= oneOf
            [ map Just registrationTree
            , succeed Nothing
            ]
        |= subtype
        |= oneOf
            [ map Just <|
                succeed identity
                    |. symbol "+"
                    |= getChompedString (chompUntilEndOr ";")
            , succeed Nothing
            ]
        |= loop Dict.empty parameters


parameters : Dict String String -> Parser (Step (Dict String String) (Dict String String))
parameters dict =
    oneOf
        [ succeed (\k v -> Loop (Dict.insert k v dict))
            |. backtrackable spaces
            |. backtrackable (symbol ";")
            |. backtrackable spaces
            |= backtrackable (getChompedString (chompUntil "="))
            |. symbol "="
            |= getChompedString (chompWhile (not << flip List.member [ ',', ';' ]))
        , succeed ()
            |> map (always <| Done dict)
        ]


{-| Attempt to parse a string as a media type.
-}
fromString : String -> Maybe MediaType
fromString s =
    Parser.run parser s
        |> Result.toMaybe
