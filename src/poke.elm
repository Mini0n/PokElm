module Poke exposing (main)

-- HTML
import Html exposing (..)
import Html.Attributes exposing (class, id, classList)
-- App
import Browser
-- HTTP | elm install elm/http
import Http
-- JSON | elm install NoRedInk/elm-json-decode-pipeline
import Json.Decode exposing (Decoder, bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)



type alias Pokemon = {
    number : Int,
    name   : String,
    sprite : String,
    data   : PokeData
}

type alias PokeData = {
    number     : Int,
    experience : Int,
    type       : PokeType,
    move       : PokeMove
}

type alias PokeType = {
    type1 : String,
    type2 : String
}

type alias PokeMove = {
    move1 : String,
    move2 : String,
    move3 : String,
    move4 : String
}

view model =
    div []
        [ text "PokElm"
        ]


main =
    view "no model yet"
