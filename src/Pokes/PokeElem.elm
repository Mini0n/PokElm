module PokeElem exposing (PokeElem, pokeElemDecoder, pokeElemListDecoder)

import Json.Decode as JD exposing (Decoder, field, float, int, list, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)


type alias PokeElem =
    { num : String -- number
    , nom : String -- name
    , sta : String -- stamina
    , atk : String -- attack
    , def : String -- defense
    , cpM : String -- max CP
    , typ : String -- type
    , cls : String -- class [normal, legendary, mythic]
    , kms : String -- buddy km
    , egg : String -- egg km
    , can : String -- candies for evolution
    , img : String -- pic (35x32px)
    }



-- DECODERS


pokeElemListDecoder : Decoder (List PokeElem)
pokeElemListDecoder =
    JD.list pokeElemDecoder


pokeElemDecoder : Decoder PokeElem
pokeElemDecoder =
    JD.succeed PokeElem
        |> optional "num" string ""
        |> optional "nom" string ""
        |> optional "sta" string ""
        |> optional "atk" string ""
        |> optional "def" string ""
        |> optional "cpM" string ""
        |> optional "typ" string ""
        |> optional "cls" string ""
        |> optional "bud" string ""
        |> optional "egg" string ""
        |> optional "can" string ""
        |> optional "img" string ""



-- pic (35x32px)
