module PokeElem exposing (PokeElem, pokeElemDecoder, pokeElemListDecoder, pokeElemListURL)

import Json.Decode as JD exposing (Decoder, field, float, int, list, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)



-- TYPES


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
    , cdy : String -- candies for evolution
    , img : String -- pic (35x32px)
    , alo : Bool -- Alolan?
    -- missing: evolutions & movements [primary & secondary]
    }



-- DECODERS


pokeElemListDecoder : Decoder (List PokeElem)
pokeElemListDecoder =
    JD.list pokeElemDecoder


pokeElemDecoder : Decoder PokeElem
pokeElemDecoder =
    JD.succeed PokeElem
        |> optional "number" string ""
        |> optional "title_1" string ""
        |> optional "sta" string ""
        |> optional "atk" string ""
        |> optional "def" string ""
        |> optional "cp" string ""
        |> optional "field_pokemon_type" string ""
        |> optional "pokemon_class" string ""
        |> optional "buddy" string ""
        |> optional "hatch" string ""
        |> optional "candy" string ""
        |> optional "image" (JD.map pokeThumb string) ""
        |> optional "title_1" (JD.map pokeAlolan string) False



-- HELPERS


pokeThumb : String -> String
pokeThumb imgStr =
    let
        start =
            case List.head (String.indexes "/sites" imgStr) of
                Nothing ->
                    0

                Just num ->
                    num

        end =
            case List.head (String.indexes ".png" imgStr) of
                Nothing ->
                    0

                Just num ->
                    num + 4
    in
    "https://pokemongo.gamepress.gg" ++ String.slice start end imgStr


pokeAlolan : String -> Bool
pokeAlolan name =
    String.contains "alolan" (String.toLower name)


pokeElemListURL : String
pokeElemListURL =
    "https://api.codetabs.com/v1/proxy?quest="
        ++ "https://gamepress.gg/sites/default/files/aggregatedjson/list-en-PoGO.json"
