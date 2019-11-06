module PokeInit exposing (PokeInit, PokeInitList, decoderPokeInit)

import Json.Decode as Decoder



-- https://gamepress.gg/sites/default/files/aggregatedjson/pokemon-data-full-en-PoGO.json
-- PokeInit


type alias PokeInit =
    { numb : Int
    , name : String
    , icon : String
    }


type alias PokeInitList =
    List PokeInit



-- JSON


decoderPokeInit : Decoder.Decoder PokeInit
decoderPokeInit =
    Decoder.map3 PokeInit
        (Decoder.field "number" Decoder.int)
        (Decoder.field "title_1" Decoder.string)
        (Decoder.field "image" Decoder.string)



-- decoderPokeInitList : Decoder.Decoder (List PokeInit)
-- decoderPokeInitList =
--     Decoder.map (List decoderPokeInit)
-- HELPERS
