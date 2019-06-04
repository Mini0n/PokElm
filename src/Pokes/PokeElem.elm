module PokeElem exposing (PokeElem, pokeElemDecoder, pokeElemListDecoder, pokeElemListURL, pokeElemListView)

-- HTML Imports

import Array
import Browser
import Html exposing (Html, a, b, br, div, h2, img, input, pre, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver)
import Json.Decode as JD exposing (Decoder, field, float, int, list, string)
import Json.Decode.Pipeline exposing (optional)
import PokeData as PData exposing (..)



-- import View
-- TYPES


type alias PokeElem =
    { num : String -- number
    , nom : String -- name
    , sta : String -- stamina
    , atk : String -- attack
    , def : String -- defense
    , cpM : String -- max CP
    , tp1 : String -- type 1
    , tp2 : String -- type 2
    , kms : String -- buddy km
    , egg : String -- egg km
    , cdy : String -- candies for evolution
    , img : String -- pic (35x32px)
    , alo : Bool --   Alolan?
    , cls : PokeClassType -- class [normal, legendary, mythic]

    -- Missing:
    -- evolutions & movements [primary & secondary]
    -- weaknesses, strength-againts
    -- rarity
    }


type alias PokeElemList =
    { fullList : List PokeElem -- Full PokeElem List
    , fltrList : List PokeElem -- Filtered (pokeSearchStr) PokeElem List
    }



-- type Msg
--     = PokeElemViewClick
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
        |> optional "cpM" string ""
        |> optional "field_pokemon_type" (JD.map (\types -> pokeType types False) string) ""
        |> optional "field_pokemon_type" (JD.map (\types -> pokeType types True) string) ""
        |> optional "buddy" string ""
        |> optional "hatch" string ""
        |> optional "candy" string ""
        |> optional "image" (JD.map pokeThumb string) ""
        |> optional "title_1" (JD.map pokeAlolan string) False
        |> optional "pokemon_class" (JD.map pokeClassType string) PokeClassNrml



-- HELPERS (Views)


pokeElemView : PokeElem -> String -> Html msg
pokeElemView poke pokeSearchStr =
    a
        [ class "list-group-item list-group-item-action justify-content-between align-items-center"
        , style "display"
            (if pokeSearch poke pokeSearchStr then
                ""

             else
                "none"
            )
        ]
        [ img [ class "rounded", src poke.img ] []
        , text poke.nom
        , div []
            [ -- ,
              span [ class "badge badge-secondary mr-1" ]
                [ text poke.tp1 ]
            , span [ class "badge badge-secondary" ]
                [ text poke.tp2 ]
            ]
        ]


pokeElemListView : List PokeElem -> String -> List (Html msg)
pokeElemListView pokes pokeSearchStr =
    List.map (\poke -> pokeElemView poke pokeSearchStr) pokes



-- HELPERS


pokeType : String -> Bool -> String
pokeType pokeTypes secondType =
    let
        index =
            if secondType then
                1

            else
                0

        typesArray =
            Array.fromList (String.split "," (String.replace " " "" pokeTypes))
    in
    case Array.get index typesArray of
        Just ptype ->
            ptype

        Nothing ->
            ""


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


pokeToString : PokeElem -> String
pokeToString poke =
    "number:"
        ++ poke.num
        ++ "name:"
        ++ poke.nom
        ++ "sta:"
        ++ poke.sta
        ++ "def:"
        ++ poke.def
        ++ "atk:"
        ++ poke.atk
        ++ "type:"
        ++ poke.tp1
        ++ "type:"
        ++ poke.tp2
        ++ "kms:"
        ++ poke.kms
        ++ "kms:"
        ++ poke.egg
        ++ "egg:"
        ++ poke.egg
        ++ "candy:"
        ++ poke.cdy
        ++ "type:"
        ++ pokeClass poke.cls



-- ++ "class" ++ ( pokeClass (poke.cls)
-- class (normal, legen, myth)
-- cp


pokeClass : PokeClassType -> String
pokeClass poke =
    case poke of
        PokeClassMyth ->
            "Mythical"

        PokeClassLgnd ->
            "Legendary"

        _ ->
            "Normal"


pokeSearch : PokeElem -> String -> Bool
pokeSearch poke searchString =
    let
        searchable =
            String.toLower (String.replace " " "" (pokeToString poke))
    in
    String.contains searchString searchable
