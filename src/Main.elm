module Main exposing (Msg(..), PokemonFirst, init, main, pokeDecoder, pokeDiv, pokeListDecoder, pokeListDiv, pokeString, subscriptions, update, view)

import Browser
import Html exposing (Html, b, br, div, h2, img, input, pre, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver)
import Http
import Json.Decode as JD exposing (Decoder, field, float, int, list, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { pokeLoadStatus : PokeLoadMsg
    , pokeSearchString : String
    , selectedPokeNum : String
    , selectedPokeImg : String
    , mouseOverPokeNum : String
    , pokeFullLoadStatus : PokeFullLoadMsg
    }


type PokeLoadMsg
    = Failure Http.Error
    | Loading
    | Success (List PokemonFirst)


type PokeFullLoadMsg
    = FailureFull Http.Error
    | LoadingFull
    | SuccessFull PokeFull


type alias PokemonElem =
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


type alias PokemonFirst =
    { num : String
    , name : String
    , img : String
    , sta : String
    , atk : String
    , def : String
    , evo : String
    , typ : String
    , catch_rate : String
    , flee_rate : String
    , prim_atks : String
    , secu_atks : String
    , thumb : String
    }


type alias PokeFull =
    { num : Int -- number
    , nom : String -- name
    , tp1 : String -- type 1
    , tp2 : String -- type 2
    , atk : Int -- attack
    , sta : Int -- stamina
    , def : Int -- defense
    , myt : Int -- mythic?
    , leg : Int -- legendary?
    , gen : Int -- generation
    , can : Int -- candies for evolution
    , kms : Int -- buddy km
    , des : String -- pokedex info (description)
    , wgt : Float -- weight
    , hgt : Float -- height
    , wea : List String -- weather boost
    , cps : PokeFullCPList -- CPs
    }


type alias PokeFullCPList =
    { max : Int
    , wildMax : Int
    , wildMin : Int
    , weathMax : Int
    , weathMin : Int
    , eggMax : Int
    , eggMin : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pokeLoadStatus = Loading
      , pokeSearchString = ""
      , selectedPokeNum = ""
      , selectedPokeImg = ""
      , mouseOverPokeNum = ""
      , pokeFullLoadStatus = LoadingFull
      }
    , Http.get
        { url = pokeProxy ++ "https://gamepress.gg/sites/default/files/aggregatedjson/pokemon-data-full-en-PoGO.json"
        , expect = Http.expectJson GotPokeList pokeListDecoder
        }
    )



-- DECODER


pokeListMsg : Cmd Msg
pokeListMsg =
    Http.get
        { url = pokeProxy ++ "https://gamepress.gg/sites/default/files/aggregatedjson/list-en-PoGO.json"
        , expect = Http.expectJson GotPokeList pokeListDecoder
        }


pokeFullDecoder : Decoder PokeFull
pokeFullDecoder =
    JD.succeed PokeFull
        |> optional "id" int -1
        |> optional "name" string ""
        |> optional "type1" string ""
        |> optional "type2" string ""
        |> optional "atk" int -1
        |> optional "sta" int -1
        |> optional "def" int -1
        |> optional "isMythical" int -1
        |> optional "isLegendary" int -1
        |> optional "generation" int -1
        |> optional "candyToEvolve" int -1
        |> optional "kmBuddyDistance" int -1
        |> optional "description" string ""
        |> optional "weight" float -1.0
        |> optional "height" float -1.0
        |> optional "weatherInfluences" (list string) []
        |> custom (JD.field "CPs" pokeFullCPListDecoder)


pokeFullCPListDecoder : Decoder PokeFullCPList
pokeFullCPListDecoder =
    JD.map7 PokeFullCPList
        (field "max" int)
        (field "wildMax" int)
        (field "wildMin" int)
        (field "weatherMax" int)
        (field "weatherMin" int)
        (field "eggMax" int)
        (field "eggMin" int)



-- pokemonElemDecoder
-- pokemo
-- |> optional "num" string "" -- number
-- |> optional "nom" string "" -- name
-- |> optional "sta" string "" -- stamina
-- |> optional "atk" string "" -- attack
-- |> optional "def" string "" -- defense
-- |> optional "cpM" string "" -- max CP
-- |> optional "typ" string "" -- type
-- |> optional "cls" string "" -- class [normal, legendary, mythic]
-- |> optional "bud" string "" -- buddy km
-- |> optional "egg" string "" -- egg km
-- |> optional "can" string "" -- candies for evolution
-- |> optional "img" string "" -- pic (35x32px)


pokeListDecoder : Decoder (List PokemonFirst)
pokeListDecoder =
    JD.list pokeDecoder


pokeDecoder : Decoder PokemonFirst
pokeDecoder =
    JD.succeed PokemonFirst
        |> required "number" string
        |> required "title_1" string
        |> required "uri" string
        |> required "sta" string
        |> required "atk" string
        |> required "def" string
        |> required "field_evolutions" string
        |> required "field_pokemon_type" string
        |> required "catch_rate" string
        |> required "field_flee_rate" string
        |> required "field_primary_moves" string
        |> required "field_secondary_moves" string
        |> required "pokemon_image_small" string



-- UPDATE


type Msg
    = GotPokeList (Result Http.Error (List PokemonFirst))
    | PokeSearch String
    | PokeDivSelected PokemonFirst
    | PokeDivMouseOver String
    | PokeDivMouseOut String
    | GotPokeFull (Result Http.Error PokeFull)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPokeList result ->
            case result of
                Ok pokesList ->
                    ( { model | pokeLoadStatus = Success pokesList }, Cmd.none )

                Err error ->
                    ( { model | pokeLoadStatus = Failure error }, Cmd.none )

        PokeSearch searchStr ->
            ( { model | pokeSearchString = String.toLower searchStr }, Cmd.none )

        PokeDivMouseOver pokeNum ->
            ( { model | mouseOverPokeNum = pokeNum }, Cmd.none )

        PokeDivMouseOut pokeNum ->
            ( { model | mouseOverPokeNum = "" }, Cmd.none )

        PokeDivSelected poke ->
            let
                newPokeNum =
                    model.selectedPokeNum /= poke.num
            in
            ( { model
                | selectedPokeNum =
                    if newPokeNum then
                        poke.num

                    else
                        ""
                , selectedPokeImg = poke.img
                , pokeFullLoadStatus =
                    if newPokeNum then
                        LoadingFull

                    else
                        model.pokeFullLoadStatus
              }
            , if newPokeNum then
                Http.get
                    { url = pokeProxy ++ "https://db.pokemongohub.net/api/pokemon/" ++ poke.num ++ "/"
                    , expect = Http.expectJson GotPokeFull pokeFullDecoder
                    }

              else
                Cmd.none
            )

        GotPokeFull result ->
            case result of
                Ok pokeFull ->
                    ( { model | pokeFullLoadStatus = SuccessFull pokeFull }, Cmd.none )

                Err error ->
                    ( { model | pokeFullLoadStatus = FailureFull error }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ case model.pokeLoadStatus of
            Failure error ->
                statusViewError "Loading Pokemons Failed" error

            Loading ->
                statusViewLoading "Loading Pokemons"

            Success pokesList ->
                div [ class "container bg-light" ]
                    [ div
                        [ class "sticky-top bg-light pb-2 pt-2"
                        ]
                        [ div
                            [ style "border" "1px solid black"
                            , style "padding" "4px"
                            ]
                            [ input
                                [ class "bg-light"
                                , placeholder "search"
                                , style "border" "none"
                                , style "width" "100%"
                                , style "outline" "none"
                                , onInput PokeSearch
                                , title "You can filter your search with:\nnum:, name:, sta:, atk:, def:, evo:, type:"
                                ]
                                []
                            ]
                        , div
                            [ class "container border p-0 mt-2"
                            , classList [ ( "d-none", String.isEmpty model.selectedPokeNum ) ]
                            ]
                            [ pokeFullView model ]
                        ]
                    , div
                        [ style "border-top-style" "solid"
                        , style "border-top-color" "black"
                        , style "border-top-width" "1px"
                        , style "margin-top" "8px"
                        ]
                      <|
                        pokeListDiv pokesList model
                    ]
        ]


pokeFullView : Model -> Html Msg
pokeFullView model =
    case model.pokeFullLoadStatus of
        FailureFull error ->
            statusViewError "Loading Pokemon Info Failed" error

        LoadingFull ->
            statusViewLoading "Loading Pokemon"

        SuccessFull pokeFull ->
            statusViewPokeFull pokeFull model


statusViewPokeFull : PokeFull -> Model -> Html Msg
statusViewPokeFull poke model =
    div [ class "container p-2" ]
        [ div [ class "row" ]
            [ div [ class "col-12 text-center" ] [ img [ width 200, src model.selectedPokeImg ] [] ]
            , div [ class "col-12 text-center" ] [ h2 [] [ text poke.nom ] ]
            , div [ class "col-12" ] [ text poke.des ]
            ]
        ]


statusViewLoading : String -> Html Msg
statusViewLoading displayText =
    div [ class "container" ]
        [ div [ class "row text-center" ]
            [ div [ class "col-12 py-4" ] [ text displayText ]
            , div [ class "col-12 pb-4" ] [ div [ class "spinner-border text-dark" ] [] ]
            ]
        ]


statusViewError : String -> Http.Error -> Html Msg
statusViewError displayText error =
    div [ class "container" ]
        [ div [ class "row text-center" ]
            [ div [ class "col-12 py-3" ] [ text displayText ]
            , div [ class "col-12 py-2" ] [ img [ src "https://vignette.wikia.nocookie.net/es.pokemon/images/4/4c/Kabuto_NB.gif" ] [] ]
            , div [ class "col-12 pt-3" ] [ pre [ class "text-danger text-truncate" ] [ text (String.left 143 (Debug.toString error)) ] ]
            ]
        ]


pokeDiv : PokemonFirst -> Model -> Html Msg
pokeDiv poke model =
    div
        [ onClick (PokeDivSelected poke)
        , onMouseOver (PokeDivMouseOver poke.num)
        , onMouseOut (PokeDivMouseOut poke.num)
        , style "display" "flex"
        , style "border" "1px solid Silver"
        , style "border" "0px 0px"
        , style "display"
            (if String.contains model.pokeSearchString (pokeString poke) then
                "flex"

             else
                "none"
            )
        , style "border"
            (if model.mouseOverPokeNum == poke.num || poke.num == model.selectedPokeNum then
                "3px solid CornflowerBlue"

             else
                "1px solid Silver"
            )
        , style "background-color"
            (if poke.num == model.selectedPokeNum then
                "lightSteelBlue"

             else
                ""
            )
        ]
        [ div
            [ style "width" "70px"
            , style "display" "grid"
            ]
            [ img [ src (pokeThumb poke.thumb), width 35, style "margin" "auto" ] []
            ]
        , div
            [ style "width" "78%"
            , style "padding" "4px"
            ]
            [ b [] [ text (poke.num ++ ": " ++ poke.name) ]
            , text (" [ " ++ poke.typ ++ " ]")
            , div []
                [ b [] [ text "Stats: " ]
                , text ("atk:" ++ poke.atk ++ " | def:" ++ poke.def ++ " | sta:" ++ poke.sta)
                ]
            , div []
                [ b [] [ text "Evolutions: " ]
                , if String.isEmpty poke.evo then
                    text "No evolutions"

                  else
                    text poke.evo
                ]
            , div [ style "display" "none" ]
                [ b [] [ text "Attacks:" ]
                , div [] [ text ("- " ++ poke.prim_atks) ]
                , div [] [ text ("- " ++ poke.secu_atks) ]
                ]
            ]
        ]


pokeListDiv : List PokemonFirst -> Model -> List (Html Msg)
pokeListDiv pokes model =
    List.map (\poke -> pokeDiv poke model) pokes



-- HELPERS


pokeString : PokemonFirst -> String
pokeString poke =
    String.toLower
        ("num:"
            ++ poke.num
            ++ "name:"
            ++ poke.name
            ++ "sta:"
            ++ poke.sta
            ++ "atk:"
            ++ poke.atk
            ++ "def:"
            ++ poke.def
            ++ pokeStringEvos poke.evo
            ++ pokeStringTypes poke.typ
         -- ++ poke.prim_atks
         -- ++ poke.secu_atks
        )


pokeStringTypes : String -> String
pokeStringTypes types =
    String.join " " (List.map (\typ -> "type:" ++ typ) (String.split "," (String.replace " " "" types)))


pokeStringEvos : String -> String
pokeStringEvos evos =
    String.join " " (List.map (\evo -> "evo:" ++ evo) (String.split "," (String.replace " " "" evos)))


pokeProxy : String
pokeProxy =
    "https://api.codetabs.com/v1/proxy?quest="


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



-- TODO: https://www.reddit.com/r/elm/comments/91t937/is_it_possible_to_make_multiple_http_requests_in/
