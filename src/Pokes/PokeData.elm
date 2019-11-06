module PokeData exposing (PokeClassType(..), PokeType, pokeClassType)


type alias PokeType =
    { clr : String -- type color
    , bdg : String -- type badge
    , icn : String -- type icon
    , b24 : String -- badge 24x24
    , i24 : String -- icon 24x24
    }


type PokeClassType
    = PokeClassNrml -- Poke Class Normal
    | PokeClassLgnd -- Poke Class Legendary
    | PokeClassMyth -- Poke Class Mythical


pokeClassType : String -> PokeClassType
pokeClassType pokeType =
    let
        lowr =
            String.toLower pokeType

        myth =
            String.contains "mythic" lowr

        lgnd =
            String.contains "legend" lowr

        nrml =
            String.contains "normal" lowr
    in
    if myth then
        PokeClassMyth

    else if lgnd then
        PokeClassLgnd

    else
        PokeClassNrml



-- unknown type could be added
