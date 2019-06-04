module PokeData exposing (PokeType)


type alias PokeType =
    { clr : String -- type color
    , bdg : String -- type badge
    , icn : String -- type icon
    , b24 : String -- badge 24x24
    , i24 : String -- icon 24x24
    }


type PokeClass
    = PokeClassNrml -- Poke Class Normal
    | PokeClassLgnd -- Poke Class Legendary
    | PokeClassMyth -- Poke Class Mythical
