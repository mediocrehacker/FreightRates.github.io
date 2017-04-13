module Types exposing (..)


type alias SeaPort =
    { code : String
    , name : String
    , country : String
    }


seaPorts : List SeaPort
seaPorts =
    [ SeaPort "RUVVO" "Vladivostok" "Russis"
    , SeaPort "CNSHA" "Shanghai" "China"
    , SeaPort "HKHKG" "Hong Kong" "Hong Kong"
    , SeaPort "CNSWA" "Shantou" "China"
    , SeaPort "CNDLC" "Dalian" "China"
    ]


type alias Tariff =
    { company : String
    , pol : String
    , pod : String
    , container : String
    , status : String
    , owners : String
    , freight : String
    , baf : String
    }
