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


tariffs : List Tariff
tariffs =
    [ Tariff "FESCO" "RUVVO" "CNSHA" "40'HC" "Full" "SOC" "650.0" "50.0"
    , Tariff "FESCO" "RUVVO" "CNSHA" "40'HC" "Full" "SOC" "700.0" "50.0"
    , Tariff "FESCO" "RUVVO" "CNSHA" "20'DC" "Full" "SOC" "525.0" "25.0"
    , Tariff "FESCO" "RUVVO" "CNSHA" "20'DC" "Full" "SOC" "550.0" "25.0"
    , Tariff "FESCO" "RUVVO" "CNSHA" "40'RF" "Full" "SOC" "1300.0" "50.0"
    ]
