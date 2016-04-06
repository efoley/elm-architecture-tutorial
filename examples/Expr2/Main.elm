
import Table
import StartApp.Simple exposing (start)


main =
  start
    { model = Table.initModel 4 5
    , update = Table.update
    , view = Table.view
    }
