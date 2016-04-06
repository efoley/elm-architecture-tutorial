import Table exposing (initModel, update, view)
import StartApp.Simple exposing (start)


main =
  start
    { model = initModel 4 5
    , update = update
    , view = view
    }
