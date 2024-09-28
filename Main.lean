import Json.Basic
import Init.System

def testJson := "
  {
    \"name\"  : {
    }
  }
"

def testJson2 := "{ }"

-- #eval jsonParser testJson2

def main (args : List String) : IO Unit := do
  IO.println args
  match args.get? 0 with
  | Option.none => IO.println "Please input a json file."
  | Option.some filename =>
    let json ← IO.FS.readFile filename
    -- IO.println json
    jsonParser json.data |> reprStr |> IO.println
