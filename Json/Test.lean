import Json.JsonParser


-- ------------------------- Tests ------------------------------
#eval jsonParser "{ \"name\": [ 234, -23e15]} "


#eval integerP "-123"
#eval fractionP ".23k"
#eval exponentP "e23"
#eval jNumberP "023.13"

  -- String.mk <$> traverse charP lst
#eval (List.cons <$> (charP 'a') <*> (List.cons <$> (charP 'b') <*> (pure []))) "abc"

#eval (pure (. == 'a') <*> (charP 'a')) "abc"
#eval (List.cons <$> (charP 'a') <*> (pure [])) "abc"
#eval (charIf (fun x => x == 'a')) "aABC"
#eval Functor.map (. == 'a') (charIf (. == 'a')) "aABC"
#eval charP 'a' "abc"
#eval stringP "ab" "abc"
#eval Traversable.sequence [charP 'a', charP 'b'] "abc"

#reduce charP 'a' "abc"
#reduce stringP "a" "abc"

#eval List.cons <$> (charP 'a') <*> (pure []) <| "abc"


#eval (charP 'a' <|> charP 'b') "abc"

#eval (charP 'a') "abc"
#eval ((MyParser.pure "a") <|> (stringP "a")) "abc"

#eval (oneOrMore_ 10 (stringP "ab")) "abc"
#eval (many (charP 'a')) "abc"

#eval ((many <| charP 'a') *> (charP 'b')) "aaaaab"

#eval (oneOrMore (charIf (const true))) "abc"
def hello := "world"

#eval jNullP "null"
#eval jArrayP "[ \n null]"
#eval jArrayP "[]"
#eval jArrayP "[[null]]"

#eval separatedBy (charP 'a') (charP ',') "a"
#eval separatedBy (charP 'a') (charP ',') "a,a,ab"
#eval jBoolP "true"
#eval jValueP "[true, false, Null, \"hello \"]"

#eval characterP "\\u234212"
#eval jStringP "\"ab\"cd\""
#eval hexP "0abc"
#eval charactersP "\\u234212\\b23"
#eval jStringP "\"\\u234212\\b23Ä"


#eval membersP "\"name\" : \"Phil\""
#eval memberP " \"name\" : \"Phil\""

#eval jObjectP "{}"

#eval (List.cons <$> (charP 'a') <*> (List.cons <$> (charP 'b') <*> pure [])) "ab"


def f : Int -> Int := fun a => a + 1

def foo : (α -> α) -> (Char × Bool) := fun f => ('a', true)

#eval foo f

#eval String.append "adb" "dcd"
