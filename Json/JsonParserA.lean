open Option

inductive J where
  | JNull   : J
  | JArray  : List J → J
  | JObject : List (J × J) -> J
  | JBool   : Bool -> J
  | JString: String -> J
  | JNumber : String -> J
  deriving Repr

open J

-- Define the parser type
def Parser.{u} (α : Type u) := List Char → Option (List Char × α)

def map_.{u}  {α β : Type u} (f : α → β) (a : Parser α) : Parser β :=
  fun (s : List Char) =>
    match a s with
    | none => none
    | some (context, s') => some (context, (f s'))

instance MyParserF : Functor Parser where
  map := map_

open Functor

class Traversable (t : Type u -> Type u) extends Functor t where
  -- traverse : ∀ {m : Type u -> Type u} [Applicative m] {α β}, (α -> m β) -> t α -> m (t β)
  traverse {m : Type u -> Type u} [Applicative m] {α β : Type u} : (α -> m β) -> t α -> m (t β)
  sequence {m : Type u -> Type u} [Applicative m] {α : Type u} : t (m α) -> m (t α) := traverse id

-- def traverse_ {m : Type u -> Type u} [Applicative m] (f : α -> m β) (xs : List α) : m (List β) :=
def traverse_ {m : Type u -> Type u} [Applicative m] (f : α -> m β) (xs : List α) : m (List β) :=
  let rec go (ys : List α) := match ys with
  | [] => pure []
  | y :: rst => List.cons <$> (f y) <*> (go rst)
  go xs

instance : Traversable List where
  traverse := traverse_

open Traversable

def pure_ (a : α) : (Parser α) := fun (s : List Char) => some (s, a)

def seq_.{u} {α β : Type u} (f : Parser (α → β)) (a : Unit -> Parser α) : Parser β :=
  fun (s : List Char) => do
    let (s', f') <- f s
    let (s'', v) <- a () s'
    some (s'', (f' v))

instance MyParser : Applicative Parser where
  pure := pure_
  seq := seq_

open Applicative

def const {α : Type u} {β : Type v} (x : α) (_ : β) : α := x
def failure_.{u} : {α : Type u} → Parser α := const none
def orElse_.{u} {α : Type u} (p0 : Parser α) (p1 : Unit -> Parser α) : Parser α :=
  fun (str : List Char) =>
    p0 str <|> p1 () str

instance : Alternative Parser where
  failure := failure_
  orElse := orElse_

open Alternative

def MAX_RECU := 4096

mutual
  -- Bounded `many` with a maximum recursion depth `n`
  def many_.{u} {α : Type u} (n : Nat) (f : Parser α) : Parser (List α) :=
    if n = 0 then MyParser.pure []
    else (oneOrMore_ n f <|> MyParser.pure [])

  -- Bounded `oneOrMore` with a maximum recursion depth `n`
  def oneOrMore_.{u} {α : Type u} (n : Nat) (f : Parser α) : Parser (List α) :=
    if n = 0 then MyParser.pure []
    else List.cons <$> f <*> many_ (n - 1) f
  def many.{u} {α : Type u} : Parser α -> Parser (List α) := many_ MAX_RECU
  def oneOrMore.{u} {α : Type u} : Parser α -> Parser (List α) := oneOrMore_ MAX_RECU
end

def anyOf.{u} {α : Type u} (xs : List (Parser α)) : Parser α :=
  List.foldl (fun acc x => x <|> acc ) failure xs

def separatedBy (item : Parser α) (sep : Parser β) : Parser (List α) :=
  List.cons <$> item <*> many (sep *> item)

def charIf (p : Char -> Bool) : Parser Char :=
  fun (str : List Char) => match str with
    | [] => none
    | x :: xs => if p x
                  then some (xs, x)
                  else none

def anyCharP := charIf (const true)

def charP (c : Char) : Parser Char :=
  charIf (. == c)

def stringP (str : List Char) : Parser (List Char) :=
  Traversable.traverse charP str

def whiteSpace := "\x20\x0A\x0D\x09"
def wsP := many <| anyOf (map charP whiteSpace.data)

def jBoolP : (Parser J) :=
  JBool <$> (pure true <* (stringP "true".data) <|> (stringP "false".data) *> pure false)

-- JList Char

def hexP :=
  charIf (fun x =>
    let n := x.toNat
    n >= '0'.toNat && n <= '9'.toNat) <|>
  charIf (fun x =>
    let n := x.toNat
    n >= 'a'.toNat && n <= 'z'.toNat) <|>
  charIf (fun x =>
    let n := x.toNat
    n >= 'A'.toNat && n <= 'Z'.toNat)

def escapeP : Parser (List Char) :=
  List.cons <$> (anyOf <| map charP ("\"\\bfnrt".data)) <*> pure [] <|>
  sequence [charP 'u', hexP, hexP, hexP, hexP]

def characterP := List.cons <$> charIf (fun x =>
  let n := x.toNat
  n >= 0x0020 && n <= 0x10FFFF && x != '\\' && x != '"') <*> pure []  <|>
  List.append <$> stringP "\\".data <*> escapeP

def charactersP := List.join <$> (many characterP)

def jStringP : (Parser J) :=
  JString <$> (charP '"' *> (String.mk <$> charactersP) <* (charP '"'))


-- JNull
def jNullP : Parser J :=
  (const JNull) <$> (stringP "null".data)

-- JNumber
def signP : Parser (List Char) :=
  List.cons <$> anyOf (map charP "+-".data) <*> pure [] <|> (pure [])

def onenineP : Parser (List Char) :=
  List.cons <$> (anyOf <| map charP "123456789".data) <*> pure []

def digitP : Parser (List Char) :=
  List.cons <$> (charP '0') <*> pure [] <|> onenineP

def digitsP : Parser (List Char) :=
  List.join <$> (many digitP)

def integerP : Parser (List Char) :=
  List.append <$> onenineP <*> digitsP <|>
  List.append <$> (List.append <$> (stringP "-".data) <*> onenineP) <*> digitsP <|>
  List.append <$> (stringP "-".data) <*> digitP <|>
  digitP

def fractionP : Parser (List Char) :=
  List.append <$> (stringP ".".data) <*> digitsP <|> pure []

def exponentP : Parser (List Char) :=
  List.append <$> (List.append <$> (stringP "E".data <|> (stringP "e".data)) <*> signP) <*> digitsP
  <|> pure []

def jNumberP : Parser J :=
  J.JNumber <$> (String.mk <$> (List.append <$> (List.append <$> integerP <*> fractionP) <*> exponentP))

mutual
  def elementP_  (n : Nat) :=
    if n = 0 then jNullP
    else wsP *> jValueP_ (n - 1) <* wsP

  def elementsP_ (n : Nat) :=
    if n = 0 then pure []
    else separatedBy (elementP_ (n - 1)) (charP ',') <|> pure []

  def jArrayP_ (n: Nat) : Parser J :=
    if n = 0 then jNullP
    else JArray <$> ((stringP "[".data) *> (elementsP_ (n - 1)) <* (stringP "]".data))

  def jValueP_  (n: Nat) : Parser J :=
    if n = 0 then jNullP
    else jNullP <|> jStringP <|> jBoolP <|> jObjectP_ (n - 1) <|> jNumberP <|>jArrayP_ (n - 1)

  -- JObject
  def memberP_ (n : Nat) : Parser (J × J) :=
    if n = 0 then pure (JNull, JNull)
    else Prod.mk <$> (wsP *> jStringP <* wsP <* stringP ":".data) <*> elementP_ (n - 1)

  def membersP_ (n : Nat) : Parser (List (J × J)) :=
    if n = 0 then pure []
    else separatedBy (memberP_ (n - 1)) (charP ',') <|> (wsP *> pure [])

  def jObjectP_ (n : Nat) : Parser J :=
    if n = 0 then jNullP
    else JObject <$> (stringP "{".data *> (membersP_ (n - 1)) <* (stringP "}".data))

  def jArrayP := jArrayP_ MAX_RECU
  def jValueP := jValueP_ MAX_RECU
  def elementP := elementP_ MAX_RECU
  def elementsP := elementsP_ MAX_RECU
  def jObjectP := jObjectP_ MAX_RECU
  def memberP := memberP_ MAX_RECU
  def membersP := membersP_ MAX_RECU
end

def jsonParser : Parser J :=
  elementP
