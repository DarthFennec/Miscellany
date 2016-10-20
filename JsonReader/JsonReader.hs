module JsonReader
  ( JsonValue(..)
  , readJson
  ) where

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Eq, Show)

data JsonToken
  = JsonTokenNull
  | JsonTokenTrue
  | JsonTokenFalse
  | JsonTokenNumber Double
  | JsonTokenString String
  | JsonTokenArrayOpen
  | JsonTokenArrayClose
  | JsonTokenObjectOpen
  | JsonTokenObjectClose
  | JsonTokenColon
  | JsonTokenComma
  | JsonTokenEOF
  deriving Eq

readJson :: String -> Maybe JsonValue
readJson xs = case lexJson xs >>= parseJson of
  Just (v, []) -> Just v
  _ -> Nothing

parseJson :: [JsonToken] -> Maybe (JsonValue, [JsonToken])
parseJson (JsonTokenNull:xs) = Just (JsonNull, xs)
parseJson (JsonTokenTrue:xs) = Just (JsonBool True, xs)
parseJson (JsonTokenFalse:xs) = Just (JsonBool False, xs)
parseJson (JsonTokenNumber x:xs) = Just (JsonNumber x, xs)
parseJson (JsonTokenString x:xs) = Just (JsonString x, xs)
parseJson (JsonTokenArrayOpen:xs) = parseJsonArray xs
parseJson (JsonTokenObjectOpen:xs) = parseJsonObject xs
parseJson _ = Nothing

parseJsonArray :: [JsonToken] -> Maybe (JsonValue, [JsonToken])
parseJsonArray (JsonTokenArrayClose:xs) = Just (JsonArray [], xs)
parseJsonArray xs = parseJsonArray' xs

parseJsonArray' :: [JsonToken] -> Maybe (JsonValue, [JsonToken])
parseJsonArray' xs = case parseJson xs of
  Just (v, JsonTokenArrayClose:ts) -> Just (JsonArray [v], ts)
  Just (v, JsonTokenComma:ts) -> do
    (JsonArray vs, ts') <- parseJsonArray' ts
    return (JsonArray $ v:vs, ts')
  _ -> Nothing

parseJsonObject :: [JsonToken] -> Maybe (JsonValue, [JsonToken])
parseJsonObject (JsonTokenObjectClose:xs) = Just (JsonObject [], xs)
parseJsonObject xs = parseJsonObject' xs

parseJsonObject' :: [JsonToken] -> Maybe (JsonValue, [JsonToken])
parseJsonObject' (JsonTokenString s:JsonTokenColon:xs) = case parseJson xs of
  Just (v, JsonTokenObjectClose:ts) -> Just (JsonObject [(s, v)], ts)
  Just (v, JsonTokenComma:ts) -> do
    (JsonObject vs, ts') <- parseJsonObject' ts
    return (JsonObject $ (s, v):vs, ts')
  _ -> Nothing
parseJsonObject' _ = Nothing

lexJson :: String -> Maybe [JsonToken]
lexJson xs = case readJsonToken xs of
  Nothing -> Nothing
  Just (JsonTokenEOF, _) -> Just []
  Just (t, xs') -> lexJson xs' >>= Just.(t:)

readJsonToken :: String -> Maybe (JsonToken, String)
readJsonToken [] = Just (JsonTokenEOF, [])
readJsonToken (x:xs) | elem x " \n\r\t" = readJsonToken xs
readJsonToken ('n':'u':'l':'l':xs) = Just (JsonTokenNull, xs)
readJsonToken ('t':'r':'u':'e':xs) = Just (JsonTokenTrue, xs)
readJsonToken ('f':'a':'l':'s':'e':xs) = Just (JsonTokenFalse, xs)
readJsonToken xs@(x:_) | x == '-' || x >= '0' && x <= '9' = readJsonNumber xs
readJsonToken ('"':xs) = readJsonString xs
readJsonToken ('[':xs) = Just (JsonTokenArrayOpen, xs)
readJsonToken (']':xs) = Just (JsonTokenArrayClose, xs)
readJsonToken ('{':xs) = Just (JsonTokenObjectOpen, xs)
readJsonToken ('}':xs) = Just (JsonTokenObjectClose, xs)
readJsonToken (':':xs) = Just (JsonTokenColon, xs)
readJsonToken (',':xs) = Just (JsonTokenComma, xs)
readJsonToken _ = Nothing

readJsonNumber :: String -> Maybe (JsonToken, String)
readJsonNumber rest1 = do
  (minus, rest2) <- getMinus rest1
  (intpt, rest3) <- getIntPart rest2
  (decpt, rest4) <- getDecPart rest3
  (exppt, rest5) <- getExpPart rest4
  return (JsonTokenNumber $ read $ concat [minus, intpt, decpt, exppt], rest5)
  where
    getMinus ('-':xs) = Just ("-", xs)
    getMinus xs = Just ([], xs)
    getIntPart ('0':xs) = Just ("0", xs)
    getIntPart xs = getDigits xs
    getDecPart ('.':xs) = fmap (\(d, x) -> ('.':d, x)) $ getDigits xs
    getDecPart xs = Just ([], xs)
    getExpPart ('e':xs) = getExpPart' xs
    getExpPart ('E':xs) = getExpPart' xs
    getExpPart xs = Just ([], xs)
    getExpPart' ('-':xs) = fmap (\(d, x) -> ('E':'-':d, x)) $ getDigits xs
    getExpPart' ('+':xs) = fmap (\(d, x) -> ('E':'+':d, x)) $ getDigits xs
    getExpPart' xs = fmap (\(d, x) -> ('E':'+':d, x)) $ getDigits xs
    getDigits xs = checkDigits $ span (\x -> x >= '0' && x <= '9') xs
    checkDigits ([], _) = Nothing
    checkDigits x = Just x

readJsonString :: String -> Maybe (JsonToken, String)
readJsonString xs = fmap (\(t, s) -> (JsonTokenString t, s)) $ getChars xs
  where
    getChars ('"':xs) = Just ([], xs)
    getChars xs = do
      (c, xs') <- getChar xs
      (s, xs'') <- getChars xs'
      return (c:s, xs'')
    getChar [] = Nothing
    getChar (x:_) | x < ' ' = Nothing
    getChar ('\\':xs) = getEscape xs
    getChar (x:xs) = Just (x, xs)
    getEscape ('"':xs) = Just ('"', xs)
    getEscape ('/':xs) = Just ('/', xs)
    getEscape ('\\':xs) = Just ('\\', xs)
    getEscape ('b':xs) = Just ('\b', xs)
    getEscape ('f':xs) = Just ('\f', xs)
    getEscape ('n':xs) = Just ('\n', xs)
    getEscape ('r':xs) = Just ('\r', xs)
    getEscape ('t':xs) = Just ('\t', xs)
    getEscape ('u':w:x:y:z:xs) = getUnicode [w, x, y, z] xs
    getEscape _ = Nothing
    getUnicode n xs | all isHex n = Just (toEnum $ read $ '0':'x':n, xs)
    getUnicode _ _ = Nothing
    isHex x = inRange 'A' 'Z' x || inRange 'a' 'z' x || inRange '0' '9' x
    inRange f t x = x >= f && x <= t
