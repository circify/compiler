{
{-# OPTIONS_GHC -Wno-all #-}
module Parser.Zokrates.Lexer (tokenize, Token(..), AlexPosn(AlexPn), start, end, str) where
}

%wrapper "monadUserState"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$inlinewhite = [\ \t\f\v]       -- inline whitespace

tokens :-

  (\r\n|\n)+                            { tok $ \p s -> Newline p }
  $inlinewhite                                      ;
  \/\/[^\r\n]*                                      ;
  \#[^\r\n]*                                        ;
  \\                                                ;
  \/\*([^\*]|[\r\n]|(\*+([^\*\/]|[\r\n])))*\*+\/    ;
  \" ($printable # \")* \"              { tok $ \p s -> StrLit p $ take (length s - 2) $ drop 1 s }
  "bool"                                { tok $ \p s -> Bool p }
  "true"                                { tok $ \p s -> TrueLit p }
  "false"                               { tok $ \p s -> FalseLit p }
  $digit+                               { tok $ \p s -> IntLit p s }
  "u8"                                  { tok $ \p s -> U8 p }
  "u16"                                 { tok $ \p s -> U16 p }
  "u32"                                 { tok $ \p s -> U32 p }
  "field"                               { tok $ \p s -> Field p }
  "0x"[0-9 a-f A-F]+                    { tok $ \p s -> HexLit p s }
  "private"                             { tok $ \p s -> Private p }
  "if"                                  { tok $ \p s -> If p }
  "then"                                { tok $ \p s -> Then p }
  "else"                                { tok $ \p s -> Else p }
  "fi"                                  { tok $ \p s -> Fi p }
  "for"                                 { tok $ \p s -> For p }
  "in"                                  { tok $ \p s -> In p }
  "do"                                  { tok $ \p s -> Do p }
  "endfor"                              { tok $ \p s -> EndFor p }
  "assert"                              { tok $ \p s -> Assert p }
  "def"                                 { tok $ \p s -> Def p }
  "return"                              { tok $ \p s -> Return p }
  "struct"                              { tok $ \p s -> Struct p }
  "import"                              { tok $ \p s -> Import p }
  "from"                                { tok $ \p s -> From p }
  "as"                                  { tok $ \p s -> As p }
  $alpha [$alpha $digit \_ \']*         { tok $ \p s -> Ident p s }
  \,                                    { tok $ \p s -> Comma p }
  \.\.\.                                { tok $ \p s -> DotDotDot p }
  \.\.                                  { tok $ \p s -> DotDot p }
  \.                                    { tok $ \p s -> Dot p }
  \;                                    { tok $ \p s -> SemiColon p }
  \:                                    { tok $ \p s -> Colon p }
  \(                                    { tok $ \p s -> BeginParen p }
  \[                                    { tok $ \p s -> BeginBracket p }
  \{                                    { tok $ \p s -> BeginBrace p }
  \)                                    { tok $ \p s -> EndParen p }
  \]                                    { tok $ \p s -> EndBracket p }
  \}                                    { tok $ \p s -> EndBrace p }
  [\<\>\-\=\!\~\%\^\&\*\/\?\+\|]+       { tok $ \p s -> Symbols p s }

{

-- The token type:
data Token = Bool AlexPosn
           | TrueLit AlexPosn
           | FalseLit AlexPosn
           | IntLit AlexPosn String
           | StrLit AlexPosn String
           | HexLit AlexPosn String
           | U8 AlexPosn
           | U16 AlexPosn
           | U32 AlexPosn
           | Field AlexPosn
           | Indent AlexPosn
           | Dedent AlexPosn
           | Private AlexPosn
           | If AlexPosn
           | Then AlexPosn
           | Else AlexPosn
           | Fi AlexPosn
           | For AlexPosn
           | In AlexPosn
           | Do AlexPosn
           | EndFor AlexPosn
           | Assert AlexPosn
           | Def AlexPosn
           | Return AlexPosn
           | Import AlexPosn
           | From AlexPosn
           | As AlexPosn
           | Struct AlexPosn
           | Ident AlexPosn String
           | DotDotDot AlexPosn
           | DotDot AlexPosn
           | Dot AlexPosn
           | SemiColon AlexPosn
           | Colon AlexPosn
           | Symbols AlexPosn String
           | Comma AlexPosn
           | BeginParen AlexPosn
           | BeginBracket AlexPosn
           | BeginBrace AlexPosn
           | EndParen AlexPosn
           | EndBracket AlexPosn
           | Newline AlexPosn
           | EndBrace AlexPosn
           deriving (Show)

start :: Token -> AlexPosn
start t = case t of
           Bool p -> p
           TrueLit p -> p
           FalseLit p -> p
           IntLit p _ -> p
           StrLit p _ -> p
           HexLit p _ -> p
           U8 p -> p
           U16 p -> p
           U32 p -> p
           Field p -> p
           Indent p -> p
           Dedent p -> p
           Private p -> p
           If p -> p
           Then p -> p
           Else p -> p
           Fi p -> p
           For p -> p
           In p -> p
           Do p -> p
           EndFor p -> p
           Assert p -> p
           Def p -> p
           Return p -> p
           Struct p -> p
           Import p -> p
           From p -> p
           As p -> p
           Ident p _ -> p
           DotDotDot p -> p
           DotDot p -> p
           Dot p -> p
           SemiColon p -> p
           Colon p -> p
           Symbols p _ -> p
           Comma p -> p
           BeginParen p -> p
           BeginBracket p -> p
           BeginBrace p -> p
           EndParen p -> p
           EndBracket p -> p
           Newline p -> p
           EndBrace p -> p

str :: Token -> String
str t = case t of
           Bool _ -> "bool"
           TrueLit _ -> "true"
           FalseLit _ -> "false"
           IntLit _ s -> s
           StrLit _ s -> "\"" ++ s ++ "\""
           HexLit _ s -> s
           U8 _ -> "u8"
           U16 _ -> "u16"
           U32 _ -> "u32"
           Field _ -> "field"
           Indent _ -> "INDENT"
           Dedent _ -> "DEDENT"
           Private _ -> "private"
           If _ -> "if"
           Then _ -> "then"
           Else _ -> "else"
           Fi _ -> "fi"
           For _ -> "for"
           In _ -> "in"
           Do _ -> "do"
           EndFor _ ->"endfor"
           Assert _ -> "assert"
           Def _ -> "def"
           Return _ -> "return"
           Struct _ -> "struct"
           Import _ -> "import"
           From _ -> "from"
           As _ -> "as"
           Ident _ s -> s
           DotDotDot _ -> "..."
           DotDot _ -> ".."
           Dot _ -> "."
           SemiColon _ -> ";"
           Colon _ -> ":"
           Symbols _ s -> s
           Comma _ -> ","
           BeginParen _ -> "("
           BeginBracket _ -> "["
           BeginBrace _ -> "{"
           EndParen _ -> ")"
           EndBracket _ -> "]"
           EndBrace _ -> "}"
           Newline _ -> "\n"

end :: Token -> AlexPosn
end t =
  let len = length $ str t
      AlexPn charN line col = start t
  in  AlexPn (charN + len) line (col + len)


--tokenize :: String -> [Token]
--tokenize = alexScanTokens

data AlexUserState = AlexUserState
                   { prevIndent :: Int
                   , curIndent :: Int
                   , indentEnded :: Bool
                   , tokens :: [Token]
                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   { prevIndent  = 0
                   , curIndent  = 0
                   , indentEnded = False
                   , tokens = []
                   }

-- s :: AlexUserState -> AlexUserState
-- alex_ust :: AlexState -> AlexUserState
-- -> Returns the current state from AlexState.
modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \s -> let current = alex_ust s
                                     new     = f current
                                 in
                                   Right (s { alex_ust = new },())

-- Returns the current state.
-- I.e., a list of tokens.
getUserState ::  Alex AlexUserState
getUserState = Alex $ \s -> Right (s,alex_ust s)

-- Each action per token should be a value of result `AlexAction`.
-- type AlexAction a = AlexInput -> Int -> Alex a
-- type AlexInput = (AlexPosn, Char, [Byte], String)

tok :: (AlexPosn -> String -> Token) -> AlexAction ()
tok f (p, _, _, s) l = pushToken (f p $ take l s) >> alexMonadScan

pushToken :: Token -> Alex ()
pushToken t = modifyUserState $ \ust -> ust{tokens=t:tokens ust}

alexEOF :: Alex ()
alexEOF = return ()

-- | remove adjacent newlines
uniqNl :: [Token] -> [Token]
uniqNl [] = []
uniqNl (x@Newline {}:xs) = x:uniqNl (dropWhile isNl xs)
 where
  isNl (Newline {}) = True
  isNl _ = False
uniqNl (x:xs) = x:uniqNl xs


tokenize :: String -> Either String [Token]
tokenize s = runAlex s $ alexMonadScan >> (uniqNl . reverse . tokens <$> getUserState)
}
