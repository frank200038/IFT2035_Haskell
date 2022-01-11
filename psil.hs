-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
-- Yan Zhuang, Yu Deng
{-# OPTIONS_GHC -Wall #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                        --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2') =
              showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire                                          --
---------------------------------------------------------------------------

type Var = String

data Ltype = Lint               -- Int -- done
           | Lboo               -- Bool -- done
           | Larw Ltype Ltype   -- τ₁ → τ₂ --done
           | Ltup [Ltype]       -- tuple τ₁...τₙ --done
           deriving (Show, Eq)

data Lexp = Lnum Int                    -- Constante entière. -- done
          | Lvar Var                    -- Référence à une variable. -- done
          | Lhastype Lexp Ltype         -- Annotation de type. -- done
          | Lcall Lexp Lexp             -- Appel de fonction, avec un argument. -- done
          | Lfun Var Lexp               -- Fonction anonyme prenant un argument. -- done
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Llet [(Var, Lexp)] Lexp     -- done
          | Lif Lexp Lexp Lexp          -- Expression conditionelle. -- done
          | Ltuple [Lexp]               -- Construction de tuple -- done
          | Lfetch Lexp [Var] Lexp      -- lecture d'un tuple  -- done
          deriving (Show, Eq)


---------------------------------------------------------------------------
-- Conversion de Sexp à Lexp                                             --
---------------------------------------------------------------------------

sexp2list :: Sexp -> [Sexp]
sexp2list s = loop s []
    where
      loop (Scons hds tl) acc = loop hds (tl : acc)
      loop Snil acc = acc
      loop _ _ = error ("Improper list: " ++ show s)

-- extract middle part of an array (Without head and tail)
extractMiddle :: [a] -> [a]
extractMiddle (x:_:[]) = [x]
extractMiddle (x:xs) = x : extractMiddle xs

-- Extract the name of variable and convert
extractName :: Sexp -> Lexp
extractName a@(Ssym _) = (s2l a)

-- Extract the name of all variables and convert
extractNameAll :: [Sexp] -> [Lexp]
extractNameAll [] = []
extractNameAll (x:xs) = (extractName ((sexp2list x) !! 0) ):extractNameAll xs

-- Extract the type of all variables and convert
extractTypeAll :: [Sexp] -> [Ltype]
extractTypeAll [] = []
extractTypeAll (x:xs) = (s2t ((sexp2list x) !! 1)) : extractTypeAll xs

-- Merge the type of function
-- [Larw Lint Lint,Lint] => Larw (Larw Lint Lint ) Lint
linkTypeAll :: [Ltype] -> Ltype
linkTypeAll x = foldr1 Larw x

-- To extract arguments
exrVar :: Sexp -> [Var] -> [Var]
exrVar (Ssym a) list = (a: list)
exrVar (Scons a b) list | a /= Snil = exrVar a (exrVar b list)
                        | a == Snil = exrVar b list

-- redefine foldr1 to help eliminate syntactic sugar of fun
foldr1Fun :: (Var->Lexp->Lexp) -> [Lexp] -> Lexp
foldr1Fun _ [x] = x
foldr1Fun f (Lvar x:xs) = f x (foldr1Fun f xs)

-- Elinmiate syntatic sugar of `let` and convert
-- (let (x (y Bool) (z Int) Bool `body`) => (let (x Bool->Int->Bool) 
--             (fun (y z) `body`)) => 
--             (let (x (hastype (fun (y z) `body`) (Bool->Int->Bool))))
--             => equivalent in Lexp
buildHasType :: [Sexp] -> Lexp
buildHasType b = let mid = extractMiddle b
                     fst = init mid
                     lastElem = s2l (last b)
                     returnType = s2t (last mid)
                     combinedType = extractTypeAll fst++[returnType]
                     extractedType = linkTypeAll combinedType
                     extractedName = extractNameAll fst
                     linkedFun = foldr1Fun Lfun (extractedName ++ [lastElem])
                  in Lhastype linkedFun extractedType

-- Analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s

s2l (Scons (Scons (Scons Snil (Ssym "call")) arg1) arg2) = 
                                              Lcall (s2l arg1) (s2l arg2)
s2l (Scons (Scons (Scons (Scons Snil (Ssym "fetch")) a ) b ) c) = 
                                          Lfetch (s2l a) extracted (s2l c)
                                          where extracted = exrVar b []
                                          
s2l (se@(Scons _ _)) =  let splited = sexp2list se
                        in case splited of
  (Ssym "hastype" : e : t : []) -> Lhastype (s2l e) (s2t t)

  (Ssym "let":e) -> 
    let middle = extractMiddle e
    in  Llet (map (\x -> checkEach (sexp2list x)) middle) (s2l (last splited))
       where checkEach (Ssym a:b:[]) = (a, s2l b)
             checkEach (Ssym a:b:c:[]) = (a, Lhastype (s2l c) (s2t b))
             checkEach (Ssym a:b) = (a, buildHasType b)

  (Ssym "call":e) -> let (Scons a b) = se
                         exprA = s2l a
                         exprB = s2l b
                     in case (exprA, exprB) of
                        (Lcall a b,c) -> Lcall (Lcall a b) c

  (Ssym "tuple" : e) -> Ltuple (map (\x -> s2l x) e)

  (Ssym "fun":e) -> let converted = map (\x-> s2l x) e
                    in foldr1Fun Lfun converted

  (Ssym "if":cond: true:false:[]) -> Lif (s2l cond) (s2l true) (s2l false)

  (x:_) -> s2l x

s2l se = error ("Unrecognized Psil expression: " ++ (showSexp se))

-- Remove "->" in an array
-- [Ssym "Int",Ssym "->",Ssym "Int"] => [Ssym "Int",Ssym "Int"] 
cleanType :: [Sexp] -> [Sexp]
cleanType [] = []
cleanType (x:xs) | x == Ssym "->" = cleanType xs
                 | otherwise = x:cleanType xs

s2t :: Sexp -> Ltype
s2t (Ssym "Int") = Lint
s2t (Ssym "Bool") = Lboo
s2t (Scons (Scons (Scons Snil (Ssym "Tuple")) a) b) = Ltup ((s2t a):(s2t b):[])
s2t (Scons (Scons Snil (Ssym "Tuple")) a) =  Ltup ((s2t a):[])

s2t (se@(Scons _ _)) =  let splited = sexp2list se
                        in case splited of
                        (Ssym "Tuple":e) -> let (Scons a b) = se
                                                exprA = s2t a
                                                exprB = s2t b
                                           in case (exprA, exprB) of
                                              (Ltup a,b) -> Ltup (a ++ [b])

                        (x) -> let converted = map (\x -> s2t x) (cleanType x)
                                                   in foldr1 Larw converted

s2t s = error ("Unrecognized Psil type: " ++ (showSexp s))

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Type des valeurs renvoyées par l'évaluateur.
data Value = Vnum Int
           | Vbool Bool
           | Vtuple [Value]
           | Vfun (Maybe String) (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec p (Vtuple vs) =
        showString "[" . showValues vs . showString "]"
        where showValues [] = showString ""
              showValues (v:vs') =
                  showString " " . showsPrec p v . showValues vs'
    showsPrec _ (Vfun (Just n) _) =
          showString "<fun " . showString n . showString ">"
    showsPrec _ (Vfun Nothing _) = showString "<fun>"

type Env = [(Var, Value, Ltype)]

-- L'environnement initial qui contient les fonctions prédéfinies et leur type.
env0 :: Env
env0 = [prim "+"  (+) Vnum  Lint,
        prim "-"  (-) Vnum  Lint,
        prim "*"  (*) Vnum  Lint,
        prim "/"  div Vnum  Lint,
        prim "="  (==) Vbool Lboo,
        prim ">=" (>=) Vbool Lboo,
        prim "<=" (<=) Vbool Lboo]
       where prim name op cons typ =
              (name,
               Vfun (Just name)
                    (\ (Vnum x) -> Vfun Nothing (\ (Vnum y) -> cons (x `op` y))),
               Larw Lint (Larw Lint typ))

-- Point d'entrée de l'évaluation
eval :: Env -> Lexp -> Value
eval env e =
  -- Extrait la liste des variables et la liste de leur valeurs,
  -- et ignore leurs types, qui n'est plus utile pendant l'évaluation.
  eval2 (map (\(x,_,_) -> x) env) e (map (\(_,v,_) -> v) env)

e2lookup :: [Var] -> Var -> Int          -- Find position within environment
e2lookup env x = e2lookup' env 0
    where e2lookup' :: [Var] -> Int -> Int
          e2lookup' [] _ = error ("Variable inconnue: " ++ show x)
          e2lookup' (x':_) i | x == x' = i
          e2lookup' (_:xs) i = e2lookup' xs (i+1)

-------------- La fonction d'évaluation principale.  ------------------------
-- Au lieu de recevoir une liste de paires (Var, Val), on passe la liste
-- des noms de variables (`senv`) et la liste des valeurs correspondantes
-- (`venv`) séparément de manière à ce que (eval2 senv e) renvoie unev
-- fonction qui a déjà fini d'utiliser `senv`.

eval2 :: [Var] -> Lexp -> ([Value] -> Value)
eval2 _    (Lnum n) = \_ -> Vnum n
eval2 senv (Lhastype e _) = eval2 senv e
eval2 senv (Ltuple a) = \venv -> Vtuple (map (\x -> eval2 senv x venv) a)

eval2 senv (Llet a b) = \venv ->
                    let newName = map fst a ++ senv
                        exp = map snd a
                        venv' = map (\x -> eval2 newName x venv' ) exp ++ venv
                    in eval2 newName b venv'


eval2 senv (Lfetch a b c) = \venv ->
                                    let Vtuple tuple = eval2 senv a venv
                                        newSenv = b ++ senv
                                        newVenv = tuple ++ venv
                                    in eval2 newSenv c newVenv

eval2 senv (Lcall a b) = \venv -> let newB = eval2 senv b venv
                                      function = eval2 senv a venv
                                   in case function of
                                     (Vfun _ a) -> a newB
                                     (a) -> error "Not a function type"

eval2 senv (Lfun a b) = \venv -> Vfun Nothing (\v -> eval2 (a:senv) b (v:venv))

eval2 senv (Lif cond true false) = \venv ->
                              let exprCond = eval2 senv cond venv
                              in case exprCond of
                                (Vbool True) -> eval2 senv true venv
                                (Vbool False) -> eval2 senv false venv

eval2 senv (Lvar x) =
  -- Calcule la position que la variable aura dans `venv`.
  let i = e2lookup senv x in
  -- Renvoie une fonction qui n'a plus besoin de charcher et comparer le nom.
  -- De cette manière, si la fonction renvoyée par (eval2 senv v) est appelée
  -- plusieurs fois, on aura fait la recherche dans `senv` une seule fois.
    \venv -> venv !! i

---------------------------------------------------------------------------
-- Vérificateur de types                                                 --
---------------------------------------------------------------------------

type TEnv = [(Var, Ltype)]
type TypeError = String

-- Les valeurs ne servent à rien pendant la vérification de type,
-- donc extrait la partie utile de `env0`.
tenv0 :: TEnv
tenv0 = (map (\(x,_,t) -> (x,t)) env0)

-- Check type of each declaration of variable, associate them
-- and add them into an array (Not yet support recursive definition)

-- Extract the type of each element in tuple, associate them with variable name
extractTuple :: Ltype -> [Var] -> TEnv
extractTuple (Ltup []) _ = []
extractTuple (Ltup (x:xs)) (y:ys) = (y,x):extractTuple (Ltup xs) ys

tlookup :: [(Var, a)] -> Var -> a
tlookup [] x = error ("Variable inconnue: " ++ x)
tlookup ((x',t):_) x | x == x' = t
tlookup (_:env) x = tlookup env x

infer :: TEnv -> Lexp -> Ltype
infer _ (Lnum _) = Lint
infer tenv (Lvar x) = tlookup tenv x

infer tenv (Ltuple x) = Ltup (checkEach x)
                    where checkEach :: [Lexp] -> [Ltype]
                          checkEach [] = []
                          checkEach (x:xs) = ((infer tenv x):(checkEach xs))

infer tenv (Llet a b) = let tenv' = map(\(a,b) -> (a,infer tenv' b)) a ++ tenv
                        in infer tenv' b

infer tenv (Lhastype a b) = let checkR = check tenv a b
                            in case checkR of
                                (Nothing) -> b
                                (Just a) -> error a

infer tenv (Lcall a b) = let (Larw c d) = infer tenv a
                             result = check tenv b c
                         in case (result) of
                           (Just a) -> error a
                           (Nothing) -> d

infer _ (Lfun _ _)     = error "Can't infer type of `fun`"
infer _ (Lfetch _ _ _) = error "Can't infer type of `fetch`"
infer _ (Lif _ _ _)    = error "Can't infer type of `if`"

--Lhastype (Llet [("even",Lhastype (Lfun "x" (Lif (Lcall (Lcall (Lvar "=") (Lvar "x")) (Lnum 0)) (Lcall (Lcall (Lvar "=") (Lnum 0)) (Lnum 0)) (Lcall (Lvar "odd") (Lcall (Lcall (Lvar "-") (Lvar "x")) (Lnum 1))))) (Larw Lint Lboo)),("odd",Lhastype (Lfun "x" (Lif (Lcall (Lcall (Lvar "=") (Lvar "x")) (Lnum 0)) (Lcall (Lcall (Lvar "=") (Lnum 1)) (Lnum 0)) (Lcall (Lvar "even") (Lcall (Lcall (Lvar "-") (Lvar "x")) (Lnum 1))))) (Larw Lint Lboo))] (Lcall (Lvar "odd") (Lnum 8))) Lboo


check :: TEnv -> Lexp -> Ltype -> Maybe TypeError
check tenv (Lfun x body) (Larw t1 t2) = check ((x,t1):tenv) body t2
check tenv (Lhastype a b) _ = check tenv a b
check tenv (Lif cond true false) t =
                          let condCheck = check tenv cond Lboo
                          in case condCheck of
                            Just a -> error a
                            Nothing ->
                                 let trueType = check tenv true t
                                     falseType = check tenv false t
                                 in case (trueType,falseType) of
                                   (Just a,_) -> Just a
                                   (_,Just a) -> Just a
                                   (Nothing,Nothing) -> Nothing

check tenv (Lfetch a var b) typ = let tuple = infer tenv a
                                      newEnv = extractTuple tuple var ++ tenv
                                  in case tuple of
                                    (Ltup _) -> check newEnv b typ
                                    (a) -> Just ("Type mismatch")

check _ (Lfun _ _) t = Just ("Not a function type: " ++ show t)

check tenv e t =
    -- Essaie d'inférer le type et vérifie alors s'il correspond au
    -- type attendu.
    let t' = infer tenv e
    in if t == t' then Nothing
       else Just ("Type mismatch: " ++ show t ++ " != " ++ show t')

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do filestring <- readFile filename
       (hPutStr stdout)
           (let sexps s = case parse pSexps filename s of
                            Left _ -> [Ssym "#<parse-error>"]
                            Right es -> es
            in (concat
                (map (\ sexp -> let { ltyp = infer tenv0 lexp
                                   ; lexp = s2l sexp
                                   ; val = eval env0 lexp }
                               in "  " ++ show val
                                  ++ " : " ++ show ltyp ++ "\n")
                     (sexps filestring))))

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

typeOf :: String -> Ltype
typeOf = infer tenv0 . lexpOf

valOf :: String -> Value
valOf = eval env0 . lexpOf
