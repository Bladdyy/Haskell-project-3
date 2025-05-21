module Common where
import qualified Data.Map as Map
import Control.Applicative (Alternative(..))

type Name = String
data Def = Def { defMatches :: [Match] }

data Match = Match  
    { matchName :: Name
    , matchPats :: [Pat]
    , matchRhs  :: Expr
    }

infixl 9 :$
data Expr
    = Var Name
    | Con Name
    | Expr :$ Expr
data Pat = PVar Name | PApp Name [Pat]

type DefMap = Map.Map Name Def
type ExprMap = Map.Map Name Expr


steps, internal_steps :: Int
steps = 29
internal_steps = 15

-- State description

data StateDesc = StateDesc
  { state     :: Loc
  , history   :: SnocList Loc
  , fuel      :: Int
  , inner     :: Bool
  , defs      :: DefMap
  }


instance Show Expr where
    showsPrec _ (Var name) = showString name
    showsPrec _ (Con name) = showString name
    showsPrec p (e1 :$ e2) =
        showParen (p > 10) (showsPrec 9 e1 . showString " " . showsPrec 11 e2)



-- Inputs Match list into Map checking for duplicates of main and different parameter number.
matchToMap :: [Match] -> DefMap
matchToMap lst = foldr change Map.empty lst
    where
    change (Match name pats expr) mapping = 
        case Map.lookup name mapping of
          Nothing -> Map.insert name (Def [Match name pats expr]) mapping
          Just (Def (Match name' pats' expr':xs)) -> 
                if name == "main" then
                  error "ERROR: Multiple definitions of main."
                else if length pats == length pats' then
                  Map.insert name (Def (Match name pats expr : Match name pats' expr' : xs)) mapping
                else
                  error ("ERROR: Multiple definitions of " ++ name 
                          ++ " have different number of arguments.")


-- SnocList definitions.
newtype SnocList a = SnocList {unSnocList :: [a]}

toList :: SnocList a -> [a]
toList (SnocList lst) = reverse lst

fromList :: [a] -> SnocList a 
fromList lst = SnocList (reverse lst)

snoc :: SnocList a -> a -> SnocList a
snoc (SnocList lst) el = SnocList (el : lst) 

instance Eq a => Eq (SnocList a) where
  xs == ys = toList xs == toList ys

instance Show a => Show (SnocList a) where
  show xs = show (toList xs)

instance Semigroup (SnocList a) where
  SnocList xs <> SnocList ys = SnocList (ys ++ xs)

instance Monoid (SnocList a) where
  mempty = SnocList []

instance Functor SnocList where
  fmap f (SnocList xs) = SnocList (map f xs)

instance Applicative SnocList where
  pure x = SnocList [x]
  SnocList fs <*> SnocList xs = SnocList [f x | f <- fs, x <- xs]

instance Alternative SnocList where
    empty = SnocList []
    SnocList xs <|> SnocList ys = SnocList (xs ++ ys)

-- Expr Zipper

data Cxt = Top | L Cxt Expr | R Expr Cxt

type Loc = (Expr, Cxt)

top :: Expr -> Loc
top e = (e, Top)

left, right, up :: Loc -> Loc
left  (e1 :$ e2, c) = (e1, L c e2)
left loc = loc

right (e1 :$ e2, c) = (e2, R e1 c)
right loc = loc

up (e, L c r) = (e :$ r, c)
up (e, R l c) = (l :$ e, c)
up loc = loc


getCxt :: Loc -> Cxt
getCxt l = snd l

getExpr :: Loc -> Expr
getExpr (t, Top) = t
getExpr loc = getExpr (up loc)            

modifyHere :: (Expr -> Expr) -> Loc -> Loc
modifyHere f (e, c) = (f e, c) 

showLoc :: Loc -> String
showLoc (e, c) = unwrap c (showString "{" . shows e . showString "}") False
  where
    unwrap :: Cxt -> ShowS -> Bool -> String
    unwrap Top s _ = s ""
    unwrap (L ctx r) s _ = case r of
                              _ :$ _ -> unwrap ctx (s . showString " (" . shows r . showChar ')') True
                              _ -> unwrap ctx (s . showChar ' ' . shows r) True
    unwrap (R l ctx) s par = case par of
                              True -> unwrap ctx (shows l . showChar ' ' . showChar '(' . s . showChar ')') True
                              False -> unwrap ctx (shows l . showChar ' ' . s) True