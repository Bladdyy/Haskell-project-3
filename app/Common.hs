module Common where
import qualified Data.Map as Map


type Name = String
data Def = Def { defMatches :: [Match] } deriving Show

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

steps :: Int
steps = 29


instance Show Pat where
    show (PVar name) = name
    show (PApp name pats) =
        name ++ "(" ++ unwords (name : map show pats) ++ ")"

instance Show Match where
    show (Match name pats rhs) =
        name ++ " " ++ unwords (map show pats) ++ " = " ++ show rhs



instance Show Expr where
    showsPrec _ (Var name) = showString name
    showsPrec _ (Con name) = showString name
    showsPrec p (e1 :$ e2) =
        showParen (p > 10) (showsPrec 9 e1 . showString " " . showsPrec 11 e2)


-- Inputs Match list into Map checking for duuplicates of main and different argument numbers.
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