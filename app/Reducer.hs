module Reducer where
import Data.List (intercalate)
import qualified Data.Map as Map
import Common

-- Performs a step of reduction on given state.
rstep :: StateDesc -> Int -> Maybe StateDesc
-- If not beginning of expression.
rstep (StateDesc (e1 :$ e2, ctx) his f inner' mapping) args = 
    case rstep (StateDesc (left (e1 :$ e2, ctx)) his f inner' mapping) (args + 1) of
      Nothing -> rstep (StateDesc (right (e1 :$ e2, ctx)) his f inner' mapping) 0
      Just (StateDesc loc' his' f' i' mapping') -> Just (StateDesc loc' his' f' i' mapping')

rstep (StateDesc (Con _, _) _ _ _ _) _ = Nothing

-- Beginning of expression.
rstep (StateDesc (Var name, ctx) his f inner' mapping) args = 
    -- Check for matches for this expression.
    case Map.lookup name mapping of
        Just (Def (Match n pats expr:xs)) -> 
            if length pats <= args then
              findReduce (StateDesc (Var name, ctx) his f inner' mapping) (Match n pats expr:xs)
            else Nothing
        -- No matches.
        _ -> Nothing
  where

  -- Checks any pattern matches expression in given order. 
  findReduce :: StateDesc -> [Match] -> Maybe StateDesc
  findReduce (StateDesc loc his' f' inn' mapping') (Match _ pats expr:xs) = 
      case matchPat (StateDesc loc his' f' inn' mapping') pats expr Map.empty of
        (Nothing, _) -> findReduce (StateDesc loc his' f' inn' mapping') xs
        (Just new_state, correct_mapping) -> Just (reduce new_state correct_mapping expr)

  -- No pattern matches
  findReduce _ _ = Nothing

  -- Checks every parameter in pattern with corresponding expression.
  matchPat :: StateDesc -> [Pat] -> Expr -> ExprMap -> (Maybe StateDesc, ExprMap)
  matchPat (StateDesc (e1, L cxt e2) hi fu i ma) (pat:ps) expr_result emap =
    case checkPattern e2 pat emap of
      -- Incorrect matching. Reduce if possible then check again.
      (_, False) -> 
        case forceCheck (StateDesc (right (up (e1, L cxt e2))) hi fu i ma) pat emap of
          (Nothing, _) -> (Nothing, emap)

          (Just (StateDesc new_loc new_his new_f _ _), new_mapping) ->
            case i of 
              True -> matchPat (StateDesc (new_loc) (hi <> new_his) new_f i ma) ps expr_result new_mapping
              _ -> matchPat (StateDesc (new_loc) (hi <> new_his) fu i ma) ps expr_result new_mapping

      -- Correctly matched parameter. Go to the next one.
      (new_mapping, True) -> matchPat (StateDesc (up (e1, L cxt e2)) his f i mapping) ps expr_result new_mapping

  -- All parameters matched.    
  matchPat state' [] _ emap = (Just state', emap)

  -- Something went wrong.
  matchPat _ _ _ emap = (Nothing, emap)

  forceCheck :: StateDesc -> Pat -> ExprMap -> (Maybe StateDesc, ExprMap)
  forceCheck (StateDesc (e1, R e2 cxt) hi' fu' i' ma') pat emap' = 
    let state' = if not i'
        then StateDesc (e1, R e2 cxt) mempty internal_steps True ma'
        else StateDesc (e1, R e2 cxt) hi' fu' i' ma'
    in
    case rstep state' 0 of
      Nothing -> (Nothing, emap') 
      Just (StateDesc (new_arg, new_cxt) new_his new_f new_inner _) ->
        case checkPattern new_arg pat emap' of
          (new_expr_mapping, True) -> (Just (StateDesc (up (new_arg, new_cxt)) new_his new_f new_inner ma'), new_expr_mapping)
          (_, False) -> case new_f of
                          0 -> (Nothing, emap') 
                          _ -> forceCheck (StateDesc (new_arg, new_cxt) new_his new_f new_inner ma') pat emap'
  forceCheck _ _ emap' = (Nothing, emap')
  -- Reduces given expression and adds new history.
  reduce :: StateDesc -> ExprMap -> Expr -> StateDesc                   
  reduce (StateDesc (_, ctx') hi fu i ma) emap template = 
    let reduced = reduction template emap 
    in
    move_up (StateDesc (reduced, ctx') (snoc hi (reduced, ctx')) (fu - 1) i ma)
  
  reduction :: Expr -> ExprMap -> Expr
  reduction (a :$ b) small_map = reduction a small_map :$ reduction b small_map
  reduction (Var name') small_map = case Map.lookup name' small_map of
                                     Nothing -> Var name'
                                     Just val -> val
  reduction (Con name') _ = Con name'

  move_up :: StateDesc -> StateDesc
  move_up (StateDesc (ex, L a b) hi' fu' i' ma') = move_up (StateDesc (up (ex, L a b)) hi' fu' i' ma')
  move_up state' = state'

-- Checks if pattern matches for expr. If the pattern is correct: passes correct mapping.  TODO (maybe change it into zipper.)

checkPattern :: Expr -> Pat -> ExprMap -> (ExprMap, Bool)
checkPattern e pat maps = comp (expToLst e []) pat maps
  where
  -- Compares one parameter with expression.
  comp :: [Expr] -> Pat -> ExprMap -> (ExprMap, Bool) 
  comp (Con ex:es) (PApp name lst) mapping = if ex == name && length es == length lst then compLsts es lst mapping
                                            else (mapping, False)
  comp (x:xs) (PVar name) mapping = (Map.insert name (lstToExp x xs) mapping, True)
  comp _ _ mapping = (mapping, False) 

  -- Compares a whole list of parameters inside of a parameter.
  compLsts :: [Expr] -> [Pat] -> ExprMap -> (ExprMap, Bool)
  compLsts (x:xs) (y:ys) mapping = case checkPattern x y mapping of 
                               (new_mapping, False) -> (new_mapping, False)
                               (new_mapping, True) -> compLsts xs ys new_mapping
  compLsts _ _ mapping = (mapping, True)

  -- Changes Expr to list.
  expToLst :: Expr -> [Expr] -> [Expr]
  expToLst (e1 :$ e2) lst = expToLst e1 (e2:lst)
  expToLst ex lst = ex:lst

  lstToExp :: Expr -> [Expr] -> Expr
  lstToExp e0 (e1:e2) = lstToExp (e0 :$ e1) e2
  lstToExp ex _ = ex

performSteps :: StateDesc -> IO()
performSteps (StateDesc loc his fue inne mapping) = 
  case rstep (StateDesc loc his fue inne mapping) 0 of
    Nothing -> printStory his
    Just (StateDesc o_loc o_his o_fuel _ _) -> 
      case o_fuel of
        0 -> printStory o_his
        _ -> performSteps (StateDesc (getTop o_loc) o_his o_fuel inne mapping)


-- source for unlines: https://stackoverflow.com/questions/5289779/printing-elements-of-a-list-on-new-lines
-- I wanted to use unlines from link above but it was adding "\n" at the end so i found:
-- https://www.reddit.com/r/haskell/comments/4vctyi/intercalation_inconsistency/
-- intercalate inserts wanted string in between every two elements in the list.
printStory :: SnocList Loc -> IO ()
printStory story = putStrLn (intercalate "\n" (map showLoc (toList story))) -- TODO CHANGE PRINT 

