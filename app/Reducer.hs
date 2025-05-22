module Reducer where
import qualified Data.Map as Map
import Common
import Debug.Trace

-- Performs a step of reduction on given state.
rstep :: StateDesc -> Int -> Maybe StateDesc
-- If not beginning of expression.
rstep (StateDesc (e1 :$ e2, ctx) his f inner mapping) args = 
    case rstep (StateDesc (left (e1 :$ e2, ctx)) his f inner mapping) (args + 1) of
      Nothing -> rstep (StateDesc (right (e1 :$ e2, ctx)) his f inner mapping) 0
      Just (StateDesc loc his f inner mapping) -> Just (StateDesc loc his f inner mapping)

rstep (StateDesc (Con name, ctx) his f inner mapping) args = Nothing

-- Beginning of expression.
rstep (StateDesc (Var name, ctx) his f inner mapping) args = 
    -- Check for matches for this expression.
    case Map.lookup name mapping of
        Just (Def (Match name pats expr:xs)) -> if length pats <= args then trace (showLoc (Var name, ctx) ++ "   " ++ show inner ++  " CORE")
                      findReduce (StateDesc (Var name, ctx) his f inner mapping) (Match name pats expr:xs)
                  else Nothing
        -- No matches.
        Nothing -> Nothing
    where

    -- Checks any pattern matches expression in given order. 
    findReduce :: StateDesc -> [Match] -> Maybe StateDesc
    findReduce (StateDesc loc his f inner mapping) (Match _ pats expr:xs) = 
        case matchPats (StateDesc loc his f inner mapping) pats expr Map.empty of
          (Nothing, _) -> trace (show pats ++ " next") findReduce (StateDesc loc his f inner mapping) xs
          (Just new_state, correct_mapping) -> trace (show pats ++" worked " ++ show new_state ++ " " ++ show correct_mapping ++ "  " ++ show expr)
              Just (reduce new_state correct_mapping expr)

    -- No pattern matches
    findReduce state _ = Nothing

    -- Checks every parameter in pattern with corresponding expression.
    matchPats :: StateDesc -> [Pat] -> Expr -> ExprMap -> (Maybe StateDesc, ExprMap)
    matchPats (StateDesc (e1, L cxt e2) his f inner mapping) (pat:ps) expr_result expr_map =
      case checkPattern e2 pat expr_map of
        -- Incorrect matching. Reduce if possible then check again.
        (_, False) -> case forceCheck (StateDesc (right (up (e1, L cxt e2))) his f inner mapping) pat expr_map of
                      (Nothing, _) -> trace ("1 " ++ show pat) (Nothing, expr_map)
                      (Just (StateDesc new_loc new_his new_f _ _), new_mapping) ->
                        case inner of 
                          True ->  trace ("2 " ++ show pat)  matchPats (StateDesc (new_loc) (his <> new_his) new_f inner mapping) ps expr_result new_mapping
                          _ ->  trace ("3 " ++ showLoc new_loc ++ " 4") matchPats (StateDesc (new_loc) (his <> new_his) f inner mapping) ps expr_result new_mapping

        -- Correctly matched parameter. Go to the next one.
        (new_mapping, True) -> matchPats (StateDesc (up (e1, L cxt e2)) his f inner mapping) ps expr_result new_mapping

    -- All parameters matched.    
    matchPats state [] _ expr_map = (Just state, expr_map)

    -- Something went wrong.
    matchPats _ _ _ expr_map = (Nothing, expr_map)

    forceCheck :: StateDesc -> Pat -> ExprMap -> (Maybe StateDesc, ExprMap)
    forceCheck (StateDesc (e1, R e2 cxt) his f inner mapping) pat expr_map = 
      let state = if not inner 
          then trace (show e1 ++ " in force") StateDesc (e1, R e2 cxt) mempty internal_steps True mapping
          else trace (show e1 ++ " in force inner " ++ show e2) StateDesc (e1, R e2 cxt) his f inner mapping
      in
      case rstep state 0 of
        Nothing -> trace (show pat ++ " 33 ")(Nothing, expr_map) 
        Just (StateDesc (new_arg, new_cxt) new_his new_f new_inner _) ->
          case checkPattern new_arg pat expr_map of
            (new_expr_mapping, True) -> trace (show pat ++ " 22 " ++ show new_expr_mapping ++ "  " ++ showLoc (new_arg, new_cxt)) (Just (StateDesc (up (new_arg, new_cxt)) new_his new_f new_inner mapping), new_expr_mapping)
            (_, False) -> case new_f of
                            0 -> (Nothing, expr_map) 
                            _ -> trace (show pat ++ " 11 " ++ show new_arg ++ "   " ++ show (new_cxt)) forceCheck (StateDesc (new_arg, new_cxt) new_his new_f new_inner mapping) pat expr_map
    
    -- Reduces given expression and adds new history.
    reduce :: StateDesc -> ExprMap -> Expr -> StateDesc                   
    reduce (StateDesc (e1, ctx) his f inner mapping) e_mapping template = 
      let reduced = reduction template e_mapping 
      in
      move_up (StateDesc (reduced, ctx) (snoc his (reduced, ctx)) (f - 1) inner mapping)
    
    reduction :: Expr -> ExprMap -> Expr
    reduction (a :$ b) small_map = reduction a small_map :$ reduction b small_map
    reduction (Var name) small_map = case Map.lookup name small_map of
                                       Nothing -> Var name
                                       Just val -> val
    reduction (Con name) _ = Con name

    move_up :: StateDesc -> StateDesc
    move_up (StateDesc (ex, L a b) his f inner mapping) = move_up (StateDesc (up (ex, L a b)) his f inner mapping)
    move_up (StateDesc loc his f inner mapping)  = trace ("!!!!!!!!!!  " ++ showLoc loc)(StateDesc loc his f inner mapping)

-- Checks if pattern matches for expr. If the pattern is correct: passes correct mapping.  TODO (maybe change it into zipper.)

checkPattern :: Expr -> Pat -> ExprMap -> (ExprMap, Bool)
checkPattern e pat maps = comp (expToLst e []) pat maps
  where
  -- Compares one parameter with expression.
  comp :: [Expr] -> Pat -> ExprMap -> (ExprMap, Bool) 
  comp (Con e:es) (PApp name lst) mapping = if e == name && length es == length lst then compLsts es lst mapping
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
  expToLst e lst = e:lst

  lstToExp :: Expr -> [Expr] -> Expr
  lstToExp e0 (e1:e2) = lstToExp (e0 :$ e1) e2
  lstToExp e _ = e


performSteps (StateDesc loc his fuel inner mapping) = 
  case rstep (StateDesc loc his fuel inner mapping) 0 of
    Nothing -> printStory his
    Just (StateDesc o_loc o_his o_fuel _ _) -> 
      case o_fuel of
        0 -> printStory o_his
        _ -> trace ("next move " ++ showLoc (getTop o_loc) ++ "  " ++ show o_fuel ++ "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!") performSteps (StateDesc (getTop o_loc) o_his o_fuel inner mapping)


-- source for unlines: https://www.reddit.com/r/haskell/comments/msfnc6/printing_string_on_new_line/
printStory history = putStrLn (unlines (map showLoc (toList history))) -- TODO CHANGE PRINT 

