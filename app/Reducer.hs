module Reducer where
import qualified Data.Map as Map
import Common

-- Performs a step of reduction on given state.
rstep :: StateDesc -> Int -> Maybe StateDesc
-- If not beginning of expression.
rstep (StateDesc (e1 :$ e2, ctx) his f inner mapping) args = 
    rstep (StateDesc (left (e1 :$ e2, ctx)) his f inner mapping) (args + 1)

-- Beginning of expression.
rstep (StateDesc (Var name, ctx) his f inner mapping) args = 
    -- Check for matches for this expression.
    case Map.lookup name mapping of
        Just (Def xs) -> if length xs >= args then 
                      findReduce (StateDesc (Var name, ctx) his f inner mapping) xs
                  else Nothing
        -- No matches.
        Nothing -> Nothing
    where

    -- Checks any pattern matches expression in given order. 
    findReduce :: StateDesc -> [Match] -> Maybe StateDesc
    findReduce state (Match _ pats expr:xs) = 
        case matchPats state pats expr Map.empty of
          (Nothing, _) -> findReduce state xs
          (Just state, correct_mapping) -> 
              Just (reduce state correct_mapping expr)

    -- No pattern matches
    findReduce state _ = Nothing

    -- Checks every parameter in pattern with corresponding expression.
    matchPats :: StateDesc -> [Pat] -> Expr -> ExprMap -> (Maybe StateDesc, ExprMap)
    matchPats (StateDesc (e1, L cxt e2) his f inner mapping) (pat:ps) expr_result expr_map =
      case checkPattern e2 pat expr_map of
        -- Incorrect matching. Reduce if possible then check again.
        (_, False) -> case forceCheck (StateDesc (right (up (e1, L cxt e2))) his f inner mapping) pat expr_map of
                      (Nothing, _) -> (Nothing, expr_map)
                      (Just (StateDesc new_loc new_his new_f _ _), new_mapping) ->
                        case inner of 
                          True -> matchPats (StateDesc (up new_loc) (new_his <> his) new_f inner mapping) ps expr_result new_mapping
                          _ -> matchPats (StateDesc (up new_loc) (new_his <> his) f inner mapping) ps expr_result new_mapping

        -- Correctly matched parameter. Go to the next one.
        (new_mapping, True) -> matchPats (StateDesc (up (e1, L cxt e2)) his f inner mapping) ps expr_result new_mapping

    -- All parameters matched.    
    matchPats state [] _ expr_map = (Just state, expr_map)

    -- Something went wrong.
    matchPats _ _ _ expr_map = (Nothing, expr_map)

    forceCheck :: StateDesc -> Pat -> ExprMap -> (Maybe StateDesc, ExprMap)
    forceCheck (StateDesc (e1, R e2 cxt) his f inner mapping) pat expr_map = 
      let state = if not inner 
          then (StateDesc (e1, R e2 cxt) mempty internal_steps True mapping)
          else (StateDesc (e1, R e2 cxt) his f inner mapping)
      in
      case rstep state 0 of
        Nothing -> (Nothing, expr_map) 
        Just (StateDesc (new_arg, _) new_his new_f new_inner _) -> 
          case checkPattern new_arg pat expr_map of
            (new_expr_mapping, True) -> (Just (StateDesc (up (new_arg, R e2 cxt)) new_his new_f new_inner mapping), new_expr_mapping)
            (_, False) -> forceCheck (StateDesc (new_arg, R e2 cxt) new_his new_f new_inner mapping) pat expr_map

    
    -- Reduces given expression and adds new history.
    reduce :: StateDesc -> ExprMap -> Expr -> StateDesc                   
    reduce (StateDesc (e1, ctx) his f inner mapping) e_mapping template = 
      let reduced = reduction template e_mapping 
      in
      move_up (StateDesc (reduced, ctx) (snoc his (reduced, ctx)) f inner mapping)
    
    reduction :: Expr -> ExprMap -> Expr
    reduction (a :$ b) small_map = reduction a small_map :$ reduction b small_map
    reduction (Var name) small_map = case Map.lookup name small_map of
                                       Nothing -> Var name
                                       Just val -> val
    reduction (Con name) _ = Con name

    move_up :: StateDesc -> StateDesc
    move_up (StateDesc (ex, L a b) his f inner mapping) = move_up (StateDesc (up (ex, L a b)) his f inner mapping)
    move_up state = state

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


perform_steps (StateDesc loc his fuel inner mapping) = 
  case rstep (StateDesc loc his fuel inner mapping) 0 of
    Nothing -> print (map showLoc (toList his))
    Just (StateDesc o_loc o_his o_fuel _ _) -> 
      case o_fuel of
        0 -> print (map showLoc (toList o_his))
        _ -> perform_steps (StateDesc o_loc o_his o_fuel inner mapping)



