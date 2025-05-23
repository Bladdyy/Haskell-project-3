module Reducer where
import qualified Data.Map as Map
import Data.List (intercalate)
import Common


-- Performs a step of reduction on given state.
rstep :: StateDesc -> Int -> Maybe StateDesc
-- If not beginning of expression.
rstep (StateDesc (e1 :$ e2, ctx) hi fu ir ma) args = 
    case rstep (StateDesc (left (e1 :$ e2, ctx)) hi fu ir ma) (args + 1) of
      -- Left reduction was not succesful.
      Nothing -> rstep (StateDesc (right (e1 :$ e2, ctx)) hi fu ir ma) 0
      -- Left reduction was succesful.
      Just state' -> Just state'

rstep (StateDesc (Con _, _) _ _ _ _) _ = Nothing

-- Beginning of expression.
rstep (StateDesc (Var nam, ctx) hi fu ir ma) args = 
    -- Check for matches for this expression.
    case Map.lookup nam ma of
        Just (Def (Match nam' pats expr:xs)) -> if length pats <= args then
                      findReduce (StateDesc (Var nam, ctx) hi fu ir ma) (Match nam' pats expr:xs)
                  else Nothing
        -- No matches.
        _ -> Nothing
    where

    -- Checks any pattern matches expression in given order. 
    findReduce :: StateDesc -> [Match] -> Maybe StateDesc
    findReduce (StateDesc loc' hi' fu' ir' ma') (Match _ pats' expr':xs') = 
        case matchPat (StateDesc loc' hi' fu' ir' ma') pats' expr' Map.empty of
          -- Next pattern needed.
          (Nothing, _) -> findReduce (StateDesc loc' hi' fu' ir' ma') xs'
          -- Expression matches current patter.
          (Just new_state, correct_mapping) -> 
              Just (reduce new_state correct_mapping expr')

    -- No pattern matches
    findReduce _ _ = Nothing

    -- Checks every parameter in pattern with corresponding expression.
    matchPat :: StateDesc -> [Pat] -> Expr -> ExprMap -> (Maybe StateDesc, ExprMap)
    matchPat (StateDesc (e1', L cxt' e2') hi' fu' ir' ma') (pat':ps') eres' emap' =
      case checkPattern (e2', Top) pat' emap' 0 of
      -- Incorrect matching. Reduce argument expression if possible then check again.
        (_, False) -> 
          case forceCheck (StateDesc (right (up (e1', L cxt' e2'))) hi' fu' ir' ma') pat' emap' of
            -- Couldn't match despite reducing argument.
            (Nothing, _) -> (Nothing, emap')
            -- Matched after additional reductions.
            (Just (StateDesc new_loc new_his new_f _ _), new_mapping) ->
              case ir' of 
                -- If inner then save inner fuel.
                True -> matchPat (StateDesc (new_loc) (hi' <> new_his) new_f ir' ma')
                                 ps' eres' new_mapping
                _ -> matchPat (StateDesc (new_loc) (hi' <> new_his) fu' ir' ma') 
                              ps' eres' new_mapping

        -- Correctly matched parameter. Go to the next one.
        (new_mapping, True) -> matchPat (StateDesc (up (e1', L cxt' e2')) hi' fu' ir' ma') 
                                        ps' eres' new_mapping

    -- All parameters matched.    
    matchPat state' [] _ emap' = (Just state', emap')

    -- Something went wrong.
    matchPat _ _ _ emap' = (Nothing, emap')

  -- Reduces argument then checks if finally fits given parameter pattern.
    forceCheck :: StateDesc -> Pat -> ExprMap -> (Maybe StateDesc, ExprMap)
    forceCheck (StateDesc (e1', R e2' cxt') hi' fu' ir' ma') pat' emap' = 
      let state' = if not ir' 
        -- Not inner. Going into expression argument for the first time. New fuel and history.
          then StateDesc (e1', R e2' cxt') mempty internal_steps True ma'
        -- Inner already. Pass current fuel and history.
          else StateDesc (e1', R e2' cxt') hi' fu' ir' ma'
      in
    -- Try to reduce.
      case rstep state' 0 of
        -- Reduction not possible.
        Nothing -> (Nothing, emap') 
        -- Reduction was succesful. 
        Just (StateDesc (new_arg', new_cxt') new_his' new_f' new_inner' _) ->
          -- Check if fits after reduction.
          case checkPattern (new_arg', Top) pat' emap' 0 of
            -- Reduced argument fits parameter pattern.
            (new_expr_mapping, True) -> (Just (StateDesc (up (new_arg', new_cxt')) 
                                              new_his' new_f' new_inner' ma'), new_expr_mapping)
          -- No match.
            (_, False) -> case new_f' of
                            -- If there is no more fuel to reduce again.
                            0 -> (Nothing, emap') 
                            -- Enough fuel to try to reduce again.
                            _ -> forceCheck (StateDesc (new_arg', new_cxt')
                                            new_his' new_f' new_inner' ma') pat' emap'
    
    forceCheck _ _ emap' = (Nothing, emap')
    

    -- Reduces given expression and adds new history.
    reduce :: StateDesc -> ExprMap -> Expr -> StateDesc                   
    reduce (StateDesc (_, ctx') hi' fu' ir' ma') emap' template' = 
      let reduced = reduction template' emap' 
      in
      move_up (StateDesc (reduced, ctx') (snoc hi' (reduced, ctx')) (fu' - 1) ir' ma')
    
    -- Performs reduction.
    reduction :: Expr -> ExprMap -> Expr
    reduction (a :$ b) small_map = reduction a small_map :$ reduction b small_map
    reduction (Var name') small_map = case Map.lookup name' small_map of
                                       Nothing -> Var name'
                                       Just val -> val
    reduction (Con name') _ = Con name'

    -- Moves expression up until its R or TOP. 
    move_up :: StateDesc -> StateDesc
    move_up (StateDesc (ex, L a b) hi' fu' ir' ma') = move_up (StateDesc (up (ex, L a b))
                                                              hi' fu' ir' ma')
    move_up state' = state'


-- Checks if pattern matches for expr. If the pattern is correct: passes correct mapping.
checkPattern :: Loc -> Pat -> ExprMap -> Int -> (ExprMap, Bool)
checkPattern (e1 :$ e2, ctx) pat maps args = checkPattern (left (e1 :$ e2, ctx)) pat maps (args + 1)
checkPattern e pat maps args = comp e pat maps args
  where
  -- Compares one parameter with expression.
  comp :: Loc -> Pat -> ExprMap -> Int -> (ExprMap, Bool) 
  -- If Con then check if name and number of arguments are the same.
  comp (Con ex, ctx') (PApp name lst) mapping args' = if ex == name && args' == length lst 
                                            -- Compare arguments.
                                            then compLsts (up (Con ex, ctx')) lst mapping
                                            -- No match.
                                            else (mapping, False)
  -- If Var then map whole expression to it.
  comp ex (PVar name) mapping _ = (Map.insert name (getExpr ex) mapping, True)
  comp _ _ mapping _ = (mapping, False) 

  -- Compares a whole list of parameters inside of a parameter.
  compLsts :: Loc -> [Pat] -> ExprMap -> (ExprMap, Bool)
  compLsts (ex :$ es, ctx') (y:ys) mapping = case checkPattern (es, Top) y mapping 0 of 
                               (new_mapping, False) -> (new_mapping, False)
                               (new_mapping, True) -> compLsts (up (ex :$ es, ctx')) ys new_mapping
  compLsts _ _ mapping = (mapping, True)



-- Performs steps until there are no reductions or there is no fuel left.
performSteps :: StateDesc -> IO()
performSteps (StateDesc l h f i m) = 
  case rstep (StateDesc l h f i m) 0 of
    -- Reduction failed.
    Nothing -> printStory h
    -- Reduced succesfully.
    Just (StateDesc o_loc o_his o_fuel _ _) -> 
      -- Check for fuel.
      case o_fuel of
        0 -> printStory o_his
        _ -> performSteps (StateDesc (getTop o_loc) o_his o_fuel i m)



-- Source for unlines: 
-- https://stackoverflow.com/questions/5289779/printing-elements-of-a-list-on-new-lines
-- I wanted to use unlines from link above but it was adding "\n" at the end so i found:
-- https://www.reddit.com/r/haskell/comments/4vctyi/intercalation_inconsistency/
-- intercalate inserts wanted string in between every two elements in the list.

-- Print whole history of reduction in new lines.
printStory :: SnocList Loc -> IO ()
printStory story = putStrLn (intercalate "\n" (map showLoc (toList story)))