module Main where

-- import Control.Monad.Loops
import Control.Monad.RWS.Strict
import Prelude                  hiding (init)
-- import System.IO

-- import Coq.StateId
import Coq.Value
import Coq.Goals
import Coq.Goal
import CoqIO
import Handlers
import XMLProtocol

coqtop :: String
coqtop = "/usr/local/Cellar/coq/8.5/bin/coqtop -ideslave -main-channel stdfds"

example_file :: String
example_file = "example.v"


foreground_goals :: Goals -> [Goal]
foreground_goals (MkGoals (MkPreGoal gs _ _ _)) = gs

print_a_goal :: Goal -> IO ()
print_a_goal (MkGoal _id contexts goal) = do
    forM_ contexts $ \c ->
        putStrLn  c
    putStrLn "========================"
    putStrLn goal


print_goals :: Goals -> Maybe Int -> IO ()
print_goals gs max_goals = print_goals' (foreground_goals gs) max_goals

-- Print goals shows the first `max_goals` along with their contexts.
print_goals' :: [Goal] -> Maybe Int -> IO ()
print_goals' [] max_goals = putStrLn "0 subgoals"
print_goals' (open_goal:gs) max_goals = do

    let (MkGoal _id contexts goal) = open_goal
    putStrLn ""
    forM_ contexts $ \c ->
        putStrLn $ replace_spaces c
    putStrLn $ "======================== ( 1 / " ++ show num_goals ++ " ) "
    putStrLn $ replace_spaces goal
    putStrLn ""

    forM_ (zip [2..goals_shown] gs) $ \(i,g) -> do
        let (MkGoal _id contexts goal) = g
        putStrLn $ "======================== ( " ++ show i ++ " / " ++ show num_goals ++ " ) "
        putStrLn $ replace_spaces goal
        putStrLn ""

  where num_goals = 1 + length gs
        goals_shown = maybe num_goals id max_goals


match_print_goals (ValueGood (Just goals)) max_goals = liftIO $ print_goals goals max_goals
match_print_goals _ _ = return ()

should_add_context :: String -> Bool
should_add_context line = last line == '.'
                        && line /= "Qed."
                        && take 7 line /= "Theorem"
                        && take 5 line /= "Proof"

replace_spaces :: String -> String
replace_spaces line = map (\c -> if c == ' ' then ' ' else c) line

main :: IO ()
main = do
  hs <- startCoqtop coqtop
  coq_file <- readFile example_file
  -- putStrLn coq_file
  void $ runRWST (io hs coq_file) hs initialCoqState
  where
    io :: Handles -> String -> CoqtopIO ()
    io (_, _, _he, _ph) coq_file = do
      forM_ (lines coq_file) $ \line_with_spaces -> do
        let line = replace_spaces line_with_spaces

        if should_add_context line then do -- A smarter check is in order.
            liftIO $ putStrLn "(*context"
            g <- goal'
            match_print_goals g (Just 1)

            liftIO $ putStrLn "<br />"
            liftIO $ putStrLn ""
            liftIO $ putStrLn line
            liftIO $ putStrLn ""
            liftIO $ putStrLn "<br />"

            add' line
            g <- goal'
            match_print_goals g Nothing
            liftIO $ putStrLn "*)"
        else -- Just add the line, don't print anything.
            void $ add' line

        liftIO $ putStrLn line

      return (ValueGood ())
      {-
                -}

{-
      sid <- getStateId
      eid <- newEditID
      r <- hCallRawResponse "Add" (("Import ListNotations.", eid), (sid, False))
      liftIO $ do
        putStrLn $ "Raw response: " ++ r
        --putStrLn $ show (r :: Value AddOutput)
        exit <- waitForProcess ph
        putStrLn $ show exit
      return (ValueGood ())
-}
