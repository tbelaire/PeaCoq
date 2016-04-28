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
        putStrLn  c
    putStrLn $ "======================== ( 1 / " ++ show num_goals ++ " ) "
    putStrLn goal
    putStrLn ""

    forM_ (zip [2..goals_shown] gs) $ \(i,g) -> do
        let (MkGoal _id contexts goal) = g
        putStrLn $ "======================== ( " ++ show i ++ " / " ++ show num_goals ++ " ) "
        putStrLn goal
        putStrLn ""

  where num_goals = 1 + length gs
        goals_shown = maybe num_goals id max_goals

print_current_goal :: Goals -> IO ()
print_current_goal (MkGoals (MkPreGoal (g:_) _ _ _)) = do
    print_a_goal g
print_goal _ = return ()


match_print_goals (ValueGood (Just goals)) max_goals = liftIO $ print_goals goals max_goals
match_print_goals _ _ = return ()

main :: IO ()
main = do
  hs <- startCoqtop coqtop
  coq_file <- readFile example_file
  -- putStrLn coq_file
  void $ runRWST (io hs coq_file) hs initialCoqState
  where
    io :: Handles -> String -> CoqtopIO ()
    io (_, _, _he, _ph) coq_file = do
      forM_ (lines coq_file) $ \line -> do
        liftIO $ putStrLn line
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
