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
        liftIO $ putStrLn ""
        liftIO $ putStrLn line
        liftIO $ putStrLn ""
        add' line
        g <- goal'
        case g of
            ValueGood (Just (MkGoals (MkPreGoal gs g _ _))) -> liftIO $ do
                putStrLn "(*context"
                forM_ gs $ \x -> case x of
                    MkGoal _id contexts goal -> do
                        putStrLn ""
                        forM_ contexts $ \c ->
                            putStrLn  c
                        putStrLn "===================="
                        putStrLn goal
                        putStrLn ""
                putStrLn "*)"
            _ -> return ()

      return (ValueGood ())
      {-
        case g of
            ValueGood (Just (MkGoals (MkPreGoal
               ((MkGoal _id contexts goal):gs) g _ _))) -> liftIO $ do
                putStrLn "(*context"
                forM_ contexts $ \c ->
                    putStrLn  c
                putStrLn "===================="
                putStrLn goal
                putStrLn "*)"
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
