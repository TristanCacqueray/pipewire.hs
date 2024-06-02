module Main (main) where

import Pipewire qualified as PW

main :: IO ()
main = PW.withInstance () printEvent \pwInstance -> do
    print =<< PW.pw_main_loop_run pwInstance.mainLoop
  where
    printEvent _ ev _ = do
        case ev of
            PW.Added pwid name props -> do
                putStrLn $ "added: " <> show pwid
                putStrLn $ " type: " <> show name
                mapM_ (putStrLn . mappend " " . show) =<< PW.spaDictRead props
            PW.Removed pwid -> do
                putStrLn $ "removed: " <> show pwid
        putStrLn ""
