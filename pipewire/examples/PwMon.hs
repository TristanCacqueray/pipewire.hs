module Main (main) where

import Pipewire qualified as PW

main :: IO ()
main = PW.withInstance () \_ pwInstance -> do
    PW.withRegistryHandler pwInstance printEvent do
        print =<< PW.runInstance pwInstance
  where
    printEvent ev = do
        case ev of
            PW.Added changed pwid name props -> do
                case changed of
                    False -> putStrLn $ "added: " <> show pwid
                    True -> putStrLn $ "changed: " <> show pwid
                putStrLn $ " type: " <> show name
                printProps props
            PW.Removed pwid -> do
                putStrLn $ "removed: " <> show pwid
        putStrLn ""
    printProps props = mapM_ (putStrLn . mappend " " . show) =<< PW.spaDictRead props
