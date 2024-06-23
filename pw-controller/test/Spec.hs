module Main where

import Data.Text.Lazy.Encoding qualified as LText
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase)
import Text.Pretty.Simple (pShowNoColor)

import Pipewire.RegistryState as Pw
import PwController.Eval
import PwController.Expr
import PwController.NodePorts
import PwController.Parser (parseConfig)

main :: IO ()
main = defaultMain $ testGroup "Tests" [configTests, evalTests]

evalTests, configTests :: TestTree
evalTests =
    testGroup
        "Eval"
        [ testCase "link pairs" testGetLinkPairs
        ]
configTests = testGroup "Config" $ map doGoldenTest ["demo", "empty", "syntax"]

doGoldenTest :: String -> TestTree
doGoldenTest test = goldenVsString fp (name <> ".golden") (LText.encodeUtf8 <$> doCheck)
  where
    name = "test/config/" <> test
    fp = name <> ".conf"
    doCheck = pShowNoColor <$> parseConfig fp

testGetLinkPairs :: IO ()
testGetLinkPairs = do
    case getLinkPairs Output reaper miniFuse of
        Right [(1, 12), (2, 13)] -> pure ()
        res -> error $ "Unexpected output " <> show res
    case getLinkPairs Input reaper miniFuse of
        Right [(10, 3), (11, 4)] -> pure ()
        res -> error $ "Unexpected input " <> show res
    case getLinkPairs InOut reaper miniFuse of
        Right [(10, 3), (11, 4), (1, 12), (2, 13)] -> pure ()
        res -> error $ "Unexpected input " <> show res
  where
    reaper =
        NodePorts
            { outputs =
                [ (1, Pw.Port "reaper:out1" 0 (Audio Nothing))
                , (2, Pw.Port "reaper:out2" 0 (Audio Nothing))
                ]
            , inputs =
                [ (3, Pw.Port "reaper:in1" 0 (Audio Nothing))
                , (4, Pw.Port "reaper:in2" 0 (Audio Nothing))
                ]
            }
    miniFuse =
        NodePorts
            { outputs =
                [ (10, Pw.Port "mini:playback_FL" 1 AudioFL)
                , (11, Pw.Port "mini:playback_FR" 1 AudioFR)
                ]
            , inputs =
                [ (12, Pw.Port "mini:capture_FL" 1 AudioFL)
                , (13, Pw.Port "mini:capture_FR" 1 AudioFR)
                ]
            }
