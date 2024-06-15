module PwController.Expr where

import PwController.Matcher (Matcher)

data Condition = DeviceMatcher Matcher | NodeMatcher Matcher
    deriving (Show)

data TargetKind = Node | Port | Media | Device
    deriving (Show)

data Target = Target {kind :: TargetKind, matcher :: Matcher}
    deriving (Show)

data Direction = Input | Output | InOut
    deriving (Show)

type Rules = [Expr]

data Expr
    = When {condition :: Condition, body :: Rules}
    | Disconnect {direction :: Direction, target :: Target}
    | Connect {direction :: Direction, target :: Target, with :: Target, redirect :: Maybe Target}
    deriving (Show)
