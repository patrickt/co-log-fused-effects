{- |
Copyright: (c) 2020 Patrick Thomson
SPDX-License-Identifier: MPL-2.0
Maintainer: Patrick Thomson <patrickt@github.com>

A fused-effects interface to the co-log ecosystem.
-}

module CoLogFusedEffects
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
