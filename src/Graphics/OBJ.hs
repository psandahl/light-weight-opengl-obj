-- |
-- Module: Graphics.OBJ
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
--
-- Loading a subset of Wavefront OBJ files in a way that fit together with
-- LWGL.
module Graphics.OBJ
    ( loadVTNFromFile
    ) where

import           Graphics.OBJ.Assembly
