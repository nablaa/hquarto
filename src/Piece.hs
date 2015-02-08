module Piece (Piece(..), Color(..), Shape(..),
              Height(..), Top(..)) where

data Color = White | Black
           deriving (Show, Eq)

data Shape = Circular | Square
           deriving (Show, Eq)

data Height = Short | Tall
            deriving (Show, Eq)

data Top = Hollow | Solid
         deriving (Show, Eq)

data Piece = Piece Color Shape Height Top
           deriving (Show, Eq)
