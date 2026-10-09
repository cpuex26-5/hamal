module Type (Type (..)) where

data Type
  = UnitTy
  | BoolTy
  | IntTy
  | FloatTy
  | FunTy [Type] Type
  | TupleTy [Type]
  | ArrayTy Type
  deriving stock (Eq, Show)
