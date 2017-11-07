module SimpleTypes.Types 
where

data Type = TBool           -- Boolean Type 
          | TArr Type Type  -- Arrow type:  Type -> Type
          | TUnit
          deriving Eq


instance Show Type where
  show TBool          = "Bool"
  show ( TArr t1 t2 ) = concat ["(", show t1, " -> ", show t2, ")"]
  show TUnit          = "Unit"





