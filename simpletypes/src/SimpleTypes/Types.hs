module SimpleTypes.Types 
where

data Type = TBool           -- Boolean Type 
          | TArr Type Type  -- Arrow type:  Type -> Type
          | TUnit
          deriving ( Eq, Show )

instance PrettryPrinter Type where 
  prettyprint TBool          = " Bool "
  prettyprint ( TArr t1 t2 ) = concat ["(", prettyprint t1, " -> ", prettyprint t2, ")"]
  prettyprint TUnit          = " Unit "


class PrettryPrinter a where 
  prettyprint :: a -> String




