module SimpleTypes.Types 
where

data Type = TUnit 
          | TInt
          | TFloat
          | TBool 
          | TArr Type Type  -- Arrow type:  Type -> Type
          deriving ( Eq, Show )

instance PrettryPrinter Type where 
  prettyprint TUnit          = " Unit "
  prettyprint TInt           = " Int "
  prettyprint TFloat         = " Float "
  prettyprint TBool          = " Bool "
  prettyprint ( TArr t1 t2 ) = concat ["(", prettyprint t1, " -> ", prettyprint t2, ")"]


class PrettryPrinter a where 
  prettyprint :: a -> String




