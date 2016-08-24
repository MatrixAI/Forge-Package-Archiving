The data type is a short hand to do the following

data T = A Ta | B Tb | C Tc | F Dom | AB Ta Tb

is equivalent to 
A :: Ta -> T
B :: Tb -> T
C :: Tc -> T
F :: TDom -> T
AB :: Ta -> Tb -> T

/// WRONG, BUT WORTH EXPLORING LATER
Where a, b, c are types themselves, and T is the returned type. Furthermore, T can be considered now to be the set of
{A, B, C, F, AB}; it is a set of functions that return a member of the set (a function) itself; we can now evenly explicitly define what it means to be a member of T, i.e:
///

`A function X is an element of T iff there exists a G element of T such that for all x element of Tx, 'X(x) = G'`

Furthermore, we say that a function F that is defined for every x E TDom, is universally quantified over TDom (the type domain). We can say that in standard Haskell, F :: TDom -> T is equivalent to the propositional logic:
`forall x in TDom, there exists y in T such that y = F a`. When this is the case, we call F a 'total function'.

On quantified types:
First off, `forall` does not mean what you think it means. It actually means !4! different things. Related, and confusing.

 by default when we declare
data T = MkT a; we really mean:
data T = forall a. MkT a

For conciseness, we will use GADT (Generalised Algebraic Data Types), where the above is equivalent to:
data T where
    MkT :: a -> T


Just remember, Haskell has 2 languages; a type language, and a term level language.

ETC:
Nat -> Nat -> Nat

Nat^Nat^Nat

-> (function application)

Nat^Nat


data T = MkT a

:t MkT
MkT :: {private ::: a} -> T

MkT :: a -> T

data HashesToBeUsed = forall a. HashAlgorithm a => WrapHash a

[WrapHash MD5, WrapHash SHA512] :: [HashToBeUsed]

map (\(WrapHash h) -> hashInitWith h) hashesToBeHashed

