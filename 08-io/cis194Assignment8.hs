---- Assignment 8 - Party -----
import Data.Monoid
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e:l) (f + empFun e)

instance Monoid GuestList where
    mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)
    mempty = GL [] 0

-- fix var naming
moreFun :: GuestList -> GuestList -> GuestList
moreFun a@(GL _ f1) b@(GL _ f2) = if f1 > f2 then a else b