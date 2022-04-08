module Stack(Stack,empty,push,top,pop,isEmpty) where

data Stack a = StackImplementation [a] deriving Show
         
empty :: Stack a
empty = StackImplementation []

isEmpty :: Stack a -> Bool
isEmpty (StackImplementation []) = True
isEmpty _ = False

push :: Stack a -> a -> Stack a
push (StackImplementation lista) elem = StackImplementation (elem:lista)

top :: Stack a -> a
top (StackImplementation[]) = error "Stack Empty!"
top (StackImplementation[a]) = a
top (StackImplementation(a:b)) = a

pop :: Stack a -> Stack a  
pop (StackImplementation[]) = error "Stack Empty!"
pop (StackImplementation(a:b)) = StackImplementation b