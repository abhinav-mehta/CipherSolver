module BinTree where

data BinTree a =  NullTree
		| Node (BinTree a) a (BinTree a)
		deriving (Show)

null1 NullTree 	= True
null1 _	 	= False

empty = NullTree

singleton a = Node empty a empty

inorder NullTree = []
inorder (Node lt a rt) = inorder lt ++ a : inorder rt

preorder NullTree = []
preorder (Node lt a rt) = a : preorder lt ++ preorder rt

postorder NullTree = []
postorder (Node lt a rt) = postorder lt ++ postorder rt ++ [a]

clockwise (Node (Node t1 b t2) a t) = Node t1 b (Node t2 a t)
clockwise t = t

anticlockwise (Node b a (Node t1 t t2)) = Node (Node b a t1) t t2
anticlockwise t = t

--{-
t3 = singleton 3
b = singleton 6
t4 = singleton 4
a = singleton 5
t1 = singleton 1
t = singleton 0
t2 = singleton 2
--}-
tree1 = Node (Node t3 6 t4) 5 (Node t1 0 t2)


aux x empty = (x, empty)
aux x (Node l input r) = (y, res)
	where (xl, l') = aux x l
	      (xr, r') = aux x r
	      y = minimum[xl, xr, input]
	      res = Node l' y r'
