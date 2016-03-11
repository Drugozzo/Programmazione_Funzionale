module Stack

type Stack<'a when 'a : equality> 

exception EmptyStack
val empty : 'a Stack
val push : 'a -> 'a Stack -> 'a Stack
val pop : 'a Stack -> 'a * 'a Stack
val top : 'a Stack -> 'a
val size : 'a Stack -> int 
val isEmpty : 'a Stack -> bool 
val push_list : 'a list -> 'a Stack -> 'a Stack
val append : 'a list -> 'a Stack -> 'a Stack
val toList : 'a Stack -> 'a list
val ofList :  'a list -> 'a Stack 
