// From Section 7.6 Parameterized modules. Type variables. 
// Signature file for a simple Queue

module Queue
type Queue<'a>
val empty : Queue<'a>
val put   : 'a -> Queue<'a> -> Queue<'a>
val get   : Queue<'a> -> 'a * Queue<'a>
exception EmptyQueue
val isEmpty : Queue<'a> -> bool
val size : Queue<'a> -> int
val ofList :  'a list -> Queue<'a>
val toList : Queue<'a> -> 'a list
val put_list : 'a list -> Queue<'a> -> Queue<'a>
