module binTree
type binTree<'a>
val intToFloatTree : binTree<int> -> binTree<float>
val inorderToList : binTree<'a> -> 'a list

//val search1 : 'a * binTree<'a> -> bool when 'a : equality