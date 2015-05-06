module Exercise
(*
    This is an F# Signature file. While it is not required when creating F# code, it has a number of use cases when creating libraries (E.g. Encapsulation)
    In this situation, I'm using the signatures to help show what the functions are meant to do, so everything within Exercise.fs is reflected in here
    (In a normal signature file, you wouldn't necessarily do this, so please don't take this as a proper example)
*)
    val printList : list:'a list -> unit
    val mapList : mapping:('a -> 'b) -> list:'a list -> 'b list
    val filterList : predicate:('a -> bool) -> list:'a list -> 'a list
    val foldList : folder:('a -> 'b -> 'a) -> acc:'a -> list:'b list -> 'a
    val reduceList : reducer:('a -> 'a -> 'a) -> list:'a list -> 'a

    
