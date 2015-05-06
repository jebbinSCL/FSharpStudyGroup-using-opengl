module Exercise

    //Part one: Reimplementing standard library functions

    // This is a hint and example that uses recursion, a cons pattern and a list pattern. Example from: Pattern Matching - https://msdn.microsoft.com/en-us/library/dd547125.aspx
    let rec printList list =
        match list with
        | head :: tail -> printfn "%A " head; printList tail
        | [] -> printfn ""


    //TODO Reimplement List.map -> https://msdn.microsoft.com/en-us/library/ee370378.aspx
    // Map takes a function and a list, applies the function to each element in the list, and returns a new list of the results. 

    let rec mapList mapping list =  
        match list with
        | head :: tail -> mapping head :: mapList mapping tail
        | [] -> []

    let mapListGeneratorComprehension mapping list =
        [for a in list do yield mapping a]

    //TODO Reimplement List.filter -> https://msdn.microsoft.com/en-us/library/ee370294.aspx
    // Fold takes a function to test each list element (function returns a boolean) and a list. Returns a new list containly only the elements for which the predicate returned true. 
    let rec filterList predicate list = 
        match list with
        | head::tail when predicate head -> head :: filterList predicate tail
        | _::tail -> filterList predicate tail
        | [] -> []


    //TODO Reimplement List.fold -> https://msdn.microsoft.com/en-us/library/ee353894.aspx
    // Takes a function to update the accumulated values given the input element, the initial value, and the list. returns the final accumulated value
    let rec foldList folder acc list = 
        match list with
        | head :: tail -> foldList folder (folder acc head) tail
        | [] -> acc


    //TODO Reimplement List.reduce
    // Takes a function to update the accumulated values given the input element and the list. returns the final accumulated value
    //Throws an ArgumentException if the list is empty (Use invalidArg function -> https://msdn.microsoft.com/en-us/library/dd233178.aspx ) 
    let rec reduceList reducer list =
        match list with
        | [] -> invalidArg "list" "Cannot perform reduce on empty list"
        | head::tail -> foldList reducer head tail


// List comprehension / Generator for other data structures

    let mapArrayGenerator mapping array = 
        [| for a in array do yield mapping a|]
    let mapSeqGenerator mapping sequence = 
        seq { for a in sequence do yield mapping a}

// Tail Recursion

    let rec mapListBasic2 mapping list =  
        match list with
        | [] -> []
        | head :: tail -> 
            let temp = mapListBasic2 mapping tail; 
            mapping head:: temp



    let mapListTail mapping list = 
        let rec loop acc loopList = 
            match loopList with
            | [] -> List.rev acc
            | head :: tail -> loop (mapping head :: acc) tail
        loop [] list
        
    // See http://stackoverflow.com/questions/3248091/f-tail-recursive-function-example for continuation style
    let mapListCont mapping list = 
        let rec loop cont loopList = 
            match loopList with
            | [] -> cont []
            | head :: tail -> loop (fun acc -> cont(mapping head::acc)) tail
        loop id list // id x = x (A built in function)