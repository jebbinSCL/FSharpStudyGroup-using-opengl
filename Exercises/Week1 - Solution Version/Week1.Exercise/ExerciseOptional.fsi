module ExerciseOptional 
    (*
    This is an F# Signature file. While it is not required when creating F# code, it has a number of use cases when creating libraries (E.g. Encapsulation)
    In this situation, I'm using the signatures to help show what the functions are meant to do, so everything within Exercise.fs is reflected in here
    (In a normal signature file, you wouldn't necessarily do this, so please don't take this as a proper example)
    *)
    
    
    type DomainExtension =
        | CoUk
        | Com
    type Contact = {
        FirstName: string;
        LastName: string;
        Email: string;
        EmailExtension: DomainExtension;
    }

    val FSharpStudyGroup : string

    val splitStringBySemiColons : semiColonSepString:string -> string list
    val parseContact : contact:string -> Contact
    val getExtensionPhrase : extension:DomainExtension -> string
    val getContactString : contact:Contact -> string
    val transformContactString : (string -> string)
    val prettyPrintFSharpGroupRecords : recordsList:string list -> unit
    val printRecordsStats : recordsList:string list -> unit

    val executeExercise : unit -> unit
