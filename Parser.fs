(*
    Functional Programming - Assignment 7
    Sam Al-Sapti (sals@itu.dk)
    March 28th, 2022
*)


module ImpParser

    open Eval

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "not implemented"
    let pPointValue = pstring "not implemented"

    let pCharToInt  = pstring "not implemented"
    let pToUpper    = pstring "not implemented"
    let pToLower    = pstring "not implemented"
    let pCharValue  = pstring "not implemented"

    let pTrue       = pstring "not implemented"
    let pFalse      = pstring "not implemented"
    let pIsDigit    = pstring "not implemented"
    let pIsLetter   = pstring "not implemented"
    let pIsVowel   = pstring "not implemented"

    let pif       = pstring "not implemented"
    let pthen     = pstring "not implemented"
    let pelse     = pstring "not implemented"
    let pwhile    = pstring "not implemented"
    let pdo       = pstring "not implemented"
    let pdeclare  = pstring "not implemented"

    let whitespaceChar = pstring "not implemented"
    let pletter        = pstring "not implemented"
    let palphanumeric  = pstring "not implemented"

    let spaces         = pstring "not implemented"
    let spaces1        = pstring "not implemented"

    let (.>*>.) _ _ = failwith "not implemented"
    let (.>*>) _ _  = failwith "not implemented"
    let (>*>.) _ _  = failwith "not implemented"

    let parenthesise p = p // incorrect (not implemented)

    let pid = pstring "not implemented"

    
    let unop _ = failwith "not implemented"
    let binop _ p1 p2 = p1 .>>. p2 // incorrect (not implemented)

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref := choice [MulParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    do aref := choice [NParse; ParParse]

    let AexpParse = TermParse 

    let CexpParse = pstring "not implemented"

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"

(* These five types will move out of this file once you start working on the project *)
    type coord      = int * int
    type squareProg = Map<int, string>
    type boardProg  = {
            prog       : string;
            squares    : Map<int, squareProg>
            usedSquare : int
            center     : coord
    
            isInfinite : bool   // For pretty-printing purposes only
            ppSquare   : string // For pretty-printing purposes only
        }

    type word   = (char * int) list
    type square = Map<int, squareFun>

    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    let mkBoard (bp : boardProg) = failwith "not implemented"

