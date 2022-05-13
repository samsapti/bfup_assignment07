module ImpParser

    open Eval
    open StateMonad

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    // Helper functions
    let curry f x y = f (x, y)
    let uncurry f (x, y) = f x y
    
    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) a b = a .>> spaces .>>. b
    let (.>*>) a b  = a .>> spaces .>> b
    let (>*>.) a b  = a .>> spaces >>. b

    let parenthesise p = (pchar '(') >*>. p .>*> (pchar ')')
    let curlysise p = (pchar '{') >*>. p .>*> (pchar '}')

    let pid = (pchar '_' <|> pletter) .>>. many (palphanumeric <|> pchar '_') |>> (fun (c1, c2) -> c1 :: c2 |> System.String.Concat)
    
    let unop op b = op >*>. b
    let binop op a b = a .>*> op .>*>. b

    (*
        aExp parser
    *)

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Add"
    do tref.Value <- choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Div"
    do pref.Value <- choice [MulParse; DivParse; ModParse; AtomParse]
    
    let ParParse = parenthesise TermParse
    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid |>> V <?> "string"
    let NegParse = unop (pchar '-') pint32 |>> fun i -> Mul(N (-1), N i)
    let PVParse  = pPointValue >*>. ParParse |>> PV <?> "Point value"

    let AexpParse = TermParse
    let AParParse = parenthesise AexpParse

    (*
        cExp parser
    *)
    
    let CexpParse, cref = createParserForwardedToRef<cExp>()
    let CParParse = parenthesise CexpParse

    let CParse = (pchar '\'') >>. anyChar .>> (pchar '\'') |>> C <?> "Char"
    let TUParse = pToUpper >*>. CParParse |>> ToUpper <?> "To upper"
    let TLParse = pToLower >*>. CParParse |>> ToLower <?> "To lower"
    let CVParse = pCharValue >*>. AParParse |>> CV <?> "Char value"
    let ITCParse = pIntToChar >*>. AParParse |>> IntToChar <?> "Int to char"
    do cref.Value <- choice [ITCParse; CParse; TUParse; TLParse; CVParse]

    let CTIParse = pCharToInt >*>. CParParse |>> CharToInt <?> "Char to int"
    do aref.Value <- choice [CTIParse; NegParse; PVParse; ParParse; VParse; NParse;]

    (*
        bExp parser
    *)

    let BTermParse, btref = createParserForwardedToRef<bExp>()
    let BProdParse, bpref = createParserForwardedToRef<bExp>()
    let BAtomParse, baref = createParserForwardedToRef<bExp>()

    let ConjParse = binop (pstring "/\\") BProdParse BTermParse |>> Conj <?> "Conjunction"
    let DisjParse = binop (pstring "\\/") BProdParse BTermParse |>> uncurry (.||.) <?> "Disjunction"
    do btref.Value <- choice [ConjParse; DisjParse; BProdParse]

    let EqParse = binop (pchar '=') AexpParse AexpParse |>> AEq <?> "Equals"
    let NeqParse = binop (pstring "<>") AexpParse AexpParse |>> AEq |>> Not <?> "Not equals"
    let LTParse = binop (pchar '<') AexpParse AexpParse |>> ALt <?> "Less than"
    let LTEqParse = binop (pstring "<=") AexpParse AexpParse |>> uncurry (.<=.) <?> "Less than or equal"
    let GTParse = binop (pchar '>') AexpParse AexpParse |>> uncurry (.>.) <?> "Greater than"
    let GTEqParse = binop (pstring ">=") AexpParse AexpParse |>> uncurry (.>=.) <?> "Greater than or equal"
    let TTParse = pTrue |>> (fun _ -> TT) <?> "true"
    let FFParse = pFalse |>> (fun _ -> FF) <?> "false"
    do bpref.Value <- choice [EqParse; NeqParse; LTParse; LTEqParse; GTParse; GTEqParse; BAtomParse]

    let BParParse = parenthesise BTermParse
    let NotParse = (pchar '~') >*>. BAtomParse |>> Not <?> "Not"
    let IsLetterParse = pIsLetter >*>. CParParse |>> IsLetter <?> "Is letter"
    let IsVowelParse = pIsVowel >*>. CParParse |>> IsVowel <?> "Is vowel"
    let IsDigitParse = pIsDigit >*>. CParParse |>> IsDigit <?> "Is digit"
    do baref.Value <- choice [NotParse; IsLetterParse; IsVowelParse; IsDigitParse; BParParse; TTParse; FFParse]

    let BexpParse = BTermParse

    (*
        stm parser
    *)

    let sTermParse, stref = createParserForwardedToRef<stm>()
    let sProdParse, spref = createParserForwardedToRef<stm>()
    let sAtomParse, saref = createParserForwardedToRef<stm>()
    
    let SeqParse = binop (pchar ';') sProdParse sTermParse |>> Seq
    do stref.Value <- choice [SeqParse; sProdParse]

    let AssParse = binop (pstring ":=") pid AexpParse |>> Ass
    do spref.Value <- choice [AssParse; sAtomParse]
    
    let sCurParse = curlysise sTermParse
    let ifThenParse =  pif >*>. BParParse .>*> pthen .>*>. sCurParse
    let DeclareParse = pdeclare .>> spaces1 >>. pid |>> Declare <?> "Declare"
    let ITEParse = ifThenParse .>*> pelse .>*>. sCurParse |>> (fun ((i, t), e) -> ITE (i, t, e)) <?> "If then else"
    let ITParse = ifThenParse |>> (fun (i, t) -> ITE (i, t, Skip)) <?> "If then"
    let WhileParse = pwhile >*>. BParParse .>*> pdo .>*>. sCurParse |>> While <?> "While"
    do saref.Value <- choice [DeclareParse; ITEParse; ITParse; WhileParse]

    let stmntParse = sTermParse

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    //let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}

// #load "JParsec.fs" "StateMonad.fs" "Eval.fs" "Parser.fs";; open JParsec.TextParser;; open ImpParser;;