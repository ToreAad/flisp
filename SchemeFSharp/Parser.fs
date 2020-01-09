module Parser
open System
open FParsec
open LispTypes
open ResultMonad

let lispValue, lispValueRef = createParserForwardedToRef<LispVal, unit>()

let parseAtom =
    regex "^([!#$%&*+./:<=>?@^_~a-zA-Z][!#$%&*+./:<=>?@^_~a-zA-Z0-9]*)" |>> LispAtom

let parseNumber = 
    pint32 |>> LispNumber

let parseString =
    skipChar '"' >>. manyChars (noneOf "\"") .>> skipChar '"' |>> LispString

let ws = spaces
let parseList = 
    skipChar '(' >>. spaces >>. 
    (attempt (sepBy lispValue spaces1) <|> sepEndBy1 lispValue spaces1)
    .>> spaces .>> skipChar ')' 
    |>> LispList

let parseQuote =
    skipChar '\'' >>. lispValue |>> fun x -> LispList [LispAtom "quote"; x]

let parseReserved=
    (pstring "Nil" >>% Nil) <|>
    (pstring "#t" >>% LispBool true) <|>
    (pstring "#f" >>% LispBool false) 

do lispValueRef := choice [ parseReserved
                            parseAtom
                            parseNumber
                            parseString
                            parseList
                            parseQuote
            ]

let lisp = 
    spaces >>. many lispValue .>> spaces


let parser str =
    match run lisp str with
    | Success (h::_, _, _) -> Microsoft.FSharp.Core.Ok h
    | Failure (errorMsg, _, _) -> Microsoft.FSharp.Core.Error errorMsg

//let Parser str =
//    match run lisp str with
//    | Success (result, _, _) -> printfn "Success: %A" result
//    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg