module LispTypes

open ResultMonad

type LispVal =
    | LispList of LispVal list
    | LispNumber of int
    | LispString of string
    | LispBool of bool
    | LispAtom of string
    | LispLambda of LispLambda
    | LispPrim of LispPrim
    | Nil
and
    LispLambda = {
        parameters: string list;
        body: LispVal;
        closure: Env;
    }
and 
    LispPrim = (LispVal list) -> result<LispVal>
and
    Env = Map<string, LispVal>