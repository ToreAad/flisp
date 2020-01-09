module Evaluator

open LispTypes
open ResultMonad
open System.Linq.Expressions
open System.Runtime.InteropServices.ComTypes
    
let rec disentangle (vals:LispVal list): result<(LispVal list)*(LispVal list)> =
    match vals with
    | (x1::x2::xs) -> 
        result {
            let! rest1, rest2 = disentangle xs
            return (x1::rest1, x2::rest2)
        }
    | [] -> Ok ([], [])
    | (x::[]) -> Error "Args not matching"


let bind (env: Env) (keys : string list) (vals: LispVal list) : Env =
    let zipped = List.zip keys vals
    List.fold (fun acc p -> acc.Add p) env zipped

let rec eval (env:Env) (exp:LispVal) : result<LispVal*Env> =
    match exp with
    | LispNumber num -> result {return ((LispNumber num), env)}
    | LispString str -> result {return ((LispString str), env)}
    | LispBool b -> result {return ((LispBool b), env)}
    | LispList [] -> result {return ((Nil), env)}
    | Nil -> result {return ((Nil), env)}
    | LispAtom atom -> 
        match env.TryFind atom with
            | Some v -> Ok (v,env)
            | None -> Error (sprintf "Could not find %A in env" atom)
    | LispList [LispAtom "if"; pred; consequence; alternative] ->
        result {
            let! p = getBool env pred
            if (p=true)
            then return! (eval env consequence)
            else return! (eval env alternative)
        }
    | LispList [LispAtom "let"; LispList pairs; expr] ->
        result{
            let! key_vals, vals = disentangle pairs
            let! keys = getAtoms env key_vals
            let new_env = bind env keys vals
            let! (lval, _) = (eval new_env expr)
            return (lval, env)
        }
    | LispList ((LispList [(LispAtom "define"); LispAtom atom; expr])::rest : LispVal list) ->
        result {
            let! (expr, _) = eval env expr
            let new_env = env.Add (atom, expr)
            let! (new_expr, _) = eval new_env (LispList rest)
            return (new_expr, env)
        }
    | LispList ((LispList [LispAtom "define"; LispList ((LispAtom f)::args); expr])::rest : LispVal list) ->
        result {
            let! parameters = getAtoms env args
            let func = getLambda expr parameters env
            let new_env = env.Add (f, func)
            let! (new_expr, _) = eval new_env (LispList rest)
            return (new_expr, env)
        }
    | LispList [LispAtom "lambda"; LispAtom atom; expr] ->
        result {
            let func = getLambda expr [atom] env
            return (func, env)
        }
    | LispList [LispAtom "lambda"; LispList args; expr] ->
        result {
            let! parameters = getAtoms env args
            let func = getLambda expr parameters env
            return (func, env)
        }
    | LispList ([expr;]) ->
        result {return! eval env expr}
    | LispList (f::args) ->
        result {
            let! func, new_env = (eval env f)
            let! args, last_env = evalList new_env args
            let! res = apply func args
            return (res, last_env)
        }
    | _ ->
        Error (sprintf "Failed to evaluate %A %A" exp env)
and
    evalList (env: Env) (exprs : LispVal list) : result<(LispVal list)*Env> =
        match exprs with
        | exp::rest ->
            result {
                let! evaluated_exp, new_env = eval env exp
                let! evaluated_exprs, result_env = evalList new_env rest
                return (evaluated_exp::evaluated_exprs, result_env)
            }
        | [] -> Ok ([], env)
   
and
    apply (func:LispVal) (args: LispVal list) : result<LispVal> = 
        match func with
        | LispPrim func -> 
            result{
                return! (func args)
            }
        | LispLambda {parameters = parameters; body = body; closure = closure;} ->
            result{
                let new_env = bind closure parameters args 
                let! (exp, _) = eval new_env body
                return exp
            }
and 
    getLambda (expr:LispVal) (parms: string list) (env:Env) : LispVal =
        LispLambda {
                parameters = parms;
                body = expr;
                closure = env;
            }
and
    getList (env:Env) (exp:LispVal) : result<LispVal list> =
        match eval env exp with
        | Ok (LispList list, env) -> Ok list
        | _ -> Error (sprintf "%A did not evaluate to a list" exp)
and
    getNumber (env:Env) (exp:LispVal) : result<int> =
        match eval env exp with
        | Ok (LispNumber num, env) -> Ok (num)
        | _ -> Error (sprintf "%A did not evaluate to a number" exp)
and 
    getNumbers (env:Env) (exprs : LispVal list) : result<int list> =
        match exprs with
        | expr::rest -> 
            result {
                let! number = getNumber env expr
                let! restOfNumbers = getNumbers env rest
                return number::restOfNumbers
            }
        | [] -> Ok []
and 
    getBool (env:Env) (exp:LispVal) : result<bool> =
        match eval env exp with
            | Ok (LispBool b, env) -> Ok (b)
            | _ -> Error (sprintf "%A did not evaluate to a boolean" exp)
and 
    getAtom (env:Env) (exp:LispVal) : result<string> =
        match eval env exp with
            | Ok (LispAtom atom, env) -> Ok (atom)
            | _ -> Error "No atom here"
and 
    getAtoms (env:Env) (exprs : LispVal list) : result<string list> =
        match exprs with
        | expr::rest -> 
            result {
                let! atom = getAtom env expr
                let! restOfAtoms = getAtoms env rest
                return atom::restOfAtoms
            }
        | [] -> Ok []


let evaluator exp =
    let primitiveEnv = Map.ofList [ 
        ("var", LispNumber 42);
        ("add", LispLambda 
            { parameters = ["a"; "b"];
            body = LispList [LispAtom "+"; LispAtom "a"; LispAtom "b"; LispAtom "c"];
            closure = Map.ofList [("c", LispNumber 2 )]});
        ("+", LispPrim (fun (vals : LispVal list) ->
                result{
                    let! numbers = getNumbers Map.empty vals
                    return LispNumber (List.fold (+) 0 numbers)
                }));
    ]
    eval primitiveEnv exp