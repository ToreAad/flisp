module Repl
open System
open Parser
open Evaluator
open LispTypes
open ResultMonad

let rec until (pred : string->bool) (prompter : unit -> string) (evaluator : string -> unit) =
    let result = prompter ()
    if not (pred result) then
        evaluator result
        until pred prompter evaluator

let repl_evaluator : string -> unit = 
    fun s ->
        let res = result {
            let! parsed = parser s
            let! e = evaluator parsed
            return e
         }
        match res with
            | Ok v -> printfn "Ok: %A" v
            | Error error -> printfn "Failure: %s" error

let repl () =
    let pred = fun s -> s = "exit"
    let prompter = fun () -> printf "Lisp>>> "; Console.ReadLine ()
    until pred prompter repl_evaluator