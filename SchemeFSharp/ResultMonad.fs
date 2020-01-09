module ResultMonad
open Result

type result<'a> = Result<'a, string>

type ResultClass() =
    member x.Bind(v: result<'a>, f:'a->result<'b>) = 
        match v with
        | Error error -> Error error
        | Ok a -> f a
    member x.Return (v: 'a) = Ok v
    member x.ReturnFrom (o : result<'a>) = o

let result = new ResultClass()