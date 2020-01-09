module Maybe

type maybe<'a> = option<'a>

type MaybeClass() =
    member x.Bind(v: maybe<'a>, f:'a->maybe<'b>) = 
        match v with
        | None -> None
        | Some a -> f a
    member x.Return (v: 'a) = Some v
    member x.ReturnFrom (o : maybe<'a>) = o
    member x.Zero () = None

let maybe = new MaybeClass()