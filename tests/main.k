fun do_stuff[region a](x : ref{a} int) -> ref{a} int = x

fun hey[region b]() = ()

fun bad(x : int) -> ref {x} int = ref x
type StoreARef[region x,T] = {
    a_ref : ref {x} T
}

type Maybe[T] = | None | Just(T)

type MaybeArray = | Nah | Array([int])
fun what_the_helly() = do
    let mut a = MaybeArray.Array([50]);
    let c : ref {a} _ = match a with
        MaybeArray.Array(ref x) => x,
        MaybeArray.Nah => loop end
    end;
    let m = Maybe.Just((1,2,3,4):(uint,uint,uint,uint));
    let _ = match m with 
        Maybe.Just((ref a, ref b,ref c,ref d)) =>  a^ + b^ + c^ + d^,
        Maybe.None => loop end
    end;
    a = MaybeArray.Nah;
    let d = c^.[0];
end
