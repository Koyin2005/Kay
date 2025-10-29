

type Maybe[T] = | None | Just(T)
fun id_ref[region a,T](x : ref {a} T) -> ref{a} T = x
fun id_ref_mut[region a,T](x : ref mut {a} T) -> ref mut {a} T = x
fun ref_mut_to_ref[region a,T](x : ref mut {a} T) -> ref {a} T = loop end

type MaybeArray = | Nah | Array([int])
fun what_the_helly() = do
    let mut x = 5;
    let c = ref mut x;
    let f = c^;
    c^ = 5;
end
