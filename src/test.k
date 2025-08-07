type Option : variant {
    C,
    D,
    Some
}

type A : struct{
    V : int
}
fun main(){
    let a = 1;
    let b = a;
    a = 5;
    _ = b;
    let Option.Some(a) = Option.Some(12);
    let a = init A.V{ a = 1, b = 2, c = 34};
}