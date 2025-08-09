
type Option : variant{
    C,
    D,
    Some
}

type A : struct{
    V : int
}

fun main()
    for i in 0..10 do
        println(i);
        println("Hey")
    end
    let a = 1;
    let b = a;
    a = 5;
    _ = b;
    let a = if true then println("Hey whatsup hello.") end;
    let Option.Some(a) = Option.Some(12);
    let a = A.V { a = 1, b = 2, c = 34};
end