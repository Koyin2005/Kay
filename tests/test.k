type Maybe[T] = 
    | Just(T)
    | None

fun id[T](x: T) -> T =
    x

fun main() = do
    let a : fun(int) -> int = id;
    let a = Maybe.Just(1);
    println("Hello world!");
end