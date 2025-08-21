type Maybe[T] = 
    | Just(T)
    | None

type Wrapper[T] = { value : T};
fun id[T](x: T) -> T =
    x

fun main() = do
    let c = Wrapper { value = 5};
    let a : fun(int) -> bool = id;
    let a = Maybe.Just(1);
    println("Hello world!");
end