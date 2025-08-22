import std.Option;

type Wrapper[T] = { value : T};

type Pair[T,U] = { fst : T, snd : U};

fun id[T](x: T) -> T =
    x

fun map[T,U](f:fun(T)->U,x:T ) -> U = f(x)


fun map_arr[T,U](x:[T],f:fun(T)->U) -> [U] = loop end

fun square(x : int) -> int = x * x
fun main() = do
    let a = Wrapper{ value = 5};
    let c = Pair{ fst = true, snd = false};
    let x  = map(square,5);
end