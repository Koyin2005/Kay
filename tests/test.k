import std.Option;

type Wrapper[T] = { value : T};

type Pair[T,U] = { fst : T, snd : U};

type Result[T,E] = | Ok(T) | Err(E)

fun map_option[T,U](f: fun(T) -> U, x : Option[T]) -> Option[U] = 
    match x with
        Option.Some(x) => Option.Some(f(x)),
        Option.None => Option.None
    end

fun what_is_this[T]() -> T = loop end
fun id[T](x:T) -> T = x
fun map[T,U](f:fun(T)->U,x:T) -> U = f(x)

fun add(x : int, y: int) -> int = x + y
fun at[T](arr: [T],index : uint) -> ref mut T = loop end
fun main() = do
    let a = what_is_this();
    a(1,2);
end