import std.Option;

type Wrapper[T] = { value : T};

type Pair[T,U] = { fst : T, snd : U};

type Result[T,E] = | Ok(T) | Err(E)

fun map_option[T,U](f: fun(T) -> U, x : Option[T]) -> Option[U] = 
    match x with
        Option.Some(x) => Option.Some(f(x)),
        Option.None => Option.None
    end

fun id[T](x:T) -> T = x
fun map[T,U](f:fun(T)->U,x:T) -> U = f(x)
fun main() = do
    let c  = Option.None;
    let a = match c with
        Option.Some(x) => x + 2,
        Option.None => 0
    end;
    let c = map_option(id,c);
end