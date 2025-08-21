import std.Option;

type Wrapper[T] = { value : T};
fun id[T](x: T) -> T =
    x

fun main() = do
    let mut c : _ = Option.None;
    c = Option.Some(c);
    fun d(a:int) = ()
    d(c);
end