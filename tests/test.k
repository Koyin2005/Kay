import std.Option;

type Wrapper[T] = { value : T};

type Pair[T,U] = { fst : T, snd : U};

type Result[T,E] = | Ok(T) | Err(E)

fun as_uint(x:uint) -> uint = x
fun main() = do
    let (a,b) = (1,2);
    let mut i : uint = 50;
    while true do
        println(i);
        i = i - 1;
        if i < 0:uint then
            break
        end
    end
end