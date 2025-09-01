import std.Option;


type Wrapper[T] = { value : T};

type Pair[T,U] = { fst : T, snd : U};

type Result[T,E] = | Ok(T) | Err(E)

fun as_uint(x:uint) -> uint = x
fun len[T](a : [T]) -> uint = loop end
fun x[T](a : never) -> T = a
fun main() = do
    let a = x(match return with end) : int;
    let (mut a,b) : (uint,uint) = (1,2);
    (ref mut a)^ = 5;
    let mut i : uint = 50;
    for i in [1,2,3,4,5] do
        println(i);
    end

    
end