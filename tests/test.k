import std.Option;

type Wrapper[T] = { value : T};

type Pair[T,U] = { fst : T, snd : U};

type Result[T,E] = | Ok(T) | Err(E)

fun as_uint(x:uint) -> uint = x
fun len[T](a : [T]) -> uint = loop end
fun main() = do
    let (a,b) : (uint,uint) = (1,2);
    let mut i : uint = 50;
    let mut a = return;
    a : int = 5;
    for i in 0:uint .. 10:uint do
        println(i);
    end

    
end