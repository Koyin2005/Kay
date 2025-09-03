import std.Option;


type Wrapper[T] = { value : T};

type Pair[T,U] = { fst : T, snd : U};

type Result[T,E] = | Ok(T) | Err(E)

fun as_uint(x:uint) -> uint = x
fun len[T](a : [T]) -> uint = loop end
fun x[T](a : never) -> T = a
fun main() = do
    let (mut a,b) : (uint,uint) = (1,2);
    let mut a = "Hello world";
    (ref mut a)^ = "Yo";
    let mut i : uint = 50;
    let mut a : [uint] = [1,2,3,4,5,6,7,8,9,10];
    let v = a.[0];
    a.[v] = 5;
end