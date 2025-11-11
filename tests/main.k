type A = {
    b : int,
    c : int
}
type Three = 
| One
| Two
| Three
import std;
type Result[T,E] = | Ok(T) | Fail(E)

fun print_int(x : uint) = ()
fun what_the_helly() = do
    let mut x = Option.Some[int](5);
    
    region r1;
    let c = match x with
        Option.Some(ref mut v) => v : ref mut {r1} _,
        Option.None => panic("Lmao")
    end;
    x = Option.None;
    c^ = 10;
end
