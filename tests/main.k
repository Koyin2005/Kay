type A = {
    b : int,
    c : int
}
type Three = 
| One
| Two
| Three
type Result[T,E] = | Ok(T) | Fail(E)

fun never_to_any[T](x: never) -> T = x
fun print_int(x : uint) = ()
fun what_the_helly() = do
    let a : Option[int] = never_to_any(return);
end
