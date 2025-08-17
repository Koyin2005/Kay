type A : variant{
    V(int),
    C()
}
type B : struct{
    a  : int
}
fun is_even(x : uint) -> bool = match x with
    0 => true,
    n => is_even(n - 1 as uint) == true
end

fun push_bool(array: ref mut [bool], value : bool) = ()
fun push_int(array: ref mut [int], value : int) = ()
fun push_str(array: ref mut [ref string], value : ref string) = ()

fun make_b(a: int) -> B = _{ a = a}
fun get_a(b : B) -> int = b.a

fun main() = do 
    let a  = make_b(5);
    let c = get_a(a);
    let  mut c = 5;
    let a = ref c;
    match a with
      1^ => println(1 + 5),
    end

end