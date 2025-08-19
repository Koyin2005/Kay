type Color = 
    | Red
    | Green
    | Blue
    | Yellow
    | Orange
    | Pink


type B = {
    a : int,
    b : int
}
    
fun a_and_b() -> B = B{ a = 1, b = 123}

fun a_or_b(a : |A | B) -> bool = do
    let _ =match a with end;
    true
end

fun is_even(x : uint) -> bool = match x with
    0 => true,
    n => is_even(n - 1 as uint) == true
end

fun push_bool(array: ref mut [bool], value : bool) = ()
fun push_int(array: ref mut [int], value : int) = ()
fun push_str(array: ref mut [ref string], value : ref string) = ()

fun make_B(a: int, b : int) -> B = B{ a = a, b = b}
fun get_a(b : B) -> int = b.a

fun main() = do 
    let a  = make_B(5,12);
    let c = get_a(a);
    let  mut c = 5;
    let a = ref c;
    match a with
      1^ => println(1 + 5),
    end

end