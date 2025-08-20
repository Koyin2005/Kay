type Color = 
    | Red
    | Green
    | Blue
    | Yellow
    | Orange
    | Pink

type NeverNotEvenOnce;


type B = {
    a : int,
    b : int
}

type ResultForever = 
    | Ok(never)
    | Err(int)

fun random() -> int = 42
fun forever() -> ResultForever = ResultForever.Ok(loop
    if random() == 17 then
        return ResultForever.Err(0)
    end
end)


    
fun how_many(x : NeverNotEvenOnce) -> int = match x with end
fun favorite_color() -> Color = Color.Red

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
    let a = if true then 
    
    end
    match forever() with
        ResultForever.Ok(_) => (),
        ResultForever.Err(error_code) => println("Error code ",error_code)
    end
end