type Result[T,E] = | Ok(T) | Err(E)

fun stuff() -> Result[int,Empty] = Result.Ok(5)
fun into_ok[T,E](x : Result[T,E], f : fun(E) -> never) -> T = 
    match x with
        Result.Ok(x) => x,
        Result.Err(e) => f(e)
    end


fun from_both[origin a,origin b,T]() -> ref [a,b] T = loop end
fun diverge() -> never = loop end 
fun hey[T](x : T, y: T) = ()
fun main() = do 
    let a =5 ; let b = 7;
    let c : ref [a,b] int = match 5 with 
        0 => return, 
        1 => ref a, 
        2 => from_both[a,b,_](), 
        _ => ref b 
    end;
    fun mid_result() -> Result[int,never] = Result.Ok(5)
    let Result.Ok(x) = mid_result();
end


type Empty;

fun empty_to_never(x : Empty) -> never = match x with end
fun absurd[T](x : never) -> T = x 
fun fib(x : int) -> int = 
    match x with
        0 => 0,
        1 => 1,
        2 => 1,
        n => fib(n-1) + fib(n-2)
    end