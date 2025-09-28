fun main() = do
    let abc_0  = do 
        let mut a = [true,return,false];
        let mut c = [true,false,return];
        let mut b = [true];
        let a1 = if true then 
                    ref mut a 
                else  
                    ref mut c
                end;
        let mut b2 = if true then ref mut a else ref mut b end;
        let mut abc = if true then b2 else a1 end;
        d[a,b,c](abc)
    end;
    abc_0^ = false;
end
fun d[origin a,origin b,origin c](x : ref mut [a,b,c] [bool]) -> ref mut [a,b,c] bool = ref mut x^.[0]

fun bool_to_int(b: bool) -> int = if b then 1 else 0 end