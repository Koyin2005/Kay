fun main() = do
    println("Hello world it's me.");
    println("Hey ");
end

type StrOrInt = | Str(ref string) | Int(int)

fun this_is_fun(c: bool, d:bool) -> ref string = do
    let mut x = StrOrInt.Str("Hello");
    let y = ref mut x;
    match x with
        StrOrInt.Str(ref insides) => do
            y^ = StrOrInt.Int(5);
            let v : ref[insides] ref string = insides;
        end
    end
    "Hey"
end