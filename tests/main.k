type StrOrInt = | Str(ref string) | Int(int)


fun main() = do
    let mut y = 5;
    let mut x  = do
        let mut a = 50;
        ref mut a
    end;
    x^ = 6;
end