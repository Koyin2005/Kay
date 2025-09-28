type StrOrInt = | Str(ref string) | Int(int)


fun main() = do
    let mut x = StrOrInt.Int(5);
    let y = match x with 
        StrOrInt.Int(ref y) => y,
        _ => loop end
    end;
    x = StrOrInt.Str("Hey");
    y^;
end