type A : variant{
    V
}
fun is_even(x : uint) -> bool = match x with
    0 => true,
    n => is_even(n - 1) == true
end


fun print_1_through_50() = do
    let a = println;
    for i in 0 as uint..500 as uint do
        println(i);
    end
end