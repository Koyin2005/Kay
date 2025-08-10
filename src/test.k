type A : variant{
    V
}
fun main() 
    for i in 0..10 do 
        println(i);
        println("Hey")
    end
    match 1 with
        Option.Some(a) => println(a),
        Option.None => println("none")
    end
    let a = 1;
    let b = a;
    4 = 5;
    _ = b;
    let a = if true then println("Hey whatsup hello.") end;
    let Option.Some(a) = Option.Some(12);
    let a = A.V { a = 1, b = 2, c = 34};
end