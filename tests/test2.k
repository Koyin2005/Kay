
fun f() = do
    import test.hello;
    println("Hi from test2!");
    hello();
end

fun h() = do 
    let a = test;
    let a = 5;
    fun x(a:int) = println(a)
end