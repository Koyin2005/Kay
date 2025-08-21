import test;

type A;
fun hey() = ()
fun f() = do
    hey();
    println("Hi from test2!");
    test.hello();
end

fun h() = do 
    test.hello();
end
