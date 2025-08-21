import test.hello;
import test.testy.hey;
fun f() = do
    hey();
    println("Hi from test2!");
    hello();
end

fun h() = do 
    hello();
end