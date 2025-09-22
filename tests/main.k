/*fun main() = do
    println("Hello world it's me.");
    println("Hey ");
end
*/
fun this_is_fun(c: int) -> ref string = do
    let a : ref[c] int = ref c;
    "Hey"
end