fun do_stuff[region a](x : ref[a] int) -> ref[a] int = x

fun main() = do
    region x
        let v = 5;
        let mut a : ref[x] int = ref v;
        let c = do_stuff(a);
        a = c;
    end

end