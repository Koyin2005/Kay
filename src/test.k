type List = variant{
    Cons(int,ref List),
    One(int),
    Nil
};

let l = List.One(10);
let l = List.Cons(10,ref List.Cons(12, ref List.One(12)));
let l = List.One(10);