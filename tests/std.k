type Result[T,E] = 
    | Ok(T)
    | Err(E)


fun array_ref_at[region a,T](self: ref{a} [T], index: uint) -> ref{a} T = ref self^.[index]

fun array_ref_mut_at[region a,T](self: ref mut {a}  [T], index: uint) -> ref mut {a}  T = ref mut self^.[index]

fun try_array_at_ref[region r,T](self: [T], index: uint) -> Option[ref{r} T] = 
    if index < len(self) then
        Option.Some(ref self.[index])
    else
        Option.None
    end


fun try_array_at_ref_mut[region r,T](mut self: [T], index: uint) -> Option[ref mut {r} T] = 
    if index < len(self) then
        Option.Some(ref mut self.[index])
    else
        Option.None
    end


fun try_array_at[T](self: [T], index: uint) -> Option[T] = 
    if index < len(self) then
        Option.Some(self.[index])
    else
        Option.None
    end



fun unwrap_option[T](self: Option[T]) -> T = match self with
    Option.Some(x) => x,
    Option.None => panic("unwrapped empty option")
end

fun map[T,U](self : Option[T], f: fun(T) -> U) -> Option[U] = 
    match self with
        Option.Some(x) => Option.Some(f(x)),
        Option.None => Option.None
    end

fun and_then[T,U](self: Option[T], f: fun(T) -> Option[U]) -> Option[U] = 
    match self with
        Option.Some(x) => f(x),
        Option.None => Option.None
    end


fun and_[T,U](self: Option[T], x : Option[U]) -> Option[U] = 
    match self with
        Option.Some(_) => x,
        Option.None => Option.None
    end

fun or_[T,U](self: Option[T], x : Option[T]) -> Option[T] = 
    match self with
        Option.Some(x) => Option.Some(x),
        Option.None => x
    end


fun or_else[T,U](self: Option[T], f: fun() -> Option[T]) -> Option[T] = 
    match self with
        Option.Some(x) => Option.Some(x),
        Option.None => f()
    end


fun as_ref[region r,T](x : ref {r} Option[T]) -> Option[ref {r} T] = match x^ with
    Option.Some(ref x) => Option.Some(x),
    Option.None => Option.None
end


fun as_ref_mut[region r,T](x : ref mut {r} Option[T]) -> Option[ref mut {r} T] = match x^ with
    Option.Some(ref mut x) => Option.Some(x),
    Option.None => Option.None
end