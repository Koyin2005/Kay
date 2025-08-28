type Option[T] = 
    | Some(T)
    | None

type Result[T,E] = 
    | Ok(T)
    | Err(E)


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

