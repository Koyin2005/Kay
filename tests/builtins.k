fun len[T](x : [T]) -> uint;
fun panic[region r](x : ref {r} string) -> never;
fun println[region r](x : ref {r} string) -> ();
type Option[T] = | Some(T) | None;