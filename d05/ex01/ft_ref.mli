type 'a ft_ref

val return : 'a -> 'a ft_ref

val get : 'a ft_ref -> 'a

val set : 'a ft_ref -> 'a -> unit

val bind : 'a ft_ref -> ('a -> 'b ft_ref) -> 'b ft_ref
