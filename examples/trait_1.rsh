trait Trait {
    fn function(param: i32) -> out;

    fn deffn(param: i32) -> out {
        out
    }
}

trait<T> GenericTrait {
    fn genfn<U>(f: T) -> U;
}
