#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

fn main() {
    test_late_init();
}

fn test_late_init() {
    #[generator]
    fn gen() -> u32 {
        let a;
        yield_!(1u32);
        a = 9;
        yield_!(2);
        yield_!(3);
        println!("{:?}", a);
    }

    let mut gen = gen();
    assert_eq!(gen.next(), Some(1));
    assert_eq!(gen.next(), Some(2));
    assert_eq!(gen.next(), Some(3));
    assert_eq!(gen.next(), None);
}
