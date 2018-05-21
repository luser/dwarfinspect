extern crate dwarfinspect;

fn main() {
    match dwarfinspect::main() {
        Ok(_) => {}
        Err(e) => eprintln!("{}", e),
    }
}
