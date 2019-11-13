extern crate lalrpop;

fn main() {
    lalrpop::Configuration::new()
            .generate_in_source_tree()
            .process_file("../src/grammar.lalrpop")
            .unwrap();
}
