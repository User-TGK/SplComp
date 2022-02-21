use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;

fn write_test(file: &mut File, path: &Path) {
    let name = path.file_stem().unwrap().to_str().unwrap();
    let content = fs::read_to_string(path).unwrap();
    write!(
        file,
        r##"
#[test]
fn test_{name}() {{
    const CODE: &str = r#"
{content}
"#;
    let tokens: Vec<Token> = Scanner::new(CODE).collect();
    let tokens = Tokens::new(&tokens);

    let (rest, _program) = program_parser(tokens).unwrap();

    assert!(rest.is_empty(), "Unconsumed input: {{:#?}}", rest);
}}
"##
    )
    .unwrap();
}

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("tests_gen.rs");
    let mut dest_file = File::create(dest_path).unwrap();

    let ignored = [
        "x.spl",
        "many_parenthesis.spl",
        "unbalanced_parenthesis2.spl",
        "more_parenthesis.spl",
        "unbalanced_parenthesis.spl",
    ];

    for entry in fs::read_dir("material/tests/").unwrap() {
        let entry = entry.unwrap();

        if ignored.contains(&entry.file_name().to_str().unwrap()) {
            continue;
        }

        write_test(&mut dest_file, &entry.path());
    }
}
