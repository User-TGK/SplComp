use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;

fn write_test(file: &mut File, path: &Path, should_parse: bool, should_type_check: bool) {
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
    let tokens: Vec<Token> = Scanner::new(CODE).unwrap().collect();
    let tokens = Tokens::new(&tokens, CODE);

    let result = program_parser(tokens);

    match {should_parse} {{
        true => {{
            assert!(result.is_ok());
            
            let (rest, mut program) = result.unwrap();
            assert!(rest.is_empty(), "Unconsumed input: {{:#?}}", rest);

            let funcs: Vec<String> = program.fun_decls.iter().map(|f| f.name.clone().0).collect();
            if !funcs.contains(&String::from("main")) {{
                program.fun_decls.push(FunDecl {{
                    name: Id(String::from("main")),
                    params: vec![],
                    fun_type: None,
                    var_decls: vec![],
                    statements: vec![Statement::Return(None)],
                }});
            }}

            match {should_type_check} {{
                true => {{
                    assert!(run(&mut program).is_ok());
                }}
                false => {{
                    assert!(run(&mut program).is_err());
                }}
            }}
        }}
        false => {{
            assert!(result.is_err());
        }}
    }}
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
        "higher_order_functions.spl",
        "list_ops.spl",
        "monomorph.spl",
        "stress.spl",
    ];

    let should_fail_parser = [
        "unbalanced_parenthesis2.spl",
        "unbalanced_parenthesis.spl",
        "empty.spl",
    ];

    let should_fail_type_checker = [
        "arguments.spl",
        "assignment_to_builtin.spl",
        "infinite_type_shouldfail.spl",
        "multiple_recursion_values.spl",
        "return_ill_typed.spl",
        "return_in_all_code_paths.spl",
        "self_application_shouldfail.spl",
        "sieve.spl",
        "too_general_function_type.spl",
        "problematic.spl",
        "polymorphic_value_again_shouldfail.spl",
        "polymorphic_value_indirect_shouldfail.spl",
        "polymorphic_value_shouldfail.spl",
    ];

    for entry in fs::read_dir("material/tests/").unwrap() {
        let entry = entry.unwrap();

        if ignored.contains(&entry.file_name().to_str().unwrap()) {
            continue;
        }

        let should_parse = !should_fail_parser.contains(&entry.file_name().to_str().unwrap());
        let should_type_check =
            !should_fail_type_checker.contains(&entry.file_name().to_str().unwrap());

        write_test(
            &mut dest_file,
            &entry.path(),
            should_parse,
            should_type_check,
        );
    }
}
