use crate::error::ErrorKind;
use crate::token::{Token, TokenKind};

use regex::Regex;

pub struct Scanner<'a> {
    /// Reference to the string contents we are parsing.
    text: &'a str,
    /// The current line used for processing.
    line: usize,
    /// The current column used for processing.
    column: usize,
    /// The current token index used for processing.
    index: usize,
    /// Contains the regular expression by which tokens are recognized from the input.
    regex: Regex,
}

impl<'a> Scanner<'a> {
    /// Constructs a new scanner.
    pub fn new(text: &'a str) -> Self {
        let regex_groups: Vec<_> = Token::lexical_analysis_order()
            .map(|t| format!("(?P<{}>{})", t.name(), t.pattern().unwrap()))
            .collect();
        let regex = regex_groups.join("|");

        Self {
            text,
            line: 0,
            column: 0,
            index: 0,
            regex: Regex::new(&regex).unwrap(),
        }
    }

    /// Constructs a new error token from an error kind.
    pub fn error_token(&self, kind: ErrorKind, size: usize) -> Token<'a> {
        Token::new(
            TokenKind::Error(kind),
            self.line,
            self.column,
            self.index,
            size,
        )
    }

    fn newline_occurences(&self, index: usize) -> Vec<usize> {
        self.text[0..index]
            .match_indices('\n')
            .map(|x| x.0)
            .collect()
    }

    /// Function that updates the current line and column based on the new index
    /// the text string will be sliced into.
    fn update_line_column_for_new_index(&mut self, new_index: usize) {
        let newline_occurences = self.newline_occurences(new_index);
        let amount_of_newlines = newline_occurences.len();

        if amount_of_newlines == 0 {
            self.column += new_index;
        } else {
            self.line += amount_of_newlines;
            self.column = new_index - newline_occurences[amount_of_newlines - 1] - 1;
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.text.is_empty() {
            return None;
        }

        loop {
            if self.text.starts_with(char::is_whitespace) {
                // Skip whitespace characters

                let c = self.text.chars().next().unwrap();
                if c == '\n' {
                    self.line += 1;
                    self.column = 0;
                } else {
                    self.column += c.len_utf8();
                }
                self.index += c.len_utf8();
                self.text = &self.text[c.len_utf8()..];
            } else if self.text.starts_with("//") {
                // Skip single line comments

                let line_index = self.text.find('\n')?;
                self.line += 1;
                self.column = 0;
                self.index += line_index + 1;
                self.text = &self.text[line_index + 1..];
            } else if self.text.starts_with("/*") {
                match self.text.find("*/") {
                    Some(close_index) => {
                        // Remove multi-line comments

                        for c in self.text[..close_index + 2].chars() {
                            if c == '\n' {
                                self.line += 1;
                                self.column = 0;
                            } else {
                                self.column += c.len_utf8();
                            }
                        }
                        self.index += close_index + 2;
                        self.text = &self.text[close_index + 2..];
                    }
                    None => {
                        // Return an error for unclosed multi-line comments

                        let err = Token {
                            kind: TokenKind::Error(ErrorKind::UnclosedMultiLineComment),
                            line: self.line,
                            column: self.column,
                            index: self.index,
                            size: self.text.len(),
                        };

                        // Skip the rest of the text
                        for c in self.text.chars() {
                            if c == '\n' {
                                self.line += 1;
                                self.column = 0;
                            } else {
                                self.column += c.len_utf8();
                            }
                        }
                        self.text = "";

                        return Some(err);
                    }
                }
            } else {
                break;
            }
        }

        // Match for tokens
        if let Some(captures) = self.regex.captures(self.text) {
            for token_kind in Token::lexical_analysis_order() {
                if let Some(m) = captures.name(token_kind.name()) {
                    if m.start() != 0 {
                        let illegal_token = self.text[0..m.start()].to_string();
                        let e = Some(
                            self.error_token(ErrorKind::IllegalToken(illegal_token), m.start()),
                        );

                        self.update_line_column_for_new_index(m.start());

                        self.index += m.start();
                        self.text = &self.text[m.start()..];

                        return e;
                    }

                    let mut token =
                        Token::new(token_kind, self.line, self.column, self.index, m.end());

                    match token.kind {
                        TokenKind::Integer(_) => {
                            token.kind = TokenKind::Integer(m.as_str().parse().unwrap())
                        }
                        TokenKind::Bool(_) => token.kind = TokenKind::Bool(m.as_str() == "True"),
                        TokenKind::Char(_) => {
                            let matched = m.as_str();
                            let chars = &matched[1..matched.len() - 1];
                            let c = match chars {
                                "\\'" => '\'',
                                "\\\\" => '\\',
                                "\\n" => '\n',
                                "\\r" => '\r',
                                "\\t" => '\t',
                                _ if chars.starts_with('\\') => {
                                    let e = Some(self.error_token(
                                        ErrorKind::IllegalEscape(chars.to_string()),
                                        m.end(),
                                    ));

                                    self.update_line_column_for_new_index(m.end());

                                    self.index += m.end();
                                    self.text = &self.text[m.end()..];

                                    return e;
                                }
                                _ => chars.chars().next().unwrap(),
                            };

                            token.kind = TokenKind::Char(c);
                        }
                        TokenKind::String(_) => {
                            let matched = m.as_str();
                            let string = matched[1..matched.len() - 1]
                                .replace(r#"\""#, r#"""#)
                                .replace(r"\\", r"\")
                                .replace(r"\n", "\n")
                                .replace(r"\r", "\r")
                                .replace(r"\t", "\t");

                            // TODO: handle illegal escapes

                            token.kind = TokenKind::String(string);
                        }
                        TokenKind::Identifier(_) => token.kind = TokenKind::Identifier(m.as_str()),
                        _ => {}
                    }

                    self.update_line_column_for_new_index(m.end());

                    self.index += m.end();
                    self.text = &self.text[m.end()..];

                    return Some(token);
                }
            }
        }

        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn single_token_test_helper(text: &str, expected_token: TokenKind) {
        let mut scanner = Scanner::new(text);

        assert_eq!(scanner.next(), Some(Token::new(expected_token, 0, 0)));
        assert_eq!(scanner.next(), None);
    }

    #[test]
    fn test_remove_single_line_comment() {
        let mut scanner = Scanner::new("// This is a comment.");
        assert_eq!(scanner.next(), None);
    }

    #[test]
    fn test_remove_multi_line_comment() {
        let multiline_comment = r"/*
        This is the first line of the comment.
        This is the second line of the comment.
        This is the third line of the comment.
        */";

        let mut scanner = Scanner::new(multiline_comment);
        assert_eq!(scanner.next(), None);
    }

    #[test]
    fn test_remove_broken_multi_line_comment() {
        let broken_comment = r"/*
        This is a broken comment";

        let mut scanner = Scanner::new(broken_comment);
        assert_eq!(
            scanner.next(),
            Some(Token {
                kind: TokenKind::Error(ErrorKind::UnclosedMultiLineComment),
                line: 0,
                column: 0,
            })
        );
    }

    #[test]
    fn test_double_colon() {
        single_token_test_helper("::", TokenKind::DoubleColon);
    }

    #[test]
    fn test_right_arrow() {
        single_token_test_helper("->", TokenKind::RightArrow);
    }

    #[test]
    fn test_plus() {
        single_token_test_helper("+", TokenKind::Plus);
    }

    #[test]
    fn test_minus() {
        single_token_test_helper("-", TokenKind::Minus);
    }

    #[test]
    fn test_divide() {
        single_token_test_helper("/", TokenKind::Divide);
    }

    #[test]
    fn test_times() {
        single_token_test_helper("*", TokenKind::Times);
    }

    #[test]
    fn test_modulo() {
        single_token_test_helper("%", TokenKind::Modulo);
    }

    #[test]
    fn test_equals() {
        single_token_test_helper("==", TokenKind::Equals);
    }

    #[test]
    fn test_lt() {
        single_token_test_helper("<", TokenKind::Lt);
    }

    #[test]
    fn test_gt() {
        single_token_test_helper(">", TokenKind::Gt);
    }

    #[test]
    fn test_le() {
        single_token_test_helper("<=", TokenKind::Le);
    }

    #[test]
    fn test_ge() {
        single_token_test_helper(">=", TokenKind::Ge);
    }

    #[test]
    fn test_not_equals() {
        single_token_test_helper("!=", TokenKind::NotEquals);
    }

    #[test]
    fn test_and() {
        single_token_test_helper("&&", TokenKind::And);
    }

    #[test]
    fn test_or() {
        single_token_test_helper("||", TokenKind::Or);
    }

    #[test]
    fn test_cons() {
        single_token_test_helper(":", TokenKind::Cons);
    }

    #[test]
    fn test_not() {
        single_token_test_helper("!", TokenKind::Not);
    }

    #[test]
    fn test_assignment() {
        single_token_test_helper("=", TokenKind::Assignment);
    }

    #[test]
    fn test_positive_integer() {
        single_token_test_helper("562", TokenKind::Integer(562u32.into()));

        single_token_test_helper(
            "123456789123456789123456789123456789",
            TokenKind::Integer("123456789123456789123456789123456789".parse().unwrap()),
        );
    }

    #[test]
    fn test_true_boolean() {
        single_token_test_helper("True", TokenKind::Bool(true));
    }

    #[test]
    fn test_false_boolean() {
        single_token_test_helper("False", TokenKind::Bool(false));
    }

    #[test]
    fn test_char() {
        single_token_test_helper("'c'", TokenKind::Char('c'));

        single_token_test_helper("'+'", TokenKind::Char('+'));

        single_token_test_helper("'\\''", TokenKind::Char('\''));
        single_token_test_helper("'\\\\'", TokenKind::Char('\\'));
        single_token_test_helper("'\\n'", TokenKind::Char('\n'));
        single_token_test_helper("'\\r'", TokenKind::Char('\r'));
        single_token_test_helper("'\\t'", TokenKind::Char('\t'));
        single_token_test_helper(
            "'\\x'",
            TokenKind::Error(ErrorKind::IllegalEscape("\\x".to_string())),
        );
    }

    #[test]
    fn test_string() {
        single_token_test_helper(
            r#""Hello world""#,
            TokenKind::String("Hello world".to_string()),
        );

        single_token_test_helper(r#""\"""#, TokenKind::String(r#"""#.to_string()));

        single_token_test_helper(r#""\\""#, TokenKind::String(r#"\"#.to_string()));

        single_token_test_helper(r#""\n""#, TokenKind::String("\n".to_string()));

        single_token_test_helper(r#""\r""#, TokenKind::String("\r".to_string()));

        single_token_test_helper(r#""\t""#, TokenKind::String("\t".to_string()));

        // TODO: illegal escapes
    }

    #[test]
    fn test_identifier() {
        single_token_test_helper(&"func_name", TokenKind::Identifier("func_name"));
        single_token_test_helper("ifibutnotif", TokenKind::Identifier("ifibutnotif"));

        single_token_test_helper("if1", TokenKind::Identifier("if1"));
    }

    #[test]
    fn test_var() {
        single_token_test_helper("var", TokenKind::Var);
    }

    #[test]
    fn test_if() {
        single_token_test_helper("if", TokenKind::If);
    }

    #[test]
    fn test_else() {
        single_token_test_helper("else", TokenKind::Else);
    }

    #[test]
    fn test_while() {
        single_token_test_helper("while", TokenKind::While);
    }

    #[test]
    fn test_return() {
        single_token_test_helper("return", TokenKind::Return);
    }

    #[test]
    fn test_int_type() {
        single_token_test_helper("Int", TokenKind::IntType);
    }

    #[test]
    fn test_bool_type() {
        single_token_test_helper("Bool", TokenKind::BoolType);
    }

    #[test]
    fn test_char_type() {
        single_token_test_helper("Char", TokenKind::CharType);
    }

    #[test]
    fn test_void_type() {
        single_token_test_helper("Void", TokenKind::VoidType);
    }

    #[test]
    fn test_hd() {
        single_token_test_helper(".hd", TokenKind::Hd);
    }

    #[test]
    fn test_tl() {
        single_token_test_helper(".tl", TokenKind::Tl);
    }

    #[test]
    fn test_fst() {
        single_token_test_helper(".fst", TokenKind::Fst);
    }

    #[test]
    fn test_snd() {
        single_token_test_helper(".snd", TokenKind::Snd);
    }

    #[test]
    fn test_empty_list() {
        single_token_test_helper("[]", TokenKind::EmptyList);
    }

    #[test]
    fn test_semicolon() {
        single_token_test_helper(";", TokenKind::Semicolon);
    }

    #[test]
    fn test_comma() {
        single_token_test_helper(",", TokenKind::Comma);
    }

    #[test]
    fn test_opening_paren() {
        single_token_test_helper("(", TokenKind::OpeningParen);
    }

    #[test]
    fn test_closing_paren() {
        single_token_test_helper(")", TokenKind::ClosingParen);
    }

    #[test]
    fn test_opening_brace() {
        single_token_test_helper("{", TokenKind::OpeningBrace);
    }

    #[test]
    fn test_closing_brace() {
        single_token_test_helper("}", TokenKind::ClosingBrace);
    }

    #[test]
    fn test_opening_square() {
        single_token_test_helper("[", TokenKind::OpeningSquare);
    }

    #[test]
    fn test_closing_square() {
        single_token_test_helper("]", TokenKind::ClosingSquare);
    }

    #[test]
    fn test_integer_evaluation_loop() {
        let text = r"1-2-3;";
        let mut scanner = Scanner::new(text);

        assert_eq!(
            scanner.next(),
            Some(Token::new(TokenKind::Integer(1u32.into()), 0, 0))
        );
        assert_eq!(scanner.next(), Some(Token::new(TokenKind::Minus, 0, 1)));
        assert_eq!(
            scanner.next(),
            Some(Token::new(TokenKind::Integer(2u32.into()), 0, 2))
        );
        assert_eq!(scanner.next(), Some(Token::new(TokenKind::Minus, 0, 3)));
        assert_eq!(
            scanner.next(),
            Some(Token::new(TokenKind::Integer(3u32.into()), 0, 4))
        );
        assert_eq!(scanner.next(), Some(Token::new(TokenKind::Semicolon, 0, 5)));
    }

    #[test]
    fn test_while_loop() {
        let text = r"  // Some commentary on this code...
                            while(month < 12) {}";

        let mut scanner = Scanner::new(text);

        assert_eq!(scanner.next(), Some(Token::new(TokenKind::While, 1, 28)));
        assert_eq!(
            scanner.next(),
            Some(Token::new(TokenKind::OpeningParen, 1, 33))
        );
        assert_eq!(
            scanner.next(),
            Some(Token::new(TokenKind::Identifier("month"), 1, 34))
        );
        assert_eq!(scanner.next(), Some(Token::new(TokenKind::Lt, 1, 40)));
        assert_eq!(
            scanner.next(),
            Some(Token::new(TokenKind::Integer(12u32.into()), 1, 42))
        );
        assert_eq!(
            scanner.next(),
            Some(Token::new(TokenKind::ClosingParen, 1, 44))
        );
        assert_eq!(
            scanner.next(),
            Some(Token::new(TokenKind::OpeningBrace, 1, 46))
        );
        assert_eq!(
            scanner.next(),
            Some(Token::new(TokenKind::ClosingBrace, 1, 47))
        );
    }

    #[test]
    pub fn test_correct_line_column_updates() {
        let text = "{\n_illegal\n}";

        let mut scanner = Scanner::new(text);

        assert_eq!(scanner.line, 0);
        assert_eq!(scanner.column, 0);

        scanner.update_line_column_for_new_index(2);
        scanner.text = &scanner.text[2..];
        assert_eq!(scanner.line, 1);
        assert_eq!(scanner.column, 0);

        scanner.update_line_column_for_new_index(2);
        scanner.text = &scanner.text[2..];
        assert_eq!(scanner.line, 1);
        assert_eq!(scanner.column, 2);

        scanner.update_line_column_for_new_index(7);
        scanner.text = &scanner.text[7..];
        assert_eq!(scanner.line, 2);
        assert_eq!(scanner.column, 0);
    }
}
