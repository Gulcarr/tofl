use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::str::Chars;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
enum RegexAST {
    Concatenation(Box<RegexAST>, Box<RegexAST>),
    Alternation(Box<RegexAST>, Box<RegexAST>),
    Group(Box<RegexAST>),
    NonCapturingGroup(Box<RegexAST>),
    Star(Box<RegexAST>),
    BackReference(u32),
    GroupReference(u32),
    Char(char),
    Empty,
}

impl Clone for RegexAST {
    fn clone(&self) -> Self {
        match self {
            RegexAST::Concatenation(a, b) => RegexAST::Concatenation(Box::new(*a.clone()), Box::new(*b.clone())),
            RegexAST::Alternation(a, b) => RegexAST::Alternation(Box::new(*a.clone()), Box::new(*b.clone())),
            RegexAST::Group(a) => RegexAST::Group(Box::new(*a.clone())),
            RegexAST::NonCapturingGroup(a) => RegexAST::NonCapturingGroup(Box::new(*a.clone())),
            RegexAST::Star(a) => RegexAST::Star(Box::new(*a.clone())),
            RegexAST::BackReference(a) => RegexAST::BackReference(*a),
            RegexAST::GroupReference(a) => RegexAST::GroupReference(*a),
            RegexAST::Char(a) => RegexAST::Char(*a),
            RegexAST::Empty => RegexAST::Empty,
        }
    }
}

struct Parser<'a> {
    input: Peekable<Chars<'a>>,
    group_count: u32,
    group_map: HashMap<u32, Box<RegexAST>>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Parser {
            input: input.chars().peekable(),
            group_count: 0,
            group_map: HashMap::new(),
        }
    }

    fn parse(&mut self) -> Result<RegexAST, String> {
        let ast = self.parse_regex()?;

        if self.group_count > 9 {
            return Err("Too many capturing groups (max 9 allowed)".to_string());
        }

        if let Some(c) = self.input.peek() {
            return Err(format!("Unexpected character '{}' after valid regex", c));
        }

        Ok(ast)
    }

    fn parse_regex(&mut self) -> Result<RegexAST, String> {
        if let Some(&')') = self.input.peek() {
            return Ok(RegexAST::Empty);
        }

        let mut left = self.parse_term()?;

        while let Some(&c) = self.input.peek() {
            match c {
                '|' => {
                    self.input.next();
                    let right = self.parse_term()?;
                    left = RegexAST::Alternation(Box::new(left), Box::new(right));
                }
                ')' => break,
                _ => {
                    if let Ok(right) = self.parse_term() {
                        left = RegexAST::Concatenation(Box::new(left), Box::new(right));
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(left)
    }

    fn check_star(&mut self, ast: RegexAST) -> RegexAST {
        if let Some(&'*') = self.input.peek() {
            self.input.next();
            RegexAST::Star(Box::new(ast))
        } else {
            ast
        }
    }

    fn parse_term(&mut self) -> Result<RegexAST, String> {
        let ast = match self.input.peek() {
            Some(&'(') => {
                self.input.next();
                match self.input.peek() {
                    Some(&'?') => {
                        self.input.next();
                        match self.input.peek() {
                            Some(&c) if c.is_ascii_digit() && c != '0' => {
                                self.input.next();
                                let num = c.to_digit(10).unwrap() as u32;
                                match self.input.next() {
                                    Some(')') => Ok(RegexAST::GroupReference(num)),
                                    _ => Err("Expected closing parenthesis after group reference".to_string()),
                                }
                            }
                            Some(&':') => {
                                self.input.next();
                                let expr = self.parse_regex()?;
                                match self.input.next() {
                                    Some(')') => Ok(RegexAST::NonCapturingGroup(Box::new(expr))),
                                    _ => Err("Expected closing parenthesis after non-capturing group".to_string()),
                                }
                            }
                            _ => Err("Invalid syntax after '(?'. Expected digit or ':'".to_string()),
                        }
                    }
                    _ => {
                        self.group_count += 1;
                        let current_group = self.group_count;
                        let expr = self.parse_regex()?;
                        match self.input.next() {
                            Some(')') => {
                                let group_ast = RegexAST::Group(Box::new(expr.clone()));
                                self.group_map.insert(current_group, Box::new(expr));
                                Ok(group_ast)
                            },
                            _ => Err("Expected closing parenthesis".to_string()),
                        }
                    }
                }
            }
            Some(&'\\') => {
                self.input.next();
                self.parse_backreference()
            }
            Some(&c) if c.is_ascii_lowercase() => {
                self.input.next();
                Ok(RegexAST::Char(c))
            }
            Some(&c) => Err(format!("Unexpected character '{}'", c)),
            None => Err("Unexpected end of input".to_string()),
        }?;

        Ok(self.check_star(ast))
    }

    fn parse_backreference(&mut self) -> Result<RegexAST, String> {
        match self.input.next() {
            Some(c) if c.is_ascii_digit() && c != '0' => {
                let num = c.to_digit(10).unwrap() as u32;
                Ok(RegexAST::BackReference(num))
            }
            _ => Err("Invalid backreference".to_string()),
        }
    }

    fn get_group_description(&self, group_num: u32) -> Option<&RegexAST> {
        self.group_map.get(&group_num).map(|boxed| boxed.as_ref())
    }
}

#[derive(Debug)]
struct SemanticChecker {
    group_count: u32,
    group_map: HashMap<u32, Box<RegexAST>>,
}

#[derive(Clone, Debug)]
struct ValidationContext {
    initialized_groups: HashSet<u32>,
    completed_groups: HashSet<u32>,
    in_star: bool,
    in_alt: bool,
    current_alt_groups: HashSet<u32>,
    current_group: Option<u32>,
}

impl ValidationContext {
    fn new() -> Self {
        ValidationContext {
            initialized_groups: HashSet::new(),
            completed_groups: HashSet::new(),
            in_star: false,
            in_alt: false,
            current_alt_groups: HashSet::new(),
            current_group: None,
        }
    }

    fn with_star(&self) -> Self {
        ValidationContext {
            initialized_groups: self.initialized_groups.clone(),
            completed_groups: self.completed_groups.clone(),
            in_star: true,
            in_alt: self.in_alt,
            current_alt_groups: self.current_alt_groups.clone(),
            current_group: self.current_group,
        }
    }

    fn with_alt(&self) -> Self {
        ValidationContext {
            initialized_groups: self.initialized_groups.clone(),
            completed_groups: self.completed_groups.clone(),
            in_star: self.in_star,
            in_alt: true,
            current_alt_groups: HashSet::new(),
            current_group: self.current_group,
        }
    }
}

impl SemanticChecker {
    fn new(group_count: u32, group_map: HashMap<u32, Box<RegexAST>>) -> Self {
        SemanticChecker {
            group_count,
            group_map,
        }
    }

    fn validate(&self, ast: &RegexAST) -> Result<(), String> {
        let mut context = ValidationContext::new();
        self.validate_node(ast, &mut context, None)
    }

    fn validate_node(&self, ast: &RegexAST, context: &mut ValidationContext, group_number: Option<u32>) -> Result<(), String> {
        match ast {
            RegexAST::Concatenation(left, right) => {
                self.validate_node(left, context, None)?;
                self.validate_node(right, context, None)
            }

            RegexAST::Alternation(left, right) => {
                let mut left_context = context.with_alt();
                let mut right_context = context.with_alt();

                self.validate_node(left, &mut left_context, None)?;
                self.validate_node(right, &mut right_context, None)?;
                Ok(())
            }

            RegexAST::Group(inner) => {
                let group_num = group_number.unwrap_or_else(|| {
                    context.current_group.map(|n| n + 1).unwrap_or(1)
                });

                let prev_group = context.current_group;
                context.current_group = Some(group_num);

                if !context.in_star && !context.in_alt {
                    context.initialized_groups.insert(group_num);
                } else if context.in_alt {
                    context.current_alt_groups.insert(group_num);
                }

                self.validate_node(inner, context, Some(group_num))?;

                context.completed_groups.insert(group_num);
                context.current_group = prev_group;

                Ok(())
            }

            RegexAST::NonCapturingGroup(inner) => {
                self.validate_node(inner, context, None)
            }

            RegexAST::Star(inner) => {
                let mut star_context = context.with_star();
                self.validate_node(inner, &mut star_context, None)
            }

            RegexAST::GroupReference(num) => {
                if *num > self.group_count {
                    return Err(format!("Reference to non-existent group {} (total groups: {})", num, self.group_count));
                }

                let mut ref_context = ValidationContext {
                    initialized_groups: context.initialized_groups.clone(),
                    completed_groups: context.completed_groups.clone(),
                    in_star: context.in_star,
                    in_alt: context.in_alt,
                    current_alt_groups: context.current_alt_groups.clone(),
                    current_group: Some(*num),
                };

                if let Some(group_content) = self.group_map.get(num) {
                    self.validate_node(group_content, &mut ref_context, Some(*num))?;
                }
                Ok(())
            }

            RegexAST::BackReference(num) => {
                if *num > self.group_count {
                    return Err(format!("Reference to non-existent group {}", num));
                }

                if !context.completed_groups.contains(num) {
                    return Err(format!("Reference to uncompleted group {}", num));
                }

                if !context.initialized_groups.contains(num) &&
                    !context.current_alt_groups.contains(num) {
                    return Err(format!("Reference to uninitialized group {}", num));
                }
                Ok(())
            }

            RegexAST::Char(_) | RegexAST::Empty => Ok(()),
        }
    }

}

fn main() {
    let test_cases = vec![
        "ab",
        "a|b",
        "(ab)",
        "(?:ab)",
        "a*",
        r"\1",
        r"a(b|c)d*",
        r"(a)(?:b)(c)",
        r"(a)(b)(c)(d)(e)(f)(g)(h)(i)",
        r"(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)",
        r"(a)(?1)",
        "(ab)*",
        "(?:ab)*",
        "(?1)*",
        "(?a)",
        "(?0)",
        "(?:)",
        "a*",
        r"\1*",
        r"a(b|c)*d*",
        r"(a)(?:b)*(c)",
        r"(a)(b)*(?2)",
        r"(a)?k",
        ")",
        r"((b)*)\2",
        r"(a)\1",
        r"(a\1)",
        r"(?2)",
        r"(a(b)|(c))\3",
        r"(a(b)|(c))(?3)",
        r"(a|(?2))(a|(bb\1))",
        r"(a)b(?3)(a)(a(?4)(g\1))",
        r"(a)b(?3)(a)(a(?4)(g\2))",
    ];

    for test in test_cases {
        println!("Parsing: {}", test);
        let mut parser = Parser::new(test);
        match parser.parse() {
            Ok(ast) => {
                //println!("AST: {:?}", ast);
                let mut checker = SemanticChecker::new(parser.group_count, parser.group_map);
                match checker.validate(&Box::new(ast.clone())) {
                    Ok(_) => (println!("coooool")),
                    Err(e) => println!("problem {}", e),
                }
            },
            Err(e) => println!("Error: {}", e),
        }
    }
}