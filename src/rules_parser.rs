use std::collections::HashMap;
use std::fmt::Display;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
    Terminal(char),
    NonTerminal(String),
}

pub(crate) fn parse_grammar(input: &str) -> Result<HashMap<Symbol, Vec<Vec<Symbol>>>, String> {
    let mut rules: HashMap<Symbol, Vec<Vec<Symbol>>> = HashMap::new();

    for line in input.lines() {
        if line.trim().is_empty() {
            continue;
        }

        // Split the line into left and right parts
        let parts: Vec<&str> = line.split("->").collect();
        if parts.len() != 2 {
            return Err(format!("Invalid rule format: {}", line));
        }

        // Parse left side (the key)
        let left = parse_symbol(parts[0].trim())?;

        // Parse right side into a vector of symbols
        let right = parse_right_side(parts[1].trim())?;

        // Add to rules
        rules.entry(left)
            .or_insert_with(Vec::new)
            .push(right);
    }

    Ok(rules)
}

fn parse_symbol(s: &str) -> Result<Symbol, String> {
    let s = s.trim();

    if s.len() == 1 {
        let c = s.chars().next().unwrap();
        if c.is_lowercase() {
            Ok(Symbol::Terminal(c))
        } else if c.is_uppercase() {
            Ok(Symbol::NonTerminal(c.to_string()))
        } else {
            Err(format!("Invalid symbol: {}", s))
        }
    } else if s.len() == 2 && s.chars().next().unwrap().is_uppercase() {
        let mut chars = s.chars();
        let first = chars.next().unwrap();
        let second = chars.next().unwrap();
        if first.is_uppercase() && second.is_digit(10) {
            Ok(Symbol::NonTerminal(s.to_string()))
        } else {
            Err(format!("Invalid non-terminal format: {}", s))
        }
    } else if s.starts_with('[') && s.ends_with(']') {
        let content = &s[1..s.len()-1];

        // Split content into letters and numbers
        let mut letters = String::new();
        let mut numbers = String::new();
        let mut seen_number = false;

        for c in content.chars() {
            if c.is_alphabetic() {
                if seen_number {
                    return Err(format!("Invalid format: letters after numbers in {}", s));
                }
                letters.push(c);
            } else if c.is_digit(10) {
                seen_number = true;
                numbers.push(c);
            } else {
                return Err(format!("Invalid character in bracketed expression: {}", c));
            }
        }

        if letters.is_empty() {
            return Err(format!("Bracketed expression must contain at least one letter: {}", s));
        }

        Ok(Symbol::NonTerminal(format!("[{}{}]", letters, numbers)))
    } else {
        Err(format!("Invalid symbol format: {}", s))
    }
}

fn parse_right_side(s: &str) -> Result<Vec<Symbol>, String> {
    let mut symbols = Vec::new();
    let mut current = String::new();
    let mut in_brackets = false;

    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        match chars[i] {
            '[' => {
                if in_brackets {
                    return Err("Nested brackets are not allowed".to_string());
                }
                if !current.is_empty() {
                    for c in current.chars() {
                        symbols.push(parse_symbol(&c.to_string())?);
                    }
                    current.clear();
                }
                in_brackets = true;
                current.push('[');
            },
            ']' => {
                if !in_brackets {
                    return Err("Unmatched closing bracket".to_string());
                }
                current.push(']');
                symbols.push(parse_symbol(&current)?);
                current.clear();
                in_brackets = false;
            },
            ' ' | '\t' => {
                if in_brackets {
                    return Err("Spaces not allowed in bracketed expressions".to_string());
                }
                if !current.is_empty() {
                    for c in current.chars() {
                        symbols.push(parse_symbol(&c.to_string())?);
                    }
                    current.clear();
                }
            },
            c => {
                if in_brackets {
                    current.push(c);
                } else {
                    symbols.push(parse_symbol(&c.to_string())?);
                }
            }
        }
        i += 1;
    }

    if !current.is_empty() {
        if in_brackets {
            return Err("Unclosed bracket".to_string());
        }

        for c in current.chars() {
            symbols.push(parse_symbol(&c.to_string())?);
        }
    }

    Ok(symbols)
}
