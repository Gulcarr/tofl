use std::collections::HashMap;
use crate::rules_parser::Symbol;

fn dfs<'a>(
    remaining_word: &'a str,
    current_symbol: &Symbol,
    table: &HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>>,
    path: &mut Vec<Vec<Symbol>>,
    depth: usize
) -> Option<&'a str> {
    if depth > 100 {
        return None;
    }

    if remaining_word.is_empty() {
        return None;
    }

    match current_symbol {
        Symbol::Terminal(c) => {
            if remaining_word.chars().next() == Some(*c) {
                Some(&remaining_word[1..])
            } else {
                None
            }
        }
        Symbol::NonTerminal(_) => {
            if let Some(rules) = table.get(current_symbol) {
                let first_char = remaining_word.chars().next().unwrap();
                let mut prefix = first_char.to_string();

                if remaining_word.len() >= 2 {
                    let two_char_prefix = remaining_word[..2].to_string();
                    if rules.contains_key(&two_char_prefix) {
                        prefix = two_char_prefix;
                    }
                }

                if remaining_word.len() >= 3 {
                    let three_char_prefix = remaining_word[..3].to_string();
                    if rules.contains_key(&three_char_prefix) {
                        prefix = three_char_prefix;
                    }
                }

                if let Some(productions) = rules.get(&prefix) {
                    for production in productions {
                        let mut current_remaining = remaining_word;
                        let mut production_path = Vec::new();
                        let mut success = true;

                        for symbol in production {
                            if let Some(new_remaining) = dfs(current_remaining, symbol, table, &mut production_path, depth + 1) {
                                current_remaining = new_remaining;
                            } else {
                                success = false;
                                break;
                            }
                        }

                        if success {
                            path.extend(production_path);
                            path.push(production.clone());
                            return Some(current_remaining);
                        }
                    }
                }
            }
            None
        }
    }
}

pub fn parse_word(
    word: &str,
    start_symbol: &Symbol,
    table: &HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>>
) -> Option<Vec<Vec<Symbol>>> {


    let mut derivation = Vec::new();
    if let Some(remaining) = dfs(word, start_symbol, table, &mut derivation, 0) {
        if remaining.is_empty() {
            derivation.reverse();
            Some(derivation)
        } else {
            None
        }
    } else {
        None
    }
}