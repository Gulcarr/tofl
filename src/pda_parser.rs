use std::collections::{HashMap, HashSet, VecDeque};
use crate::rules_parser::Symbol;


#[derive(Debug)]
struct ParseNode {
    symbols: Vec<Symbol>,
    matched: String,
    remaining: String,
    depth: usize,
}

pub fn parse_word(word: &str, grammar: &HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>>) -> bool {
    let start_symbol = Symbol::NonTerminal("S".to_string());
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();

     queue.push_back(ParseNode {
         symbols: vec![start_symbol],
         matched: String::new(),
         remaining: word.to_string(),
         depth: 0,
     });

    while let Some(node) = queue.pop_front() {
        let state_key = (node.symbols.clone(), node.matched.clone(), node.remaining.clone());
        if visited.contains(&state_key) {
            continue;
        }
        visited.insert(state_key);

        if node.symbols.is_empty() && node.remaining.is_empty() {
            return true;
        }

        if node.symbols.is_empty() {
            continue;
        }

        if node.depth < 17 {
            match &node.symbols[0] {
                Symbol::Terminal(c) => {
                    if let Some(next_char) = node.remaining.chars().next() {
                        if *c == next_char {
                            queue.push_back(ParseNode {
                                symbols: node.symbols[1..].to_vec(),
                                matched: format!("{}{}", node.matched, next_char),
                                remaining: node.remaining[1..].to_string(),
                                depth: node.depth,
                            });
                        }
                    }
                }
                Symbol::NonTerminal(nt) => {
                    if let Some(rules) = grammar.get(&Symbol::NonTerminal(nt.clone())) {
                        for (prefix, productions) in rules {
                            if prefix.is_empty() || node.remaining.starts_with(prefix) {
                                for production in productions {
                                    let mut new_symbols = production.clone();
                                    new_symbols.extend_from_slice(&node.symbols[1..]);

                                    queue.push_back(ParseNode {
                                        symbols: new_symbols,
                                        matched: node.matched.clone(),
                                        remaining: node.remaining.clone(),
                                        depth: node.depth + 1,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    false
}