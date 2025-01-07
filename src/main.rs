mod rules_parser;
mod pda_parser;

use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::{fmt, io};
use std::io::Read;
use crate::pda_parser::parse_word;
use crate::rules_parser::{parse_grammar, Symbol};

fn calculate_first_sets(grammar: &HashMap<Symbol, Vec<Vec<Symbol>>>) -> HashMap<Symbol, HashSet<char>> {
    let mut first_sets: HashMap<Symbol, HashSet<char>> = HashMap::new();

    for key in grammar.keys() {
        first_sets.insert(key.clone(), HashSet::new());
    }

    let mut changed = true;
    while changed {
        changed = false;

        for (non_terminal, productions) in grammar {
            let mut all_firsts = HashSet::new();

            for production in productions {
                match &production[0] {
                    Symbol::Terminal(c) => {
                        all_firsts.insert(*c);
                    },
                    Symbol::NonTerminal(ref nt) => {
                        if let Some(first_set) = first_sets.get(&Symbol::NonTerminal(nt.clone())) {
                            all_firsts.extend(first_set.iter().cloned());
                        }
                    }
                }
            }

            let first_set = first_sets.get_mut(non_terminal).unwrap();
            let old_size = first_set.len();
            first_set.extend(all_firsts);
            if first_set.len() > old_size {
                changed = true;
            }
        }
    }

    first_sets
}

fn process_first_sets_with_productions(grammar: &HashMap<Symbol, Vec<Vec<Symbol>>>, first_sets: &HashMap<Symbol, HashSet<char>>) -> HashMap<Symbol, HashMap<char, Vec<Vec<Symbol>>>> {
    let mut result: HashMap<Symbol, HashMap<char, Vec<Vec<Symbol>>>> = HashMap::new();

    for (non_terminal, productions) in grammar {
        let mut char_productions: HashMap<char, Vec<Vec<Symbol>>> = HashMap::new();

        for production in productions {
            if production.is_empty() {
                continue;
            }

            let first_symbols = match &production[0] {
                Symbol::Terminal(c) => {
                    vec![*c]
                },
                Symbol::NonTerminal(nt) => {
                    if let Some(first_set) = first_sets.get(&Symbol::NonTerminal(nt.clone())) {
                        first_set.iter().cloned().collect()
                    } else {
                        vec![]
                    }
                }
            };

            for c in first_symbols {
                char_productions
                    .entry(c)
                    .or_insert_with(Vec::new)
                    .push(production.clone());
            }
        }

        if !char_productions.is_empty() {
            result.insert(non_terminal.clone(), char_productions);
        }
    }

    result
}

pub fn find_k_chars(
    k: usize,
    production: &[Symbol],
    grammar: &HashMap<Symbol, Vec<Vec<Symbol>>>,
    first_sets: &HashMap<Symbol, HashMap<char, Vec<Vec<Symbol>>>>,
    mut visited: HashSet<Symbol>,
) -> HashSet<String> {
    let mut result = HashSet::new();

    if production.is_empty() {
        if k > 0 {
            result.insert("".to_string());
        }

        return result;
    }

    if k == 0 {
        return result;
    }

    match &production[0] {
        Symbol::Terminal(c) => {
            if k == 1 {
                result.insert(c.to_string());
            } else if production.len() > 1 {
                result.extend(find_k_chars(
                    k - 1,
                    &production[1..],
                    grammar,
                    first_sets,
                    HashSet::new(),
                ));
            } else {
                result.insert("".to_string());
            }
        }
        Symbol::NonTerminal(nt) => {
            let nt_symbol = Symbol::NonTerminal(nt.clone());

            if !visited.contains(&nt_symbol) {
                visited.insert(nt_symbol.clone());

                if let Some(prods) = grammar.get(&nt_symbol) {
                    for prod in prods {
                        let mut chars_from_prod = find_k_chars(k, prod, grammar, first_sets, visited.clone());
                        result.extend(chars_from_prod.clone());

                        if chars_from_prod.contains("") && production.len() > 1 {
                            let chars_from_rest = find_k_chars(k, &production[1..],
                                grammar, first_sets, visited.clone());
                            result.extend(chars_from_rest);
                        }
                    }
                }
            }
        }
    }

    result
}

pub fn process_grammar_rules_second_pass(
    grammar: &HashMap<Symbol, Vec<Vec<Symbol>>>,
    rules: &HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>>,
    first_sets: &HashMap<Symbol, HashMap<char, Vec<Vec<Symbol>>>>,
) -> HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>> {
    let mut processed_rules: HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>> = HashMap::new();

    for (non_terminal, string_rules) in rules {
        let mut final_rules: HashMap<String, Vec<Vec<Symbol>>> = HashMap::new();

        for (first_char, productions) in string_rules {
            if productions.len() <= 1 {
                final_rules.insert(first_char.clone(), productions.clone());
                continue;
            }

            // Check if we need to differentiate these productions
            let mut need_second_char = false;
            let mut all_second_chars: HashSet<String> = HashSet::new();

            for production in productions.iter() {
                let second_chars = find_k_chars(2, production, grammar, first_sets, HashSet::new());
                if !second_chars.is_empty() && !all_second_chars.is_empty() {
                    if second_chars != all_second_chars {
                        need_second_char = true;
                        break;
                    }
                }
                all_second_chars.extend(second_chars);
            }


            if need_second_char {
                for production in productions {
                    let second_chars = find_k_chars(2, production, grammar, first_sets, HashSet::new());
                    for second_char in second_chars {
                        let key = format!("{}{}", first_char, second_char);
                        final_rules
                            .entry(key)
                            .or_insert_with(Vec::new)
                            .push(production.clone());
                    }
                }
            } else {
                final_rules.insert(first_char.clone(), productions.clone());
            }
        }

        processed_rules.insert(non_terminal.clone(), final_rules);
    }

    processed_rules
}

pub fn process_grammar_rules_third_pass(
    grammar: &HashMap<Symbol, Vec<Vec<Symbol>>>,
    rules: &HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>>,
    first_sets: &HashMap<Symbol, HashMap<char, Vec<Vec<Symbol>>>>,
) -> HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>> {
    let mut processed_rules: HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>> = HashMap::new();

    for (non_terminal, string_rules) in rules {
        let mut final_rules: HashMap<String, Vec<Vec<Symbol>>> = HashMap::new();

        for (prefix, productions) in string_rules {
            if productions.len() <= 1 {
                final_rules.insert(prefix.clone(), productions.clone());
                continue;
            }

            let mut need_third_char = false;
            let mut third_chars_set: HashSet<String> = HashSet::new();

            for production in productions.iter() {
                let third_chars = find_k_chars(3, production, grammar, first_sets, HashSet::new());
                if !third_chars.is_empty() && !third_chars_set.is_empty() {
                    if third_chars != third_chars_set {
                        need_third_char = true;
                        break;
                    }
                }
                third_chars_set.extend(third_chars);
            }

            if need_third_char {
                for production in productions {
                    let third_chars = find_k_chars(3, production, grammar, first_sets, HashSet::new());
                    for third_char in third_chars {
                        let key = format!("{}{}", prefix, third_char);
                        final_rules
                            .entry(key)
                            .or_insert_with(Vec::new)
                            .push(production.clone());
                    }
                }
            } else {
                final_rules.insert(prefix.clone(), productions.clone());
            }
        }

        processed_rules.insert(non_terminal.clone(), final_rules);
    }

    processed_rules
}

pub fn process_grammar_rules(
    grammar: HashMap<Symbol, Vec<Vec<Symbol>>>,
    first_sets: &HashMap<Symbol, HashMap<char, Vec<Vec<Symbol>>>>,
) -> HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>> {
    let mut result: HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>> = HashMap::new();

    for (non_terminal, string_rules) in first_sets {
        let mut final_rules: HashMap<String, Vec<Vec<Symbol>>> = HashMap::new();
        for (key, productions) in string_rules {
            final_rules.insert(key.to_string(), productions.clone());
        }

        result.insert(non_terminal.clone(), final_rules.clone());
    }

    result
}

pub fn format_grammar_table(rules: &HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>>) -> String {
    let mut result = String::new();

    let mut all_strings: HashSet<String> = HashSet::new();
    for (_symbol, string_map) in rules {
        for key in string_map.keys() {
            all_strings.insert(key.clone());
        }
    }
    let mut string_headers: Vec<String> = all_strings.into_iter().collect();
    string_headers.sort();

    let symbol_width = 5;
    let column_width = 8;

    result.push_str(&format!("{:symbol_width$}|", "", symbol_width = symbol_width));
    for header in &string_headers {
        result.push_str(&format!("{:^column_width$}|", header, column_width = column_width));
    }
    result += "\n";

    result.push_str(&"-".repeat(symbol_width + (column_width + 1) * string_headers.len()));
    result += "\n";

    let mut symbols: Vec<_> = rules.keys().collect();
    symbols.sort_by(|a, b| format!("{:?}", a).cmp(&format!("{:?}", b)));

    for symbol in symbols {
        let mut first_row = true;
        let string_map = &rules[symbol];

        let mut all_productions: HashMap<&String, Vec<String>> = HashMap::new();
        let mut max_rows = 1;

        for header in &string_headers {
            if let Some(productions) = string_map.get(header) {
                let mut header_productions = Vec::new();
                for prod in productions {
                    let mut prod_str = match symbol {
                        Symbol::Terminal(c) => c.to_string(),
                        Symbol::NonTerminal(s) => s.clone(),
                    };
                    prod_str.push_str("->");
                    for sym in prod {
                        match sym {
                            Symbol::Terminal(c) => prod_str.push(*c),
                            Symbol::NonTerminal(s) => prod_str.push_str(s),
                        }
                    }

                    if prod_str.len() > column_width {
                        let chunks = (prod_str.len() + column_width - 1) / column_width;
                        for i in 0..chunks {
                            let start = i * column_width;
                            let end = (start + column_width).min(prod_str.len());
                            let chunk = &prod_str[start..end];
                            header_productions.push(chunk.to_string());
                        }
                    } else {
                        header_productions.push(prod_str);
                    }
                }
                max_rows = max_rows.max(header_productions.len());
                all_productions.insert(header, header_productions);
            }
        }

        for row in 0..max_rows {
            if first_row {
                match symbol {
                    Symbol::Terminal(c) => result.push_str(&format!("{:symbol_width$}|", c, symbol_width = symbol_width)),
                    Symbol::NonTerminal(s) => result.push_str(&format!("{:symbol_width$}|", s, symbol_width = symbol_width)),
                }
                first_row = false;
            } else {
                result.push_str(&format!("{:symbol_width$}|", "", symbol_width = symbol_width));
            }

            for header in &string_headers {
                let mut production = String::new();
                if let Some(prods) = all_productions.get(header) {
                    if let Some(prod_str) = prods.get(row) {
                        production = prod_str.clone();
                    }
                }
                result.push_str(&format!("{:^column_width$}|", production, column_width = column_width));
            }
            result += "\n";
        }

        result.push_str(&"-".repeat(symbol_width + (column_width + 1) * string_headers.len()));
        result += "\n";
    }

    result
}

fn format_symbol(symbol: &Symbol) -> String {
    match symbol {
        Symbol::Terminal(c) => format!("'{}'", c),
        Symbol::NonTerminal(s) => format!("<{}>", s),
    }
}

pub fn format_grammar_table2(table: &HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>>) -> String {
    let mut result = String::new();

    for (symbol, inner_map) in table {
        result.push_str(&format!("Symbol: {}\n", format_symbol(symbol)));
        result.push_str("-----------------------------\n");

        for (key, productions) in inner_map {
            result.push_str(&format!("  Key: {}\n", key));
            result.push_str("  Productions:\n");

            for (i, production) in productions.iter().enumerate() {
                let production_str = production
                    .iter()
                    .map(format_symbol)
                    .collect::<Vec<_>>()
                    .join(" ");
                result.push_str(&format!("    {}: {}\n", i + 1, production_str));
            }
            result.push('\n');
        }
        result.push_str("============================================\n");
    }

    result
}

fn check_k (table: &HashMap<Symbol, HashMap<String, Vec<Vec<Symbol>>>>) -> bool {
    for (_, rules) in table {
        for (_, productions) in rules {
            if productions.len() > 1 {
                return false
            }
        }
    }

    true
}

fn main() -> Result<(),String> {
    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("can't read a k");

    let k: usize = match input.trim().parse() {
        Ok(num) => num,
        Err(_) => {
            panic!("can't parse a k");
        }
    };

    let mut file = File::open("grammar_description")
        .map_err(|e| format!("Failed to open input file grammar_description: {}", e))?;
    let mut grammar_text = String::new();
    file.read_to_string(&mut grammar_text)
        .map_err(|e| format!("Failed to read input file grammar_description: {}", e))?;

   /*let grammar_text = "S-> aB
S-> aC
S-> bE
S-> bD
B->bK
B->F
F->pp
C->bL
K->k
K->t
L->l
E->e
D->d";*/
    //let grammar_text = "S->SaS\n S->b \n S->a";
    //let grammar_text = "S->aA\n A->bB\n B->D\n D->p\n D->o";
    //let grammar_text = "S->aS";
    //let grammar_text = "S -> aS\n S -> aA\n S-> b\n A -> cA\n A -> d\n";
    // Example grammar: S -> aS | aA | b
    //                  A -> cA | d

    let grammar = parse_grammar(grammar_text.trim())?;
    let first_sets = calculate_first_sets(&grammar.clone());
    let first_sets_with_production = process_first_sets_with_productions(&grammar, &first_sets);

    let mut table = process_grammar_rules(grammar.clone(), &first_sets_with_production);
    if k>1 {
        table = process_grammar_rules_second_pass(&grammar, &table, &first_sets_with_production);
        if k>2 {
            table = process_grammar_rules_third_pass(&grammar, &table, &first_sets_with_production);
        }
    }

    let right_k = check_k(&table);
    if !right_k {
        println!("WARNING k is not enough");
    }

    let table_string = format_grammar_table(&table);
    println!("Beautified table:");
    println!("{}", table_string);
    println!("Full table:");
    let full_table = format_grammar_table2(&table);
    println!("{}", full_table);

    loop {
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("can't read word to parse");

        input = input.trim().to_string();

        if input == "#" {
            break;
        }

        match parse_word(input.trim(), &table) {
            true => println!("Belongs"),
            false => println!("Doesn't belong to the language"),
        }
    }


    Ok(())
}
