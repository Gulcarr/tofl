use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io;
use std::io::{Error, ErrorKind, Read};

fn membership(request: &str,  membership_map: &mut HashMap<String, i8>) -> Result<i8, Error> {
    if membership_map.contains_key(request) {
        return Ok(membership_map[request]);
    }

    let mut response = String::new();

    println!("{}", request);
    io::stdin().read_line(&mut response)?;
    let res_num: i8 = response.trim().parse().map_err(|e| {
        Error::new(ErrorKind::InvalidInput, format!("Ошибка при преобразовании: {}", e))
    })?;


    membership_map.insert(String::from(request), res_num);

    Ok(res_num)
}

fn equivalence(main_table: &Vec<Vec<i8>>,
               suffixes: &Vec<String>,
               prefixes: &Vec<String>) -> Result<String, Error> {
    print!("        epsilon");
    for suffix in &suffixes[1..] {
        print!(" | {}", suffix);
    }
    println!("\n-----------------");
    for (i, row) in main_table.iter().enumerate() {
        print!("{}", if i == 0 { "epsilon" } else { &*prefixes[i] });

        for &value in row {
            print!(" | {}", value);
        }
        println!();
    }


    let mut response = String::new();
    io::stdin().read_line(&mut response)?;
    let formated_response = response.trim();
    Ok(formated_response.to_string())
}

fn build_membership_line(prefix: &String, suffixes: &Vec<String>,
                         membership_map: &mut HashMap<String, i8>) -> Result<Vec<i8>, Error> {
    let mut result = Vec::new();
    let mut response;

    for suffix in suffixes {
        response = membership(&format!("{}{}", prefix, suffix), membership_map)?;
        result.push(response);
    }

    Ok(result)
}

fn complete_tables(extend_from_ind: usize, main_table: &mut Vec<Vec<i8>>, extra_table: &mut Vec<Vec<i8>>,
                   pref_set: &mut HashSet<String>,
                   suf_set: &mut HashSet<String>, prefixes: &mut Vec<String>, extra_prefixes: &mut Vec<String>,
                   suffixes:&mut Vec<String>, lines_set: &mut HashSet<Vec<i8>>,
                   membership_map: &mut HashMap<String, i8>) -> Result<usize, Error> {
    let mut i = extend_from_ind;
        while (i < prefixes.len()) {
        let new_prefix_l = format!("{}L", prefixes[i]);
        let new_prefix_r = format!("{}R", prefixes[i]);
        let line_l = build_membership_line(&new_prefix_l, suffixes, membership_map)?;
        if lines_set.contains(&line_l) {
            extra_prefixes.push(new_prefix_l);
            extra_table.push(line_l);
        } else {
            prefixes.push(new_prefix_l.clone());
            main_table.push(line_l.clone());
            pref_set.insert(new_prefix_l);
            lines_set.insert(line_l);
        }

        let line_r = build_membership_line(&new_prefix_r, suffixes, membership_map)?;
        if lines_set.contains(&line_r) {
            extra_prefixes.push(new_prefix_r);
            extra_table.push(line_r);
        } else {
            prefixes.push(new_prefix_r.clone());
            main_table.push(line_r.clone());
            pref_set.insert(new_prefix_r);
            lines_set.insert(line_r);
        }

            i+=1;
    }

    Ok(prefixes.len())
}

fn process_example(extend_from_ind: usize, equivalence_answer: &str, main_table: &mut Vec<Vec<i8>>, extra_table: &mut Vec<Vec<i8>>,
                   pref_set: &mut HashSet<String>,
                   suf_set: &mut HashSet<String>, prefixes: &mut Vec<String>, extra_prefixes: &mut Vec<String>,
                   suffixes:&mut Vec<String>, lines_set: &mut HashSet<Vec<i8>>,
                   membership_map: &mut HashMap<String, i8>) -> Result<usize, Error> {
    for i in 0..equivalence_answer.len() {
        let suffix = &equivalence_answer[i..];
        if !suf_set.contains(suffix) {
            suf_set.insert(suffix.to_string());
            suffixes.push(suffix.to_string());
            lines_set.clear();

            for (j, row) in main_table.iter_mut().enumerate() {
                let line = format!("{}{}", prefixes[j], suffix);
                row.push(membership(&line, membership_map)?);
                lines_set.insert(row.clone());
            }

            for (j, row) in extra_table.iter_mut().enumerate() {
                let line = format!("{}{}", extra_prefixes[j], suffix);
                row.push(membership(&line, membership_map)?);
                if !lines_set.contains(row) {
                    lines_set.insert(row.clone());
                    pref_set.insert(extra_prefixes[j].clone());
                    prefixes.push(extra_prefixes[j].clone());
                    main_table.push(row.clone());
                    // TODO: delete line from extra_table
                }
            }
        }
    }

    complete_tables(extend_from_ind, main_table, extra_table, pref_set, suf_set, prefixes,
                    extra_prefixes, suffixes, lines_set, membership_map)
}

fn main() -> std::io::Result<()> {
    let mut file = File::open("parameters.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let mut membership_answer: i8;

    let mut membership_map = HashMap::new();
    let mut main_table = Vec::new();
    let mut extra_table = Vec::new();
    let mut prefixes = Vec::new();
    let mut extra_prefixes = Vec::new();
    let mut suffixes = Vec::new();
    let mut pref_set = HashSet::new();
    let mut suf_set = HashSet::new();
    let mut lines_set = HashSet::new();
    let mut extend_from_ind = 0;

    prefixes.push("".to_string());
    pref_set.insert("".to_string());
    suffixes.push("".to_string().to_string());
    suf_set.insert("".to_string());
    membership_answer = membership("epsilon", &mut membership_map)?;
    main_table.push(vec![membership_answer]);
    lines_set.insert(main_table[0].clone());
    extend_from_ind = complete_tables(extend_from_ind, &mut main_table, &mut extra_table, &mut pref_set,
                                      &mut suf_set, &mut prefixes, &mut extra_prefixes,
                    &mut suffixes, &mut lines_set, &mut membership_map)?;

    loop {
        let equivalence_answer = &*equivalence(&main_table, &suffixes, &prefixes)?;
        if equivalence_answer == "TRUE" { break }
        extend_from_ind = process_example(extend_from_ind, equivalence_answer, &mut main_table, &mut extra_table, &mut pref_set,
                        &mut suf_set, &mut prefixes, &mut extra_prefixes,
                        &mut suffixes, &mut lines_set, &mut membership_map)?;
    }

    Ok(())
}
