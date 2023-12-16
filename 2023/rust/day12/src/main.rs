use std::collections::HashSet;

fn is_valid(s: &str, vec: &[usize]) -> bool {
    let mut broken_count = 0_usize;
    let mut group_index = 0_usize;

    let mut i = 0_usize;

    let chars: Vec<char> = s.chars().collect();

    while chars[i] == '.' {
        i += 1;
    }

    while i < chars.len() {
        

        if i == chars.len() {
            return true;
        }

        if chars[i] == '?' {
            return false;
        }

        let ch = chars[i];

        if ch == '#' {
            broken_count += 1;
            i += 1;
        } else if ch == '.' {
            if group_index < vec.len() && vec[group_index] == broken_count {
                broken_count = 0;
                group_index += 1;
                while i < chars.len() && chars[i] == '.' {
                    i += 1;
                }
            } else {
                return false;
            }
        } else {
            return false;
        }

        
    }

    if group_index < vec.len() {
        vec[group_index] == broken_count
    } else {
        true
    }
}

fn work_out(s: String, vec: &[usize], index: usize, set: &mut HashSet<String>) -> bool {
    if index < vec.len() {
        if let Some(_) = s.find('?') {
            let s1 = s.clone();

            // s.replace_range(qi..qi+1, "#");
            let hash = s.replacen("?", "#", vec[index]);
            let ok = work_out(hash, vec, index + 1, set);

            let dot = s1.replacen("?", ".", 1);
            ok || work_out(dot, vec, index, set)

        } else {
            if is_valid(&s, vec) {
                set.insert(s);
                true
            } else {
                false
            }
        }
    } else {
        if is_valid(&s, vec) {
            set.insert(s);
            true
        } else {
            let s = s.replace("?", ".");
            if is_valid(&s, vec) {
                set.insert(s);
                true
            } else {
                false
            }
        }
    }
}

fn count_arrangements(s: String) -> usize {
    let mut frags = s.split(' ');
    let springs = frags.next().unwrap();
    let groups: Vec<usize> = frags
        .next()
        .unwrap()
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .collect();

    // let gs: Vec<String> = springs
    //     .split('.')
    //     .map(|s| s.to_string())
    //     .filter(|s| !s.is_empty())
    //     .collect();

    // println!("{gs:?}");

    split_into_size(springs.to_string(), groups)
}

fn find_count(gs: Vec<String>, groups: Vec<usize>) -> usize {
    if gs.len() == groups.len() {
        let total = gs
            .into_iter()
            .zip(groups)
            .map(|(springs, size)| {
                let broken_count = count_broken(&springs);
                let unknown_count = count_unknown(&springs);
                if broken_count == size {
                    0
                } else if unknown_count == size || unknown_count == 1 {
                    0
                } else {
                    split_into_size(springs, vec![size])
                }
            })
            .sum();
        println!();
        if total == 0 {
            1
        } else {
            total
        }
    } else {
        if gs.len() == 1 {
            split_into_size(gs[0].clone(), groups)
        } else {
            let mut size = 0;

            for ((springs, s), i) in gs.iter().zip(groups.clone()).rev().zip(1_usize..) {
                if count_unknown(&springs) == s {
                    continue;
                } else {
                    let gs = gs[0..gs.len() - i].to_vec();
                    let groups = groups[0..groups.len() - i].to_vec();

                    if gs.len() == 1 {
                        size = split_into_size(gs[0].clone(), groups.clone());
                        println!("split -> {gs:?} {groups:?} size={size}");
                    } else {
                        panic!("split -> {gs:?} {groups:?}");
                    }

                    break;
                }
            }

            size
        }
    }
}

fn split_into_size(gs: String, groups: Vec<usize>) -> usize {
    let mut set = HashSet::new();
    work_out(gs.clone(), &groups, 0, &mut set);

    println!("splitted: {gs:?} {groups:?} {set:?}");

    // if set.len() == 1 {
    //     0
    // } else {
    set.len()
    // }
}

fn count_broken(springs: &str) -> usize {
    springs.chars().filter(|ch| *ch == '#').count()
}

fn count_unknown(springs: &str) -> usize {
    springs.chars().filter(|ch| *ch == '?').count()
}

// fn by_pattern(s: &str, size: usize) -> usize {
//     match (count_unknown(s), size) {
//         (2, 1) => 2,
//         (2, 2) => 2,
//         (x, y) if x == y => x,
//         _ => split_into_size(s.to_string(), ),
//     }
// }

// fn find_arrangements(s: String, size: usize) -> usize {
//     let count = by_pattern(&s, size);

//     println!("find: {s} {size} = {count}");
//     count
// }

fn main() {
    let s = "???.### 1,1,3
    .??..??...?##. 1,1,3
    ?#?#?#?#?#?#?#? 1,3,1,6
    ????.#...#... 4,1,1
    ????.######..#####. 1,6,5
    ?###???????? 3,2,1";

    // let mut set = HashSet::new();

    // work_out(String::from("???"), &vec![1, 1], 0, &mut set);

    // println!("{set:?}")

    let total: Vec<usize> = s
        .split('\n')
        .map(|s| count_arrangements(s.trim().into()))
        .collect();

    println!("{total:?}");

    println!("{:?}", (total.into_iter().sum::<usize>()));
}
