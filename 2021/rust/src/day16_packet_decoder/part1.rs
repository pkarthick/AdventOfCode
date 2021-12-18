use core::*;

struct PartOne {}
impl PartSpec for PartOne {
    fn get_day(&self) -> i32 {
        16
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::One
    }
}

fn get_binary_from_hexa(h: &str) -> String {
    let mut binary = String::new();

    for c in h.chars().into_iter() {
        let z = u64::from_str_radix(&c.to_string(), 16).unwrap();
        let segment = format!("{:04b}", z);

        binary.push_str(&segment);
    }

    let blen = binary.len();

    if blen % 8 != 0 {
        let q = blen / 8;
        let leading_zeroes_count = (q + 1) * 8 - blen;
        for _ in 0..leading_zeroes_count {
            binary.insert(0, '0');
        }
    }

    binary
}

fn get_decimal_from_binary(b: &str) -> usize {
    usize::from_str_radix(b, 2).unwrap()
}

fn get_literal(b: &str) -> (String, String) {
    let mut lit: String = String::new();

    let mut start = 6;

    loop {
        lit.push_str(&b[start + 1..start + 5]);
        

        if b.chars().into_iter().nth(start) == Some('0') {
            start += 5;
            break;
        }

        start += 5;

        if start == b.len() {
            break;
        }
        
    }

    (lit, b[start..].to_string())
}

fn decode_packet(b: &str, version: &mut usize) -> Option<String> {
    if b == "" || b.chars().all(|c| c == '0') {
        return None;
    }

    let v = &b[0..3];
    let t = &b[3..6];

    *version += get_decimal_from_binary(v);

    match t {
        "100" => {
            //Literal
            let (_, rem) = &get_literal(&b);

            let mut rem = rem.to_string();

            loop {
                if let Some(r) = decode_packet(&rem, version) {
                    rem = r;
                } else {
                    return None;
                }
            }
        }
        _ => {
            // Operator
            let i = &b[6..7];

            if i == "0" {
                // sub packets

                let length = get_decimal_from_binary(&b[7..22]);
                
                let end = 22 + length;
                let subpackets = b[22..end].to_string();

                decode_packet(&subpackets, version);

                let mut rem = b[end..].to_string();

                loop {
                    if let Some(r) = decode_packet(&rem, version) {
                        rem = r;
                    } else {
                        return None;
                    }
                }
            } else {
                let length = get_decimal_from_binary(&b[7..18]);

                let mut rem = b[18..].to_string();

                if length > 1 {
                    for _ in 0..length {
                        if let Some(r) = decode_packet(&rem, version) {
                            rem = r;
                        } else {
                            return None;
                        }
                    }
                    return None;
                } else {
                    loop {
                        if let Some(r) = decode_packet(&rem, version) {
                            rem = r;
                        } else {
                            return None;
                        }
                    }
                }
            }
        }
    }
}

impl TestPart for PartOne {
    fn process_input(&self, input: String) -> String {
        let binary = get_binary_from_hexa(input.lines().nth(0).unwrap());
        let mut version = 0_usize;
        decode_packet(&binary, &mut version);
        version.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let part = PartOne {};

        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
