import strutils
import tables

let input="""ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"""

proc checkPassport(passport: Table[string, string]): bool =

    let requiredKeys = @["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

    for key in requiredKeys:
        if not passport.hasKey(key) or passport[key].len == 0:
            return false
    
    return true

proc passportInfo(lines: seq[string]): int =

    var passport = initTable[string, string]()

    for line in lines:
        if line.len > 0:
            for seg in line.split():
                let kv = seg.split({':'})
                passport[kv[0]] = kv[1]
        else:

            if checkPassport(passport):
                inc result

            passport.clear()

    if checkPassport(passport):
        inc result

echo passportInfo (input.splitLines)


