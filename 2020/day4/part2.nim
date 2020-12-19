import strutils
import sequtils
import tables

let input="""eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007

pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"""

proc checkPassport(passport: Table[string, string]): bool =

    let requiredKeys = @["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

    for key in requiredKeys:
        if not passport.hasKey(key) or passport[key].len == 0:
            return false
        else:
            let val = passport[key]
            case key
            of "ecl":
                if not ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(val) : #6 valid hex
                    echo key, ":", val #done
                    return false
            of "hcl":
                if val.len != 7 or val[0] != '#' or val[1..^1].anyIt(it notin '0'..'9' and it notin 'a'..'f'):
                    echo key, ":", val #done
                    return false
            of "hgt":
                if val.endsWith("cm"):
                    try:
                        let h = val[0..^3].parseInt
                        if h notin 150 .. 193:
                            echo key, ":", val 
                            return false
                    except:
                        return false
                elif val.endsWith("in"):
                    try:
                        let h = val[0..^3].parseInt
                        if h notin 59 .. 76:
                            echo key, ":", val 
                            return false
                    except:
                        return false
                else:
                    echo key, ":", val 
                    return false

            of "byr":
                if val.len != 4:
                    echo key, ":", val 
                    return false
                try:
                    let byr = val.parseInt
                    if byr notin 1920 .. 2002:
                        echo key, ":", val 
                        return false
                    
                except:
                    return false
            of "iyr":
                if val.len != 4:
                    echo key, ":", val 
                    return false
                try:
                    let iyr = val.parseInt
                    if iyr notin 2010 .. 2020:
                        echo key, ":", val 
                        return false
                    
                except:
                    return false
            of "eyr":
                if val.len != 4:
                    echo key, ":", val 
                    return false
                try:
                    let eyr = val.parseInt
                    if eyr notin 2020 .. 2030:
                        echo key, ":", val 
                        return false
                except:
                    return false
            of "pid":
                if val.len != 9:
                    echo key, ":", val 
                    return false
                try:
                    discard val.parseInt
                    if val.len != 9:
                        echo key, ":", val 
                        return false
                
                except:
                    return false
            else: discard

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


