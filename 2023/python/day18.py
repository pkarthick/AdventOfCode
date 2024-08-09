import itertools


def create_instruction(line: str):
    [dir, size, color] = line.split()
    size = int(size)
    return (dir, size)

def is_opposite(dir1, dir2):
    match (dir1, dir2):
        case ('L', 'R') | ('R', 'L'):
            return True
        case ('U', 'D') | ('D', 'U'):
            return True
        case _:
            return False
        
def get_opposite(dir):
    match dir:
        case 'L':
            return 'R'
        case 'R':
            return 'L'
        case 'U':
            return 'D'
        case 'D':
            return 'U'
    
   
        # case [('R', rs), ('D', ds), ('L', ls), ('U', us)]: 
        #     return (rs+1) * (ds+1) + count
        
        # case [i, (d1, s1),  (d2, s2), *rest] if d1 == d2:
        #     return simplify([i, (d1, s1+s2), *rest], prefix, count)
            
        # case [(d1, s1),  (d2, s2), *rest] if d1 == d2:
        #     return simplify([(d1, s1+s2), *rest], prefix, count)
        
        # case [(d1, s1),  (d2, s2), *rest] if is_opposite(d1, d2):
        #     return simplify([(d1, s1-s2), *rest], prefix, count)
        
        # case  [('D', s1), ('L' as d, s), ('D', s2), ('R', s3), *rest] if s == s3:
        #     return simplify([(d1, s1+s2), *rest], prefix, count - (s2-1)  * s3)
        
        # case  [('U', s1), ('R' as d, s), ('U', s2), ('L', s3), *rest]:
        #     return simplify([(d1, s1+s2), *rest], prefix, count - s  * s3)
        
        # case  [('D', s1), ('L' as d, s), ('U', s2), ('L', s3), *rest]:
        #     return simplify([('D', s1), ('L', s+s3), ('U', s2), *rest], prefix, count - s2  * s3)
        
        # case [('D', s1), ('R' as d, s), ('D', s2), *rest] | [('D', s1), ('L' as d, s), ('D', s2), *rest] | [('L', s1), ('D' as d, s), ('L', s2), *rest] | [('R', s1), ('U' as d, s), ('R', s2), *rest]:
        #     return simplify([(d1, s1+s2), *rest], prefix, count + (s-1)  * s2)
        
        # case [('R', s1), ('D' as d, s), ('R', s2), *rest] | [('L', s1), ('D' as d, s), ('L', s2), *rest] | [('U', s1), ('L' as d, s), ('U', s2), *rest] | [('R', s1), ('U' as d, s), ('R', s2), *rest] : 
        #         return simplify([(d1, s1+s2), *rest], prefix, count - (s - 1) * s1)
                
            
        # # case [('U', s1), ('R' as d, s), ('U', s2), *rest] : 
        # #     if s1 <= s2:
        # #         return simplify([(d1, s1+s2), *rest], prefix, count + (s - 1) * s1)
        # #     else:
        # #         return simplify([(d1, s1+s2)], prefix, count + (s-1) * s2)
        
        # # case [('U', s1), ('L' as d, s), ('U', s2), *rest] : 
        # #     pass
                        
        # case [('L', s1), ('U' as d, s), ('L', s2), *rest] : 
        #     if s1 <= s2:
        #         return simplify([('L', s1+s2), ('U', s), *rest], prefix, count - s * s2)
        #     else:
        #         return simplify([(d1, s1+s2), (d, s), *rest], prefix, count - s * s2)
            
        # case [('D', s1), ('R' as d, s), ('U', s2), *rest] | [('L', s1), ('D' as d, s), ('R', s2), *rest] | [('U', s1), ('R' as d, s), ('D', s2), *rest] | [('R', s1), ('U' as d, s), ('L', s2), *rest] if s1 == s2 : 
        #     return simplify([(d, s), *rest], prefix, count - (s - 1) * s1)


def simplify(ins: list[tuple[str, int]], prefix: list[tuple[str, int]], count: int):
    
    match ins:
        case [('R', rs), ('D', ds), ('L', ls), ('U', us)]: 
            return (rs+1) * (ds+1) + count
        
        case [i, (d1, s1),  (d2, s2), *rest] if d1 == d2:
            return simplify([i, (d1, s1+s2), *rest], prefix, count)
        
        case [i, i2, (d1, s1),  (d2, s2), *rest] if d1 == d2:
            return simplify([i, i2, (d1, s1+s2), *rest], prefix, count)
        
        case [(d1, s1), (d2, s2), (d3, s3), (d4, s4), *rest] if d1 == d3 and d2 == d4 and len(rest) >= 2 and ((d1 == 'L' and d2 == 'U') or (d1 == 'U' and d2 == 'R') or (d1 == 'R' and d2 == 'D') or (d1 == 'D' and d2 == 'R')): 
            return simplify([(d1, s1+s3), (d2, s2+s4),  *rest], prefix, count - s2 * s4)
        
        case [(d1, s1), (d2, s2), (d3, s3), (d4, s4), *rest] if d1 == d3 and d2 == d4 and len(rest) >= 2 and ((d1 == 'L' and d2 == 'U') or (d1 == 'U' and d2 == 'R') or (d1 == 'R' and d2 == 'D') or (d1 == 'D' and d2 == 'R')): 
            return simplify([(d1, s1+s3), (d2, s2+s4),  *rest], prefix, count + s2 * s4)

        
        # case [('U', u1), ('R', r1), ('U', u2), ('R', r2), *rest]: 
        #     return simplify([('U', u1+u2), ('R', r1+r2),  *rest], prefix, count - r1 * u2)
                
        case [('R', r1), ('U', u), ('R', r2), ('D', d), *rest] if d == u: 
            return simplify([('R', r1+r2), *rest], prefix, count - r2 * u)
        
        case [('R', r1), ('D', d), ('R', r2), ('U', u), *rest] if d == u: 
            return simplify([('R', r1+r2), *rest], prefix, count - r2 * u)
     
        case [('D', d1), ('R', r), ('D', d2), ('L', l), *rest] if l == r: 
            return simplify([('D', d1+d2), *rest], prefix, count - d2 * r)
     
        case [('D', d1), ('L', l), ('D', d2), ('R', r), *rest] if l == r: 
            return simplify([('D', d1+d2), *rest], prefix, count - d2 * r)
     
        case [('L', l1), ('D', d), ('L', l2), ('U', u), *rest] if u == d: 
            return simplify([('L', l1+l2), *rest], prefix, count - l2 * d)
     
        case [('L', l1), ('U', u), ('L', l2), ('D', d), *rest] if u == d: 
            return simplify([('L', l1+l2), *rest], prefix, count - l2 * d)
        
        case [('U', u1), ('R', r), ('U', u2), ('L', l), *rest] if r == l: 
            return simplify([('U', u1+u2), *rest], prefix, count - u2 * l)
        
        case [('U', u1), ('L', l), ('U', u2), ('R', r), *rest] if r == l: 
            return simplify([('U', u1+u2), *rest], prefix, count - u2 * l)
                
        case [i, *rest]:
            return simplify(rest, [*prefix, i], count)
        case []:
            return simplify(prefix, [], count)

def simplifyReverse(ins: list[tuple[str, int]], prefix: list[tuple[str, int]], count: int):
    
    match ins:
        
        case [('L', ls), ('D', us), ('R', rs), ('U', ds)]: 
            return (rs+1) * (ds+1) + count
        
        case [i, (d1, s1),  (d2, s2), *rest] if d1 == d2:
            return simplifyReverse([i, (d1, s1+s2), *rest], prefix, count)
            
        case [(d1, s1),  (d2, s2), *rest] if d1 == d2:
            return simplifyReverse([(d1, s1+s2), *rest], prefix, count)
        
        case [('L', s1), ('U' as d, s), ('L', s2), *rest] | [('R', s1), ('U' as d, s), ('R', s2), *rest] | [('D', s1), ('R' as d, s), ('D', s2), *rest] | [('L', s1), ('D' as d, s), ('L', s2), *rest] : 
            if s1 <= s2:
                return simplifyReverse([(d1, s1+s2), *rest], prefix, count - (s - 1) * s1)
            else:
                return simplifyReverse([(d1, s1+s2), (d, s), *rest], prefix, count - (s  * s2))
            
        case [('R', s1), ('D' as d, s), ('R', s2), *rest] | [('D', s1), ('R' as d, s), ('D', s2), *rest] : 
            if s1 <= s2:
                return simplifyReverse([(d1, s1+s2), *rest], prefix, count - (s - 1) * s1)
            else:
                return simplifyReverse([(d1, s1+s2), (d, s), *rest], prefix, count - s * s2)
            
        case [('U', s1), ('L' as d, s), ('D', s2), *rest] | [('R', s1), ('U' as d, s), ('L', s2), *rest] | [('D', s1), ('L' as d, s), ('U', s2), *rest] | [('L', s1), ('D' as d, s), ('R', s2), *rest] if s1 == s2 : 
            return simplifyReverse([(d, s), *rest], prefix, count - (s - 1) * s1)

        case [i, *rest]:
            return simplifyReverse(rest, [*prefix, i], count)
        case []:
            return simplifyReverse(prefix, [], count)
        
def count(filename: str): 

    f = open(filename, "r")
    lines = f.read().splitlines()
    f.close()

    instructions = list(map(create_instruction, lines))
    
    # if instructions[0][0] == 'L' and instructions[-1][0] == 'U':
    #     total = simplify(instructions, [], 0)
    #     print("Part 1 Answer: ", total)
    # else:
    
    total = simplify(instructions, [], 0)
    print("Part 1 Answer: ", total)

count("../input/day18")


