from dataclasses import dataclass, field
from typing import List, Set


arr = [
".###.##.#...",
".###.##..#..",
".###.##...#.",
".###.##....#",
".###..##.#..",
".###..##..#.",
".###..##...#",
".###...##.#.",
".###...##..#",
".###....##.#",
]

# for s in arr:
#     print(is_valid(s, [3,2,1]))


s = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"""

lines = s.splitlines()

@dataclass
class SizeGroup:
    size: int = 0
    resolved: bool = False
    
    

@dataclass
class Group:
    items: List[str]
    resolved: bool = False
    is_spring_group: bool = False
    unknown_count: int = 0
    total_count: int = 0
    arrangement_count: int = 0
    
    def __post_init__(self):
        self.is_spring_group = self.items[0] == '#' or self.items[0] == '?'
        self.resolved = not self.is_spring_group
        self.unknown_count = len([x for x in self.items if x == '?'])
        self.total_count = len(self.items)
            
    
    def all_hash(self):
        for c in self.items:
            if c != "#" or c != "?":
                return False

        return True

def is_valid(s, sizes):
    broken_count = 0
    gi = 0
    
    i = 0
    
    while s[i] == '.':
        i += 1
        
    while i < len(s):
        
        if s[i] == '?':
            return False
        
        if s[i] == '#':
            broken_count += 1
            i += 1
        elif s[i] == '.':
            if gi < len(sizes) and sizes[gi] == broken_count:
                broken_count = 0
                gi += 1
                
                while i < len(s) and s[i] == '.':
                    i += 1
                    
            else:
                return False
            
        else:
            return False
        
    return sizes[gi] == broken_count if gi < len(sizes) else True

def arrange(s: str, sizes, index, set: Set[str]):
    
    if index < len(sizes):
        if s.find('?') != -1:
            hash = s.replace("?", "#", sizes[index])
            ok = arrange(hash, sizes, index+1, set)
            
            if not ok:
                dot = s.replace("?", ".", 1)
                arrange(dot, sizes, index, set)
            else:
                True
            
                
        else:
            if is_valid(s, sizes):
                set.add(s)
                True
            else:
                False
    
    else:
        if is_valid(s, sizes):
                set.add(s)
                True
        else:
            s = s.replace("?", ".")
            if is_valid(s, sizes):
                set.add(s)
                True
            else:
                False
    
    True


def create_groups(pattern) -> List[Group]:
    
    groups = []
    items = []

    last = None

    for c in pattern:
        if c == ".":
            if last != ".":
                last = "."
                if len(items) > 0:
                    groups.append(Group(items))
                    items = []

            items.append(c)

        else:
            if last == ".":
                last = c
                if len(items) > 0:
                    groups.append(Group(items))
                    items = []

            items.append(c)

    if len(items) > 0:
        groups.append(Group(items))
    
    return groups
    

@dataclass
class Record:
    line: str
    pattern: str = None
    sizes: List[SizeGroup] = field(default_factory=list)
    groups: List[Group] = field(default_factory=list)
    spring_groups: List[Group] = field(default_factory=list)
    arrangement_count: int = 1
    resolved: bool = False

    def __post_init__(self):
        [pattern, sizes] = self.line.split()
        self.sizes = [SizeGroup(int(s)) for s in sizes.split(",")]
        self.pattern = pattern
        self.groups = create_groups(self.pattern)
        self.spring_groups = list(filter(lambda g: g.is_spring_group, self.groups))
                
    def check_unresolved(self):
        
        total_len = 0
        unknown_len = 0
        prefix_group_count = len(self.spring_groups)
        
        for i in range(len(self.spring_groups)):
            
            sg = self.spring_groups[i]
                      
            if sg.resolved: 
                prefix_group_count = i
                break
            
            total_len += sg.total_count
            unknown_len += sg.unknown_count

        size_len = 0
        size_count = 0
        
        prefix_size_count = len(self.sizes)
        
        for i in range(len(self.sizes)):
            
            size = self.sizes[i]
            
            if size.resolved: 
                prefix_size_count = i
                break
            
            size_len += size.size
            size_count += 1
        
        
        dot_count = size_count - 1
                
        print("pat:", self.pattern)
        print("total_len:", total_len)
        print("unknown_len:", unknown_len)
        print("size_len:", size_len)
        print("size_count:", size_count)
        print("dot_count:", dot_count)
        
        
        if total_len - dot_count == size_len:
            if total_len == unknown_len:
                for i in range(prefix_group_count):
                    self.spring_groups[i].resolved = True
                    self.spring_groups[i].arrangement_count = 1
                    
                for i in range(prefix_size_count):
                    self.sizes[i].resolved = True
            
            else:
               
                pass
        else:
            
            pass
        
        if all(map(lambda sg: sg.resolved, self.spring_groups)):
            
            product = 1
            for sg in self.spring_groups:
                product *= sg.arrangement_count
                
            self.arrangement_count = product
            self.resolved = True
        else:
            self.find_fixed_points()
                    
    def find_fixed_points(self):
        
        pat = self.pattern[:]
        sizes = [sz.size for sz in self.sizes]
    
        for size in sizes:
            pat = str(pat).replace("?", "#", size)
            if is_valid(pat, sizes):
                print("pass")
                pass
            else:
                pat = str(self.pattern).replace("?", ".", size)
                if is_valid(pat, sizes):
                    print("pass")
                    pass
        
        
    def check_leading_groups(self, pat: str) -> tuple[bool, int, int]:
        groups = create_groups(pat)
        
        gi = 0
        
        for (si, sz) in enumerate(self.sizes):
            while not groups[gi].is_spring_group: 
                gi+=1
            
            if sz.size != groups[gi].total_count:
                return (False, 0, 0)
            else:
                gi += 1
            
        return (True, gi, si)
        
    def workout(self):
        
        def check(forward):
            
            rng = range(len(self.sizes)) if forward else range(len(self.sizes)-1, -1, -1)
                
            if not self.resolved:

                if len(self.spring_groups) == len(self.sizes):
                    
                    for i in rng:
                        sg = self.spring_groups[i]
                        size = self.sizes[i].size
                                            
                        if sg.resolved:
                            continue

                        if sg.total_count == size:
                            sg.resolved = True
                            sg.arrangement_count = 1
                            self.sizes[i].resolved = True
                            
                        elif sg.total_count > size:
                            sg.arrangement_count = compute_combinations(sg.total_count, size)
                            sg.resolved = True
                            self.sizes[i].resolved = True
                        else:
                            break
                    else:
                        if all(map(lambda sg: sg.resolved, self.spring_groups)):
                            
                            product = 1
                            for sg in self.spring_groups:
                                product *= sg.arrangement_count
                                
                            self.arrangement_count = product
                            self.resolved = True
                            
                elif len(self.spring_groups) <= len(self.sizes):
                    
                    (gi, gstep) = (0, 1) if forward else (len(self.spring_groups) - 1, -1)
                    
                    for i in rng:
                        
                        if gi >= 0 and gi < len(self.spring_groups):
                            
                            sg = self.spring_groups[gi]
                            size = self.sizes[i].size
                            
                            gi += gstep
                                                
                            if sg.resolved:
                                continue

                            if sg.total_count == size:
                                self.sizes[i].resolved = True
                                sg.resolved = True
                                sg.arrangement_count = 1

                else: 
                    raise Exception("Spring groups cannot be less than the sizes!")

        check(True)
        check(False)
        
        self.check_unresolved()

    def is_valid(self):
        is_valid(self.pattern,self.sizes)


def compute_combinations(n, size):
    if size == 1:
        return n

    else:
        product = 1

        for _ in range(1, size):
            product *= n
            n -= 1

        return product

def new_arrangement(s: str, sizes: List[int]):
    
    def trim_prefix(s):
        
        i = 0
        
        while s[i] == '.':
            i += 1
        
        si = 0
        
        for j in range(i, len(s)):
            if s[j] != '.':
                return s[j:]
            if s[j] == '.':
                if j - i == sizes[si]:
                    si += 1
                elif j - i > sizes[si]:
                    print("combinations for ", s[i:j], " in ", sizes[si])
                    si += 1
                else:
                    raise "unexpected!"
                
        return ""
            
    
    def trim_suffix(s):
        
        i = len(s) - 1
        
        while s[i] == '.':
            i -= 1
        
        si = len(sizes) - 1
        
        j = i
        
        while j >= 0:
            if s[j] == '?':
                return s[0:j+1]
            if s[j] == '.':
                if i - j == sizes[si]:
                    si -= 1
                    j -= 1
                    while j >= 0 and s[j] == '.':
                        j -= 1
                        
                    continue
                    
                elif i - j > sizes[si]:
                    print("combinations for ", s[j:i], " in ", sizes[si])
                    si -= 1
                else:
                    raise "unexpected!"
                
            j -= 1
                
        return ""
    
    def take_unknown_or_hash(count: int, starting: int) -> int:
        
        i = 0
        c = 0
        
        while c < count:
            
            if s[i] == '?' or s[i] == '#':
                c += 1
            
            i += 1
            
        return i
    
    no_prefix = trim_prefix(s)
    no_prefix_suffix = trim_suffix(no_prefix)
    
    print(no_prefix_suffix)
    
    return no_prefix_suffix
        
    # return s[0 : take_unknown_or_hash(sizes[0], 0)+1]
            


def check_groups(s, group_sizes, group_count):
    
    broken_count = 0
    gi = 0
    
    i = 0
    
    while s[i] == '.':
        i += 1
        
    while i < len(s):
        
        if s[i] == '#':
            broken_count += 1
            i += 1
        else: # it is a dot or question
            if gi < group_count and group_sizes[gi] == broken_count:
                broken_count = 0
                gi += 1
                
                if gi == group_count:
                    return True
                
                while i < len(s) and s[i] == '.':
                    i += 1
                    
            else:
                return False
            
    return group_sizes, sizes[gi] == broken_count if gi < len(group_sizes, sizes) else True

hash_and_dot = 2 
one_for_hash = 1

def number_of_arrangements(s: str, group_sizes: List[int], gi: int, start: int, running_hash_count: int, count: int) -> int:

    expected_hash_count = sum(group_sizes)
    expected_dot_count_min = len(group_sizes)-1
    
    hash_count = len([x for x in s if x == '#'])
    question_count = len([x for x in s if x == '?'])
    dot_count = len([x for x in s if x == '.'])
    
    missing_hashes = expected_hash_count - hash_count
    
    
    if dot_count == expected_dot_count_min and question_count == expected_hash_count:
        return 1
    
    elif dot_count > expected_dot_count_min:
        
        expected_hash_count - hash_count
        
        #groups can be split by dots
        pass
    else:
        missing_dots =  question_count - missing_hashes
        
        # ? should be replaced with dots
        number_of_dots_to_replace = expected_dot_count_min - dot_count
    
        # some questions are hashes
        missing_hashes = expected_hash_count - hash_count
        missing_dots = question_count - missing_hashes
        
        if question_count == missing_dots + missing_hashes:
            return 1
        else:
            pass
        

    if start == len(pattern) or len(group_sizes) == gi:
        if check_groups(s, len(group_sizes)):
            return count
        else:
            return -1
        
    if s[start] == '#':
        
        running_hash_count += one_for_hash
        
        if running_hash_count > group_sizes[gi]:
            return -1
        elif running_hash_count == group_sizes[gi]:
            pass
        
        return number_of_arrangements(s_dot, group_sizes, gi, start+one_for_hash, running_hash_count+one_for_hash, count)

    elif s[start] == '?':
        pass
        
    else: # a dot
        pass
        

    s_hash = s.replace("?", "#", 1)
        
    if check_groups(s_hash, group_sizes, gi+1):
        
        if start+group_sizes[gi]+1 <= len(pattern): 
            if s[start+group_sizes[gi]+1] == '?':
                s_dot = s.replace("?", ".", 1)
                
                dot_count = number_of_arrangements(s_dot, group_sizes, gi+1, start+hash_and_dot, running_hash_count+one_for_hash, count+one_for_hash)
                return dot_count
                
            else:
                pass
   
    s_dot = s.replace("?", ".", 1)
    
    dot_count = number_of_arrangements(s_dot, group_sizes, gi, start+1, running_hash_count, count)
    return dot_count            
    
          
    
count = 0

for lin in lines:
    [pattern, sizes] = lin.split()
    sizes = [int(s) for s in sizes.split(",")]
     
    num = number_of_arrangements(pattern, sizes, 0, 0, 0)
    count += num
    
    print(lin, num, count)
     
    
print(count)