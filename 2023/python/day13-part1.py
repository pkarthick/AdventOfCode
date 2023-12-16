import itertools

def count(filename: str): 

    f = open(filename, "r")
    lines = f.read()
    f.close()

    patterns = lines.split('\n\n')
        
    def is_matching(ind, pattern) -> bool:
        top = ind - 1
        bottom = ind
        count = 0
        rocks_count = 0
        
        for i in range(ind-1, -1, -1):
            if pattern[top] == pattern[bottom]:
                count += 1
                top -= 1
                bottom += 1
                if top == -1 or bottom == len(pattern):
                    return True
            else:
                return False
        
        return False
    
    
    def vertical_split_at(pattern) -> int :
        gs = list(map(lambda g: len(list(g[1])), itertools.groupby(pattern)))
        
        ind = 0
        
        for (i, count) in enumerate(gs):
            ind += count
            if count == 2 and is_matching(ind-1, pattern): 
                return ind - 1 
        
        return 0


    total = 0
    
    for (i, pattern) in enumerate(patterns):
        pattern_lines = pattern.split('\n')
        v_at = vertical_split_at(pattern_lines)
        
        transposed_lines = [[l[j] for l in pattern_lines] for j in range(len(pattern_lines[0]))]
        h_at = vertical_split_at(transposed_lines)
        
        total += h_at
        total += v_at * 100
                
    print("Part 1 Answer: ", total)
  
count("../input/day13")


