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
    
    
    def find_point_of_incidence(pattern, allowed_count) -> tuple[int, tuple[int, int, int]]:

        def find_smudge_position(r) -> tuple[int, int, int]:
            
            count = 0
            pos = (-1, -1, -1)
                        
            for (ar, br) in zip(range(r-1,-1,-1), range(r, len(pattern))):
                
                for c in range(len(pattern[0])):
                    if pattern[ar][c] != pattern[br][c]:
                        count+=1
                        
                        if count > allowed_count:
                            return (-1, -1, -1)
                        
                        pos = (ar, br, c)
                        
                        
            return pos
                    

        row_of_incidence = 0
        smudge_pos = (-1, -1, -1)
        
            
        for r in range(1, len(pattern)):
            
            pos = find_smudge_position(r)
            
            if pos != (-1, -1, -1):
                smudge_pos = pos
                row_of_incidence = r
           
                
        return (row_of_incidence, smudge_pos)
        
    
    def find_original_point_of_incidence(pattern) -> tuple[int, tuple[int, int]]:
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
        
        v_org_at = find_original_point_of_incidence(pattern_lines)
        
        (v_at, (t,b,c)) = find_point_of_incidence(pattern_lines, 1)
        
        if v_at != 0 and v_at != v_org_at:
            total += v_at * 100

        
            

        if v_at != 0:        
       
            pattern_lines[t] = pattern_lines[t][:c] + ('#' if pattern_lines[t][c] == '.' else '.') + pattern_lines[t][c+1:]
            transposed_lines = [[l[j] for l in pattern_lines] for j in range(len(pattern_lines[0]))]
            (h_at1, _) = find_point_of_incidence(transposed_lines, 0)
            total += h_at1

            # if h_at1 == 0:
            pattern_lines = pattern.split('\n')
            pattern_lines[b] = pattern_lines[b][:c] + ('#' if pattern_lines[b][c] == '.' else '.') + pattern_lines[b][c+1:]
        
            transposed_lines = [[l[j] for l in pattern_lines] for j in range(len(pattern_lines[0]))]
            (h_at2, _) = find_point_of_incidence(transposed_lines, 0)
            total += h_at2
                
        else:
            transposed_lines = [[l[j] for l in pattern_lines] for j in range(len(pattern_lines[0]))]
            (h_at, _) = find_point_of_incidence(transposed_lines, 1 if (t,b,c) == (-1,-1,-1) else 0)
            total += h_at    
    
        
                
    print("Part 2 Answer: ", total)
    
# count("../input/day13s")
count("../input/day13")

