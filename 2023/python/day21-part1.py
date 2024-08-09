f = open("../input/day21", "r")
ls = f.read().splitlines()
f.close()

indices = set([])

for (r, l) in enumerate(ls):
    for (c, symbol) in enumerate(l):
        if symbol == 'S':
           indices.add((r,c)) 
           break

all_indices = []

for i in range(64):
    
    new_indices = indices
    indices = set([])
    
    for (r,c) in new_indices:
        
        locs = [(r+1, c), (r-1, c), (r,c+1), (r, c-1)]
                
        for (rr,cc) in locs:
            
            if rr >= 0 and rr < len(ls) and cc >= 0 and cc < len(ls[0]) and ls[rr][cc] == '.':
                indices.add((rr, cc))
        
                    

print(len(set(indices)) + 1)
    
            
