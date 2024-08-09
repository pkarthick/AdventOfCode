f = open("../input/day21s", "r")
ls = [[ch for ch in l] for l in f.read().splitlines()]
f.close()
from collections import Counter

new_items = {}

cache = {}
grids = {}

h = len(ls)
w = len(ls[0])

for r in range(11):
    for c in range(11):
       
        if ls[r][c] == 'S':

            ls[r][c] = '.'
            new_items[(r,c)] = set([(0,0)])
            break
            
for r in range(11):
    for c in range(11):
        
        if ls[r][c] == '.':
            
            cache[(r,c)] = {}
            
            for (rr,cc) in [(-1, 0), (1, 0), (0,1), (0, -1)]:
                
                ro = 0
                co = 0
                
                r1 = r + rr
                c1 = c + cc
                
                if r1>= h:
                    ro = 1
                elif r1 < 0:
                    ro = -1
                elif c1 < 0:
                    co = -1
                elif c1 >= w:
                    co = 1
                    
                (rm, cm) = (r1 % h, c1 % w)
                
                if ls[rm][cm] == '.':
                    
                    if not (rm, cm) in cache[(r,c)]:
                        cache[(r,c)][(rm, cm)] = set()
                    
                    cache[(r,c)][(rm, cm)].add((ro, co))

cell_caches = [{} for _ in range(100)]

for i in range(100):
    
    items = new_items
    new_items = {}
    
    for (rm, cm) in cache:
        new_items[((rm,cm))] = set()

    for (rr,cc), cached_items in items.items():
        
        s1 = set()
        
        for (rm, cm), quads in cache[(rr,cc)].items():
            s = new_items[(rm,cm)]
            for (rq, cq) in cached_items:
                for (rrq, ccq) in quads:
                    s.add((rq+rrq, cq+ccq))
                    s1.add((rq+rrq, cq+ccq))
        
        if len(s1) > 0:
            cell_caches[i][(rr,cc)] = s1

    new_items
    
print(sum([len(v) for v in new_items.values()]))


