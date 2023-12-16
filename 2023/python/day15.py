def get_input(filename):
    f = open("../input/" + filename, "r")
    lines = f.read()
    f.close()
    return lines

s = get_input('day15')

def hash(s:str):
    
    current = 0
    
    for ch in s:
        current = ((current + ord(ch)) * 17) % 256
        
    return current


boxes: list[dict[str, list[(int, str)]]] = [{} for _ in range(256)]

counter = [0] * 256

for ins in s.split(','):
    
    if ins[-1] == '-':
        label = ins[:-1]
        operation = ins[-1]
        box_index = hash(label)
        pos_list = boxes[box_index].get(label)
        
        if pos_list != None:
            boxes[box_index][label] = pos_list[1:]

    else:
        [label, focal_length] = ins.split('=')
        box_index = hash(label)
        pos_list = boxes[box_index].get(label)
        
        if pos_list == None:
            counter[box_index] += 1 
            boxes[box_index][label] = [(counter[box_index], focal_length)]
        else:
            if len(pos_list) == 0:
                counter[box_index] += 1 
                pos_list.append((counter[box_index], focal_length))
            else:
                pos_list[0] = (pos_list[0][0], focal_length)
            

sum_of_code = sum([hash(step) for step in s.split(',')])
print("Part 1 Answer: ", sum_of_code)
    
total = 0
        
for (box_index, box) in enumerate(boxes, start = 1):
    xs = []
    
    for list in box.values():
        xs += list
        
    xs.sort()
    
    for (slot, (_, focal_length)) in enumerate(xs, start=1):
        total += (box_index * slot * int(focal_length))
        
print("Part 2 Answer: ", total)
