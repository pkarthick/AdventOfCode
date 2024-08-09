f = open("../input/day20", "r")
ls = f.read().splitlines()
f.close()

broadcasted = []

modules = {}

flip_flops = {}
conjunctions = {}
kinds = {}
input_modules = {}

for l in ls:
    frags = l.split(' -> ')
    to_modules = frags[1].split(', ')
    from_modules = frags[0]
    
    if frags[0] == 'broadcaster':
        broadcasted = to_modules
    else:
        kind = frags[0][:1]
        module = frags[0][1:]
        
        kinds[module] = kind
        modules[module] = to_modules
        if kind == '%':
            flip_flops[module] = False
        else:
            input_modules[module] = {}
        

for con_m in input_modules:
    for k in modules:
        if con_m in modules[k]:
            input_modules[con_m][k] = False

high = 0
low = 0

cycle_count = 1000

i = 0

for i in range(100000):
    
    # if all([not s for s in flip_flops.values()]):
    #     if all([not s for s in input_modules[im].values()] for im in input_modules):
    #         if i > 0:
    #             cycle_count = i
    #             break

    pending = [('button', False, 'broadcaster')]
    
    # print()

    while len(pending) > 0:
        
        (from_module, signal , to_module) = pending[0]
        
        if signal and from_module == 'zf' and to_module=='gh':
            cycle_count = i
            print(to_module, cycle_count+1)
        
        # print(pending[0])

        if signal:
            high += 1
        else:
            low += 1
        
        if to_module in kinds:
            
            if kinds[to_module] == '%':
                if signal:
                    pass
                else:
                        
                    flip_flops[to_module] = not flip_flops[to_module]
                        
                    for to in modules[to_module]:
                        pending.append((to_module, flip_flops[to_module], to))
                
            elif kinds[to_module] == '&':
                
                input_modules[to_module][from_module] = signal
                
                inv_s = not all(input_modules[to_module].values())
                
                for to in modules[to_module]:
                    pending.append((to_module, inv_s, to))
                
            
        else:
            if to_module == 'broadcaster':
                for to in broadcasted:
                    pending.append(('broadcaster', False, to))
            
        
        pending = pending[1:]
        
    i += 1
        
# print(cycle_count)

# times = 1000 // cycle_count

# print('times', times)
# print('high', high)
# print('low', low)
        
# print((high*times * low*times))


# rk 3732
# cd 3792
# zf 3946
# qx 4056