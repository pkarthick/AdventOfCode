from dataclasses import dataclass, field


f = open("../input/day19", "r")
sections = f.read().split('\n\n')
f.close()

workflows = sections[0].splitlines()
parts = sections[1].splitlines()

@dataclass
class Part:
    ratings: dict[str, int]

@dataclass
class Validation:
    rating_name: str
    rule_operator: str
    rating_value: int

@dataclass 
class Rule:
    validation: Validation
    workflow: str
    

@dataclass
class Workflow:
    name: str
    rules: list[Rule] = field(default_factory=list)


def create_rule(rule):
    rule_items = rule.split(":")
    
    if len(rule_items) == 1:
        return Rule(None, rule)
    else:
        
        frags = rule_items[0].split('<')
        rule_operator = "<"
        
        if len(frags) == 1:
            frags = rule_items[0].split('>')
            rule_operator = ">"

        rating_name = frags[0]
        
        rating_value = int(frags[1])
        return Rule(Validation(rating_name, rule_operator, rating_value), rule_items[1])

parts_list = []    

for p in parts:
    ratings = {}
    for rating in p[1:-1].split(','):
        [name, value] = rating.split("=")
        ratings[name] = int(value)
        
    parts_list.append(Part(ratings))
        

    
workflows_map = {}        

for workflow in workflows:
    
    [name, rules_str] = workflow.split("{")
    rules = [create_rule(rule) for rule in rules_str[:-1].split(",")]
    
    workflows_map[name] = Workflow(name, rules)


accepted = []

def which_workflow(wfn: str, part: Part):
    if wfn == 'A':
        return 'A'
    elif wfn == 'R':
        return 'R'
    else:
        workflow = workflows_map[wfn]
        
        for rule in workflow.rules:
            if rule.validation == None:
                return rule.workflow
            
            elif rule.validation.rule_operator =='<':
                if part.ratings[rule.validation.rating_name] < rule.validation.rating_value:
                    return which_workflow(rule.workflow, part)
                
            elif rule.validation.rule_operator =='>':
                if part.ratings[rule.validation.rating_name] > rule.validation.rating_value:
                    return which_workflow(rule.workflow, part)

        print("Unexpected!")

for part in parts_list:
    next_wf = which_workflow('in', part)
    while next_wf != 'A' and next_wf != "R":
        next_wf = which_workflow(next_wf, part)
    if next_wf == 'A':
        accepted.append(part)

sum_of_ratings = sum([sum(part.ratings.values()) for part in accepted])

print('Answer for Part 1:', sum_of_ratings)

def get_sum_of_ratings(m: dict[str, list[tuple[int, int]]]):
        combs = 1
        
        for vs in m.values():
            total = 0
            
            for v in vs:
                total += v[1]-v[0]+1
                
            combs *= total
            
        return combs

def count_combinations(rules: list[Rule], m: dict[str, list[tuple[int, int]]]) -> int:
    
    if rules == []:
        return get_sum_of_ratings(m)
    else:
        
        rule = rules[0]
        
        if rule.validation == None:
            if rule.workflow == 'A':
                return get_sum_of_ratings(m)
            elif rule.workflow == 'R':
                return 0
            else:
                return count_combinations(workflows_map[rule.workflow].rules, m)
            
        else:
            
            if rule.validation.rule_operator == '<':
                rs = m[rule.validation.rating_name]
                
                index = -1
                
                for (i, r) in enumerate(rs):
                    if rule.validation.rating_value >= r[0] and rule.validation.rating_value <= r[1]:
                        index = i
                        break
                    
                matching = (rs[index][0], rule.validation.rating_value-1)
                non_matching = (rule.validation.rating_value, rs[index][1])
                    
                matching_list = rs[0: index] + [matching] + rs[index + 1: ]
                non_matching_list = rs[0: index] + [non_matching] + rs[index + 1: ]
                
                copy_matching = {k:[(v[0], v[1]) for v in m[k]] for k in m}
                copy_non_matching =  {k:[(v[0], v[1]) for v in m[k]] for k in m}
                
                copy_matching[rule.validation.rating_name] = matching_list
                copy_non_matching[rule.validation.rating_name] = non_matching_list
                
                s1 = 0
                
                if rule.workflow == 'A':
                    s1 = get_sum_of_ratings(copy_matching)
                elif rule.workflow == 'R':
                    s1 = 0
                else:
                    s1 = count_combinations(workflows_map[rule.workflow].rules, copy_matching) 
                    
                s2 = count_combinations(rules[1:], copy_non_matching)
                return s1 + s2
                
            else:
                rs = m[rule.validation.rating_name]
                
                index = -1
                
                for (i, r) in enumerate(rs):
                    if rule.validation.rating_value >= r[0] and rule.validation.rating_value <= r[1]:
                        index = i
                        break
                    
                matching = (rule.validation.rating_value+1, rs[index][1])
                non_matching = (rs[index][0], rule.validation.rating_value)
                    
                matching_list = rs[0: index] + [matching] + rs[index + 1: ]
                non_matching_list = rs[0: index] + [non_matching] + rs[index + 1: ]
                
                copy_matching = {k:[(v[0], v[1]) for v in m[k]] for k in m}
                copy_non_matching =  {k:[(v[0], v[1]) for v in m[k]] for k in m}
                
                copy_matching[rule.validation.rating_name] = matching_list
                copy_non_matching[rule.validation.rating_name] = non_matching_list
                
                s1 = 0
                
                if rule.workflow == 'A':
                    s1 = get_sum_of_ratings(copy_matching)
                elif rule.workflow == 'R':
                    s1 = 0
                else:
                    s1 = count_combinations(workflows_map[rule.workflow].rules, copy_matching) 
                
                s2 = count_combinations(rules[1:], copy_non_matching)
                return s1 + s2
                
    
combs = count_combinations(workflows_map['in'].rules, {'m': [(1, 4000)], 's': [(1, 4000)], 'a': [(1, 4000)], 'x': [(1, 4000)]})

print('Answer for Part 2:', combs)

    
