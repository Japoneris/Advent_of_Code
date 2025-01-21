lines = []
with open("19") as fp:
    lines = fp.read().split('\n')[:-1]


materials = ["ore", "clay", "obsidian", "geode"]
material_ID = {"ore": 0, "clay": 1, "obsidian": 2, "geode": 3}

def get_cost(item):
    cost = item.split('costs ')[1]
    vals = cost.split(" and ")
    cost = [0, 0, 0, 0]

    for val in vals:
        a, b = val.split(" ")
        cost[material_ID[b]]= int(a)
    
    return cost


def process_blueprint(line):
    a, b = line.split(":")
    ID = int(a.split()[1])
    categories = b.split(".")[:-1]
    
    # 1: ore
    # 2: clay
    # 3: obsidian
    # 4: geode
    
    robots = []
    for tp, cat in zip(["ore", "clay", "obsidian", "geode"], categories):
        dic = get_cost(cat)
        
        robots.append(dic)

    return ID, robots

def max_robots(blueprint):
    """Useless to produce more robots than necessary as
    we can only build one robot per round
    """
    tot = [0, 0, 0, 10000] # Last for unlimited geode robots
    for row in blueprint[1:]: # avoid first line
        tot = [max(r, v) for r, v in zip(row, tot)]
    
    return tot

def is_valid(robs, bpx):
    """Check if robot composition is okay or not
    """
    for a, b in zip(robs, bpx):
        if a > b:
            return False
    
    return True

def is_maximal_state(robs, bpx):
    # do not take last element nor first
    for a, b in zip(robs[:-1], bpx[:-1]):
        if a != b:
            return False
    
    return True


def update_ressources(robs, ress):
    return [a + b for a, b in zip(robs, ress)]

def consumme_ressources(ress, cost):
    return [r - c for r, c in zip(ress, cost)]

def update_robots(robs, i):
    robi = [j for j in robs]
    robi[i] += 1
    return robi

def is_sufficient(ress, cost):
    # enough ressources to build a given robot ?
    for r, c in zip(ress,  cost):
        if r < c:
            return False
    
    return True
    



def make_a_round(lst, blueprint):
    # lst: (robot, ressources)
    valid_compo = max_robots(blueprint)

    lst_new = []
    for robs, ress in lst:
        # 2. Collect ressources
        ress1 = update_ressources(robs, ress)
        
        lst_new.append((robs, ress1)) # Do nothing. Collect only
        
        # 1. build something
        for idx, cost in enumerate(blueprint):
            if is_sufficient(ress, cost):
                ress2 = consumme_ressources(ress1, cost)
                robi = update_robots(robs, idx)
                if is_valid(robi, valid_compo):
                    lst_new.append((robi, ress2))
    

    return lst_new

def is_bad_item(r0, r1):
    c0, c1 = 0, 0
    for a, b in zip(r0, r1):
        if a > b:
            c0 += 1
        elif a < b:
            c1 += 1

    return c0 == 0

def clean_list(lst):
    
    # Feed
    dico = {}
    convert = {}
    for robs, vals in lst:
        token = "-".join(list(map(str, robs)))
        if token not in dico:
            convert[token] = robs
            dico[token] = []
        dico[token].append(vals)

    # Clean
    for token, vals in dico.items():
        lst_new = []
        while vals != []:
            item = vals.pop()
            flag = True
            for x in lst_new + vals:
                if is_bad_item(item, x):
                    flag = False
                    break

            if flag:
                lst_new.append(item)
        
        dico[token] = lst_new


    # Back
    lst_new = []
    for token, vals in dico.items():
        robs = convert[token]
        lst_new.extend(list(map(lambda x: (robs, x), vals)))
    
    return lst_new

def get_max_score(lst):
    best = 0
    for _, (_,_,_,geode) in lst:
        best = max(best, geode)
    
    return best

"""
# Part 1
total = 0
for line in lines:
    ID, blueprint = process_blueprint(line)
    print("BLP", blueprint)

    lst  = [((1, 0, 0, 0), (0, 0, 0, 0))]
    for t in range(24):
        lst = make_a_round(lst, blueprint)
        #print(t, "\t", len(lst))
        lst = clean_list(lst)
        #print("\t", len(lst))


    score =  get_max_score(lst)
    print(ID, score)
    total += ID * score


print("Part 1: ", total)
"""

total = 1
for line in lines[:3]:
    ID, blueprint = process_blueprint(line)
    max_blueprint = max_robots(blueprint)
    
    print("BLP", blueprint)
    #print(max_blueprint)
    
    TMAX = 32

    lst  = [((1, 0, 0, 0), (0, 0, 0, 0))]
    for t in range(32):
        print("\nSTEP", t, len(lst))
        lst = make_a_round(lst, blueprint)
        lst = clean_list(lst)
        sc = get_max_score(lst)
        print("\tScore max:", sc)
        
        lst_max = []
        for rob, ress in lst:
            if get_max_score([(rob, ress)]) == sc:
                lst_max.append((rob, ress))
        
        if sc > 0:
            sc_worst = 0
            dt = TMAX - t
            for rob, ress in lst_max:
                sc_worst = max(sc_worst, ress[3] + rob[3] * dt)


            #print("\t ELT: ",len(lst_max))
            #print(list(map(lambda x: x[0], lst_max)))
            #print("\tWorst Case", sc_worst)
            
            # Filter low elements
            lst_new = []
            for rob, ress in lst:
                v = ress[3] + (rob[3]*2 + dt) * dt / 2
                if v >= sc_worst:
                    lst_new.append((rob, ress))
            
            lst = lst_new

    score =  get_max_score(lst)
    print(score)
    total = total * score

print("Part 2: ", total)

