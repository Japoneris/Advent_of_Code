lines = []
with open("19") as fp:
    lines = fp.read().split('\n')[:-1]


def get_cost(item):
    cost = item.split('costs ')[1]
    vals = cost.split(" and ")
    dic = {}

    for val in vals:
        a, b = val.split(" ")
        dic[b]= int(a)
    
    return dic


def process_blueprint(line):
    a, b = line.split(":")
    ID = int(a.split()[1])
    categories = b.split(".")[:-1]
    
    # 1: ore
    # 2: clay
    # 3: obsidian
    # 4: geode
    
    robots = {}
    for tp, cat in zip(["ore", "clay", "obsidian", "geode"], categories):
        dic = get_cost(cat)
        robots[tp] = dic

    return ID, robots



materials = ["ore", "clay", "obsidian", "geode"]


def get_score1(blueprint,
        ressources={"ore": 0, "clay": 0, "obsidian": 0, "geode":0}, 
        robots={"ore":1, "clay":0, "obsidian": 0, "geode": 0}, steps=0,
        factory=None):

    if steps > 24:
        return 0

    elif steps == 24:
        return ressources["geode"]

    score = ressources["geode"] + (24 - steps) * robots["geode"]
    

    if factory is not None: # Create robot

        ress = ressources.copy()
        robi = robots.copy()
        # Get the additional resources of the round
        for t, v in robots.items():
            ress[t] += v

        # Create the robot
        robi[factory] += 1
        cost = blueprint[factory]
        for t, v in cost.items():
            ress[t] -= v


        
        sc = get_score1(blueprint, ress, robi, steps+1, factory=None)
        score = max(sc, score)
        return score 

    else: # No robot to create during the current round
        for tp, cost in blueprint.items():
            # check if the robot can be built using resources
            flag = True
            for t in cost:
                if robots[t] == 0: # impossible to built
                    flag = False
                    break

            if flag:
                # Check how many steps necessary to build the robot
                n_step = 0
                for t, v in cost.items():
                    n = (v - ressources[t] - 1) // robots[t] + 1 
                    n_step = max(n_step, n)
                
                # Update the number of ressources
                ress = ressources.copy()
                for r, v in robots.items():
                    ress[r] += v *  n_step

                sc = get_score1(blueprint, ress, robots, steps+n_step, factory=tp)
                score = max(sc, score)
        
        return score


total = 0
for line in lines:
    
    ID, robots = process_blueprint(line)
    s_tot = get_score1(robots)
    print(ID, s_tot)
    total += ID * s_tot

print("Part 1: ", total)
