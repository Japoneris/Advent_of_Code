import numpy as np


FLAG_TEST = True
FLAG_TEST = False

file = "24_test"
if FLAG_TEST == False:
    file = "24"


def process_input(data):
    # Size of the board
    r_max, c_max = len(data), len(data[0])
    
    wind = []
    for idx, line in enumerate(data[1:-1]):
        for jdx, c in enumerate(line[1:-1]):
            if c == ".":
                continue
            else:
                wind.append((idx, jdx, c))
    
    # Remove borders
    return (r_max - 2, c_max - 2, wind)

dic_update = {
        ">": (0, 1), 
        "<": (0, -1),
        "^": (-1, 0),
        "v": (1, 0)
        }

def update_wind_position(tpl, r_max, c_max):
    """Update wind location using circular update"""
    r, c, d = tpl
    dr, dc = dic_update[d]
    return ((r + dr) % r_max, (c + dc) % c_max, d)


def print_wind(wind, r_max, c_max):
    lst = [["." for _ in range(c_max)]  for _ in range(r_max)]
    for x, y, d in wind:
        lst[x][y] = d
    
    lst = list(map(lambda x: "".join(x), lst))
    print("\n".join(lst))
    return

def get_wind_matrix(wind, r_max, c_max):
    lst = [[0 for _ in range(c_max)]  for _ in range(r_max)]
    for x, y, d in wind:
        lst[x][y] = 1

    return lst
    

def update_location(pos, wind):
    """Not efficient, better to keep a grid for updates.
    """
    r, c = pos

    # Keep wind at distance 1 only
    # Problematic wind locations
    sub = list(filter(lambda x: abs(x[0] - r) + abs(x[1] - c) <= 1, wind))

    # move up, down, left, right or stay
    cands = [(r+1, c), (r-1, c), (r, c+1), (r, c-1), (r, c)]
    
    for rw, cw, _ in sub:
        # Wind should not overlap new position
        cands = list(filter(lambda x: abs(rw - x[0]) + abs(cw - x[1]) != 0, cands)) 
    
    return cands


def update_location_v2(pos, wind_matrix):
    r, c = pos
    r_max, c_max = len(wind_matrix), len(wind_matrix[0])

    cands = [(r+1, c), (r-1, c), (r, c+1), (r, c-1), (r, c)]
    lst_keep = []

    for ri, ci in cands:
        if (ri == -1) & (ci == 0):
            lst_keep.append((ri, ci))
        
        elif (ri == r_max) & (ci == c_max -1):
            lst_keep.append((ri, ci))
        elif (ri < 0) | (ci < 0) | (ri >= r_max) | (ci >= c_max):
            continue
        elif wind_matrix[ri][ci] == 0:
            lst_keep.append((ri, ci))
    
    return lst_keep

data = None
with open(file, "r") as fp:
    data = fp.read()[:-1].split("\n")


r_max, c_max, wind = process_input(data)
print("Dimensions:", (r_max, c_max))
print("Wind number:", len(wind))



def filter_location(x, r_max, c_max):
    if (x[0] == -1) & (x[1] == 0):
        return True
    
    elif (x[0] == r_max) & (x[1] == c_max-1):
        return True
    
    else:
        return (x[0] >= 0) & (x[0] < r_max) & (x[1] >= 0) & (x[1] < c_max)

def drop_duplicates(lst_pos):
    dico = {}
    for x, y in lst_pos:
        dico["{}-{}".format(x, y)] = (x, y)

    return list(dico.values())

def check_end(lst_pos, r_max, c_max):
    for x, y in lst_pos:
        if (x == r_max) & (y == c_max-1):
            return True
    return False

def check_end_bis(lst_pos, r_max, c_max):
    for x, y in lst_pos:
        if (x == -1) & (y == 0):
            return True
    return False

if FLAG_TEST:
    print_wind(wind, r_max, c_max)

step = 1
lst_locs = [(-1, 0)]
while True:
    print("step", step)
    wind = list(map(lambda x: update_wind_position(x, r_max, c_max), wind))
    wind_mat = get_wind_matrix(wind, r_max, c_max)



    if FLAG_TEST:
        print_wind(wind, r_max, c_max)
    
    # Update location
    new_locs = []
    for loc in lst_locs:
        #new_locs.extend(update_location(loc, wind))
        new_locs.extend(update_location_v2(loc, wind_mat))



    if FLAG_TEST:
        print("Before", new_locs)
    

    # Wait or move.

    if len(new_locs) == 0:
        break
    
    lst_locs = drop_duplicates(new_locs)
    if FLAG_TEST:
        print(len(lst_locs))
        print("After", lst_locs)
    
    
    if check_end(lst_locs, r_max, c_max):
        print("Finished at ", step)
        break
    

    elif (step > 30) & FLAG_TEST:
        break
    
    step += 1

# Part 2:
# Go back to start
lst_locs = [(r_max, c_max-1)]

while True:
    print("step", step)
    wind = list(map(lambda x: update_wind_position(x, r_max, c_max), wind))
    wind_mat = get_wind_matrix(wind, r_max, c_max)
    
    if FLAG_TEST:
        print_wind(wind, r_max, c_max)
    
    # Update location
    new_locs = []
    for loc in lst_locs:
        #new_locs.extend(update_location(loc, wind))
        new_locs.extend(update_location_v2(loc, wind_mat))



    if FLAG_TEST:
        print("Before", new_locs)
    
    # Filter
    #new_locs = list(filter(lambda x: filter_location(x, r_max, c_max), new_locs))
    

    # Wait or move.

    if len(new_locs) == 0:
        break
    
    lst_locs = drop_duplicates(new_locs)
    if FLAG_TEST:
        print(len(lst_locs))
        print("After", lst_locs)
    
    
    step += 1
    if check_end_bis(lst_locs, r_max, c_max):
        print("Back to the start at ", step)
        break

    


# Part 2:
# Go back to start


lst_locs = [(-1, 0)]
while True:
    print("step", step)
    wind = list(map(lambda x: update_wind_position(x, r_max, c_max), wind))
    wind_mat = get_wind_matrix(wind, r_max, c_max)
    
    
    # Update location
    new_locs = []
    for loc in lst_locs:
        new_locs.extend(update_location_v2(loc, wind_mat))




    # Wait or move.

    if len(new_locs) == 0:
        break
    
    lst_locs = drop_duplicates(new_locs)
    
    step += 1
    if check_end(lst_locs, r_max, c_max):
        print("Finished at ", step )
        break
    
    
