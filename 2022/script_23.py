import numpy as np

file = "23_test"
file = "23b"
file = "23"

def process_input(data):
    lst = []
    dic = {".": 0, "#": 1}
    for line in data:
        lst.append(list(map(lambda x: dic[x], line)))

    return np.array(lst)



with open(file, "r") as fp:
    data = fp.read()[:-1].split("\n")
    data = process_input(data)


def is_static(R, r, C, c):
    locs = np.where(np.abs(R - r) <= 1)[0]
    count = sum(np.abs(C[locs] - c) <= 1)
    return count == 1

def check_positions(i, R, C, D):
    """
    :param i: ID of the elve
    :param X: position
    :param D: direction
    """
    
    r, c, dirs = R[i], C[i], D

    flag = None
    if is_static(R, r, C, c):
        return None




    for d in dirs:
        if d == "N":
            locs = np.where(R == r-1)[0]
            count = sum(np.abs(C[locs] - c) <= 1)
            if count == 0:
                flag = "N"
                break
        
        elif d == "S":
            locs = np.where(R == r+1)[0]
            count = sum(np.abs(C[locs] - c) <= 1)
            if count == 0: 
                flag = "S"
                break
        
        elif d == "W":
            locs = np.where(C == c-1)[0]
            count = sum(np.abs(R[locs] - r) <= 1)
            if count == 0: 
                flag = "W"
                break

        elif d == "E":
            locs = np.where(C == c+1)[0]
            count = sum(np.abs(R[locs] - r) <= 1)
            if count == 0:
                flag = "E"
                break
    return flag


dic_dxy = {"N": (-1, 0), "S": (1, 0), "W": (0, -1), "E": (0, 1)}

def get_new_location(d, i, R, C):
    dx, dy = 0, 0
    if d is not None:
        dx, dy = dic_dxy[d]
    
    return R[i] + dx, C[i] + dy



#print(data)
R, C = np.where(data == 1)
n = len(R)
D = ["N", "S", "W", "E"] 


def print_matrix(R, C):
    R1 = R - min(R)
    C1 = C - min(C)
    rmax = max(R1 + 1)
    cmax = max(C1 + 1)
    grid = np.zeros((rmax, cmax))
    grid[R1, C1] = 1

    for line in grid:
        for v in line:
            if v == 0:
                print(".", end="")
            else:
                print("#", end="")
        print()
    return


for step in range(10):
    print("Round", step+1)
    
    # Check possible moves
    print(D)
    suggest = []
    next_locs = []
    words = []
    for i in range(n):
        d = check_positions(i, R, C, D)
        x, y = get_new_location(d, i, R, C)
        
        suggest.append(d)
        next_locs.append((x, y))
        words.append("{}-{}".format(x, y))

    # Accept or reject moves
    for i in range(n):
        if suggest[i] is None:
            # Do not move anything as no optimal positions
            continue

        w = words[i]
        if words.count(w) == 1: # Should be only one solution
            #print("Move", R[i], C[i], "to", next_locs[i])
            R[i], C[i] = next_locs[i]
    
    # Update direction
    D = D[1:] + [D[0]]
    
    #print_matrix(R, C)

# Get dimensions
r_min, r_max = min(R), max(R)
c_min, c_max = min(C), max(C)

dr = r_max - r_min
dc = c_max - c_min

area = (dr+1) * (dc+1)
print(dr, dc, area)
print("Part 1:", area - n)

############
# Part 2

step = 0

R, C = np.where(data == 1)
n = len(R)
D = ["N", "S", "W", "E"] 

while True:
    step += 1
    print("Round", step+1)
    

    # Check possible moves
    suggest = []
    next_locs = []
    words = []
    for i in range(n):
        d = check_positions(i, R, C, D)
        x, y = get_new_location(d, i, R, C)
        
        suggest.append(d)
        next_locs.append((x, y))
        words.append("{}-{}".format(x, y))

    # Accept or reject moves
    flag = True
    for i in range(n):
        if suggest[i] is None:
            # Do not move anything as no optimal positions
            continue

        w = words[i]
        if words.count(w) == 1: # Should be only one solution
            #print("Move", R[i], C[i], "to", next_locs[i])
            R[i], C[i] = next_locs[i]
            flag = False
    
    if flag:
        print("Stop at", step)
        break

    # Update direction
    D = D[1:] + [D[0]]
    
