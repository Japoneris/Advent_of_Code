TEST = True
TEST = False
file = None

if TEST:
    file = "22_test"
else:
    file = "22"

data = None
with open(file, "r") as fp:
    data = fp.read()[:-1].split("\n")

puzzle = data[:-2]
key = data[-1]

def process_puzzle(puzzle):
    l_max = max(list(map(len, puzzle)))
    lst = []
    for line in puzzle:
        l = len(line)
        lst.append(line + " " * (l_max - l))
    
    return lst

puzzle = process_puzzle(puzzle)

def get_start(puzzle):
    """Search for start position
    """
    for idx, v in enumerate(puzzle[0]):
        if v == ".":
            return (0, idx, "Right")
    
    return (-1, -1, None)

dic_turn = {
        "Up": {"R": "Right", "L": "Left"},
        "Down": {"R": "Left", "L": "Right"},
        "Left": {"R": "Up","L": "Down"},
        "Right": {"R": "Down", "L": "Up"}
        }

def turn(direction, angle):
    """
    Direction: up / down / left / right
    Angle: Left or right
    """
    return dic_turn[direction][angle]

dic_move = {"Up": (-1, 0), "Down": (1, 0), "Right": (0, 1), "Left": (0, -1)}

def move(direction):
    return dic_move[direction]

def process_move(r, c, d, action, puzzle):
    
    if isinstance(action, str):
        return (r, c, turn(d, action))
    
    else:
        dr, dc = move(d)
        lr, lc = len(puzzle), len(puzzle[0])
        count = 0
        last = (r, c, d)
        if TEST:
            print("action", action)
        
        while count != action:
            r = (r + dr) % lr
            c = (c + dc) % lc
            if puzzle[r][c] == "#":
                return last
            
            elif puzzle[r][c] == ".":
                count += 1
                last = (r, c, d)
            
            elif puzzle[r][c] == " ":
                continue
        
        return last 

        

def make_the_game(puzzle, key):
    parsed_key = parse_key(key)
    r, c, d = get_start(puzzle)

    for action in parsed_key:
        r, c, d = process_move(r, c, d, action, puzzle)
    
    return r, c, d


def parse_key(key):
    lst = []
    buffer = []
    for v in key:
        if v in "RL":
            if len(buffer) > 0:
                lst.append(int("".join(buffer)))
                buffer = []

            lst.append(v)
        else:
            buffer.append(v)

    if len(buffer) > 0:
        lst.append(int("".join(buffer)))
        
    return lst


def get_score_part_1(r, c, d):
    dic = {"Down": 1, "Right": 0, "Left": 2, "Up": 3}
    score_d = dic[d]
    return (r+1) * 1000 + (c+1) * 4 + score_d

if TEST:
    print(puzzle)
    print("="*20)
    print(key)

r, c, d = make_the_game(puzzle, key)
print("Socre part 1:", get_score_part_1(r, c, d))


################ Part 2
# When moving to another tile, need maybe to turn.



Cube_shape = [
    [0, 1, 1],
    [0, 1, 0],
    [1, 1, 0],
    [1, 0, 0],
        ]


lc_r, lc_c = len(Cube_shape), len(Cube_shape[0])

# Convert to letters
cnt = ord("A")
Cube_faces = []
for row in Cube_shape:
    lst = []
    for v in row:
        if v == 0:
            lst.append("")
        else:
            lst.append(chr(cnt))
            cnt += 1
    Cube_faces.append(lst)

# Get reverse location
dic_location = {}
for idx, line in enumerate(Cube_faces):
    for jdx, v in enumerate(line):
        if v != "":
            dic_location[v] = (idx, jdx)

# Build dictionnary to know where to move.
dic_transition = {
        "A": {
            "Left": ("D", "RR"), 
            "Right": ("B", ""), 
            "Up": ("F", "R"), 
            "Down": ("C", "")},
        "B": {
            "Left": ("A", ""), 
            "Right": ("F", "R"), 
            "Up": ("", "R"), 
            "Down": ("C", "R")},
        "C": {
            "Left": ("D", "R"), # 
            "Right": ("B", "R"),  #
            "Up": ("A", ""), 
            "Down": ("E", "")},
        "D": {
            "Left": ("A", "RR"), # 
            "Right": ("E", ""), 
            "Up": ("C", "R"), # 
            "Down": ("F", "")},
        "E": {
            "Left": ("D", ""), 
            "Right": ("B", "RR"), # 
            "Up": ("C", ""), 
            "Down": ("F", "R")}, #
        "F": {
            "Left": ("", ""), 
            "Right": ("", ""), 
            "Up": ("D", ""), 
            "Down": ("", "")},

        }
    # Up
    # Down
    # Left
    # Right





width = 50
if TEST:
    width = 4
    Cube_shape = [
        [0, 0, 1, 0],
        [1, 1, 1, 0],
        [0, 0, 1, 1],
        ]





def move_to_tile(r, c, d, action):
    """
    action should be a number
    """
    # Si r / c % 50 != 0, alors rien a faire
    dr, dc = move(d)
    lr, lc = len(puzzle), len(puzzle[0])
    count = 0
    last = (r, c, d)
    
    l_cube_r = len(Cube_shape)
    l_cube_c = len(Cube_shape[0])
    
    while count != action:
        r1 = (r + dr) % lr
        c1 = (c + dc) % lc

        if puzzle[r1][c1] == " ":
            # TODO 
            # Need to fold
            
            R, C = r1 // width,  c1 // width

            if dr != 0:
            

            else: # dc != 0


        elif puzzle[r1][c1] == "#":
            # Stop here
            return last
        
        elif puzzle[r1][c1] == ".":
            # Nothing special
            r, c = r1, c1
            last = (r, c, d)
            count += 1

    return last



