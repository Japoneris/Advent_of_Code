def count_neighbors(items):
    n = len(items)

    neighbors = 0
    for idx, (a0, b0, c0) in enumerate(items):
        for (a1, b1, c1) in items[:idx]:
            # Si deux éléments sont voisins, incrémenter de 1 le compteur
            dx = abs(a0 - a1) + abs(b0 - b1) + abs(c0-c1)
            if dx == 1:
                neighbors += 1

    return n * 6 - 2 * neighbors

def get_connected_components(bubble):
    # Check connected component
    dic_group = dict([(i, [i]) for i in range(len(bubble))])
    dic_index = dict([(i, i) for i in range(len(bubble))])

    for idx, (a0, b0, c0) in enumerate(bubble):
        for jdx, (a1, b1, c1) in enumerate(bubble[:idx]):
            dx = abs(a0 - a1) + abs(b0 - b1) + abs(c0-c1)
            if dx == 1: # Connected
                ii = dic_index[idx]
                jj = dic_index[jdx]
                if ii == jj:
                    continue

                gpi = dic_group[ii]
                for z in gpi:
                    dic_index[z] = jj
            
                # Destroy the group
                del dic_group[ii]
                dic_group[jj].extend(gpi)
    
    return list(dic_group.values())



# Load input
txt = ''
#with open("18_test") as fp:
with open("18") as fp:
    txt = fp.read()

lines = txt.split("\n")[:-1]
n = len(lines)
print("Recovered {} lines".format(n))
print(lines[-1])

items = []
for line in lines:
    items.append(list(map(int, line.split(","))))

# Part 1
# Try to find how many neighbor each item has.

print("Part 1 Result:")
count_0 = count_neighbors(items)
print(count_0)


# Part 2

# Part 2 v2
### Do it differently
R = []
for i in range(3):
    x_range = list(map(lambda x: x[i], items))
    x_min, x_max = min(x_range), max(x_range)
    R.append((x_min, x_max))


dic = {}
for x in range(R[0][0]-1, R[0][1]+2):
    dic[x] = {}
    for y in range(R[1][0]-1, R[1][1]+2):
        dic[x][y] = {}
        for z in range(R[2][0]-1, R[2][1]+2):
            dic[x][y][z] = 1

for x, y, z in items:
    del dic[x][y][z]

elt = []
for x, sub in dic.items():
    for y, sub1 in sub.items():
        for z in sub1:
            elt.append((x, y, z))


groups = get_connected_components(elt)
order = sorted(groups, key=lambda x: -len(x))
print(list(map(len, order)))

bubble = []
for lst in order[1:]: # First is outside
    #bubble.extend(lst)
    bubble.extend(list(map(lambda x: elt[x], lst)))


count_1 = count_neighbors(items + bubble)
print(count_1)


