file = "20_test"
file = "20"

data = None
with open(file, "r") as fp:
    data = fp.read().split("\n")[:-1]
    data = list(map(int, data))


def move(lst, index, i):
    """
    :param v: value to be moved
    """
    idx = index.index(i)
    v = lst[idx]
    
    # Remove the item from the list
    lst.pop(idx)
    index.pop(idx)

    l = len(lst)
    
    
    jdx = (idx + v) % l
    lst.insert(jdx, v)
    index.insert(jdx, i)
    
    return


l = len(data)
index = [i for i in range(l)]

for i in range(l):
    move(data, index, i)
    #print(data)


i0 = data.index(0)
l = len(data)

print("Number of elements:", l)

answer = []
for i in range(1, 4):
    i1 = (i0 + i * 1000) % l
    answer.append(data[i1])

#print(data)
print(answer)
print("Part 1: {}".format(sum(answer)))


key = 811589153

# Part 2
data = None
with open(file, "r") as fp:
    data = fp.read().split("\n")[:-1]
    data = list(map(lambda x: int(x) * key, data))
    
l = len(data)
index = [i for i in range(l)]

for r in range(10):

    for i in range(l):
        move(data, index, i)

i0 = data.index(0)
answer = []
for i in range(1, 4):
    i1 = (i0 + i * 1000) % l
    answer.append(data[i1])

#print(data)
print(answer)
print("Part 2: {}".format(sum(answer)))
