
TEST = True
TEST = False

file = "25"
if TEST:
    file = "25_test"

data = None
with open(file, "r") as fp:
    data = fp.read(data)[:-1].split("\n")


kmax = max(list(map(len, data)))


pow5 = [1]
for i in range(kmax):
    pow5.append(pow5[-1] * 5)

print(pow5)

dic_val = {
        "0": 0,
        "1": 1,
        "2": 2,
        "-": -1,
        "=": -2
        }

def SNAFU_to_decimal(vals):
    v2 = [dic_val[i] for i in vals[::-1]]
    tot = 0
    for idx, v in enumerate(v2):
        tot += v * pow5[idx]
    
    return tot

dic_rev = {
        0: ("0", 0),
        1: ("1", 0),
        2: ("2", 0),
        3: ("=", 1),
        4: ("-", 1),
        5: ("0", 1)
        }

def decimal_to_SNAFU(vals):
    """
    Exemple:
    pour une valeur entre 0 et 5
    si >= 3: alors --
    si <= 2: alors 2
    """
    lst = []
    while vals != 0:
        lst.append(vals % 5)
        vals = vals // 5
    
    lst.append(0)
    lst.append(0)

    name = []
    l = len(lst)
    s = 0
    while s < l-1:
        word, adder = dic_rev[lst[s]]
        name.append(word)
        lst[s+1] += adder

        s += 1
    
    if name[-1] == "0":
        name.pop(-1)

    return "".join(name[::-1])

TOT = 0
for line in data:
    v = SNAFU_to_decimal(line)
    TOT += v
    print("====")
    print(line, v)
    print(decimal_to_SNAFU(v))

print("Final results", TOT)
print(decimal_to_SNAFU(TOT))


