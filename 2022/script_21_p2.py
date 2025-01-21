
file = "21"
#file = "21_test"

data = None
with open(file, "r") as fp:
    data = fp.read().split("\n")[:-1]


dic_cvt = {
        "+": lambda x, y: x+y, 
        "-": lambda x, y: x - y,
        "*": lambda x, y: x * y,
        "/": lambda x, y: x / y,
        }

def process_line(line):
    name, ops = line.split(": ")
    vals = ops.split(" ")
    
    if len(vals) == 1:
        return (name, float(vals[0]))
    
    else:
        return (name, (vals[0], vals[2], dic_cvt[vals[1]]))


def initialize(data):
    # Part 1
    dic = {} # Value
    dic_ops = {} # Operation needed
    dic_i = {} # inverse ops 

    for line in data:
        name, ops = process_line(line)
    
        if isinstance(ops, float):
            # Store value
            dic[name] = ops
    
        else:
            # Decompose
            u, v, o = ops
            dic_ops[name] = (u, v, o)
        
            if u not in dic_i:
                dic_i[u] = [name]
            else:
                dic_i[u].append(name)

            if v not in dic_i:
                dic_i[v] = [name]
            else:
                dic_i[v].append(name)

    return dic, dic_ops, dic_i

def process(dic, dic_ops, dic_i):
    
    # Process to remove elements
    while len(dic_ops) > 0:
        
        S = set(dic) & set(dic_i) # Value available AND value needed

        for s in S: # Value available
            val = dic[s] # value
            lst = dic_i[s] # elements needing it

            for name in lst:
                u, v, o = dic_ops[name]
                if u == s:
                    dic_ops[name] = val, v, o
                    dic_i[s].remove(name)
            
                elif v == s:
                    dic_ops[name] = u, val, o
                    dic_i[s].remove(name)

        lst = list(dic_ops)
        for name in lst:
            u, v, o = dic_ops[name]
            if isinstance(u, float) & isinstance(v, float):
                dic[name] = o(u, v)
                del dic_ops[name]
    
def process_p2(dic, dic_ops, dic_i):
    
    # Process to remove elements
    while len(dic_ops) > 0:
        
        S = set(dic) & set(dic_i)# Value available AND value needed

        for s in S: # Value available
            val = dic[s] # value
            lst = dic_i[s] # elements needing it

            for name in lst:
                u, v, o = dic_ops[name]
                if u == s:
                    dic_ops[name] = val, v, o
                    dic_i[s].remove(name)
            
                elif v == s:
                    dic_ops[name] = u, val, o
                    dic_i[s].remove(name)

        lst = list(dic_ops)
        for name in lst:
            u, v, o = dic_ops[name]
            if isinstance(u, float) & isinstance(v, float):
                if name == "root":
                    print(u, v)
                    return u, v

                dic[name] = o(u, v)
                del dic_ops[name]
    

dic, dic_ops, dic_i = initialize(data)

dic, dic_ops, dic_i = initialize(data)
process(dic, dic_ops, dic_i)

print("Part 1", dic["root"])

dic, dic_ops, dic_i = initialize(data)
v = input("Give me a value")
dic["humn"] = float(v)
print(dic)
process_p2(dic, dic_ops, dic_i)
print(dic_ops)


        
