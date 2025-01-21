
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
        return (name, (vals[0], vals[2], vals[1]))


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


def process_p1(dic, dic_ops, dic_i):

    # Process to remove elements
    while len(dic_ops) > 0:
        S = set(dic) & set(dic_i)# Value available AND value needed
        if len(S) == 0:
            break
        
        for s in S: # Value available
            val = dic[s] # value
            lst = dic_i[s] # elements needing it
            for name in lst:
                u, v, o = dic_ops[name]
                if u == s:
                    dic_ops[name] = val, v, o
            
                elif v == s:
                    dic_ops[name] = u, val, o
            
            del(dic_i[s])

        lst = list(dic_ops)
        for name in lst:
            u, v, o = dic_ops[name]
            if isinstance(u, float) & isinstance(v, float):
                if name == "root":
                    print(u, v)
                    return u, v

                dic[name] = dic_cvt[o](u, v)
                del dic_ops[name]

def inverse_ops_left(val, v, w, o):
    if o == "+":
        # val = v + w
        return val - v
    elif o == "-":
        # val = v - w
        return v - val
    elif o == "/":
        # val = v / w
        return v / val
    elif o == "*":
        # val = v * w
        return val / v

def inverse_ops_right(val, v, w, o):
    if o == "+":
        # val = v + w
        return val - w
    elif o == "-":
        # val = v - w
        return  val + w
    elif o == "/":
        # val = v / w
        return w * val
    elif o == "*":
        # val = v * w
        return val / w




def process_reverse(dic, dic_ops, dic_i):
    

    a, b, _ = dic_ops["root"]
    if isinstance(a, str):
        key, val = a, int(b)

    else:
        key, val = b, int(a)

    while key != "humn":
        
        a, b, o = dic_ops[key]
        print("Key: {} = {} {} {}".format(key, a, o, b))
        if isinstance(a, str):
            val = inverse_ops_right(val, a, int(b), o)
            key = a
            
        else:
            val = inverse_ops_left(val, int(a), b, o)
            key = b

        
        print("\t=> {} = {}".format(key, int(val)))

    return val
        


dic, dic_ops, dic_i = initialize(data)
del(dic["humn"])
print("OK")
process_p1(dic, dic_ops, dic_i)
print(dic_ops)
process_reverse(dic, dic_ops, dic_i)

# 3087390115721.0: not the right answer
