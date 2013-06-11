def transpose(m):
    height = len(m)
    width = len(m[0])
    return [[m[row][col] for row in range(0,height)] for col in range(0,width)]

def lift_1(f):
    def decorated(x):
        return [f(x[i]) for i in range(len(x))]
    return decorated

def lift_2(f):
    def decorated(x,y):
        return [f(x[i],y[i]) for i in range(len(x))]
    return decorated
def lift(f):
    def decorated(*args):
        return [f(*i) for i in transpose(args)]
    return decorated
