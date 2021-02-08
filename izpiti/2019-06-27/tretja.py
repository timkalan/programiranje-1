from functools import cache

zabojniki = [1, 3, 4, 7, 10]

@cache
def ladje(nosilnost): 
    if nosilnost < min(zabojniki):
        return 0
    elif nosilnost == min(zabojniki):
        return 1
    else: 
        return sum (ladje(nosilnost - zaboj) for zaboj in zabojniki)


print(ladje(50)) 
        
        