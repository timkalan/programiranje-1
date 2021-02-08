def fiksna(sez):
    for i in range(len(sez)):
        if i == sez[i]:
            return i
    return None

def fiksna2(sez):
    pol = len(sez) // 2
    