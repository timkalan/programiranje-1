# (a)
def simetricen(zap): 
    if len(zap) <= 1: 
        return True
    elif len(zap) == 2:
        return zap[0] == zap[1]
    else:
        return (zap[0] == zap[-1]) and (simetricen(zap[1:-1]))


# (b)
def stevilo_delov(zap):
    simetricni = []
    for i in range(len(zap)):
        for j in range(1, len(zap[i:])):
            if (simetricen(zap[i:i+j])) and zap[i:i+j] not in simetricni:
                simetricni.append(zap[i:i+j])
    
    kandidati = []
    for kandidat in simetricni:
        if kandidat = zap[0:len(kandidat)]:
            while len(kandidat) < len(zap):
                
    
print(stevilo_delov("00101011"))
