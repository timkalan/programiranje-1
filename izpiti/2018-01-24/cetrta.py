sadovnjak = [[2, 4, 1, 1], 
             [3, 2, 0, 5], 
             [8, 0, 7, 2]]

def lisjak(n, sad): 
    def aux(i, j, k): 
        if (i < 1) or (j < 1) or (k < 1):
            return 0
        elif i == 1: 
            return sum(sad[i-1][0:k])
        elif j == 1:
            return sum([sad[l][j] for l in range(k)] )
        elif k == 1:
            return sad[i][j]
        else:
            return max()
        