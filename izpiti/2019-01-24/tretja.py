from functools import cache

mocvara1 = [2, 4, 1, 2, 1, 3, 1, 1, 5]
mocvara2 = [4, 1, 8, 2, 11, 1, 1, 1, 1, 1]


def zabica(mocvara): 

    @cache
    def aux(i, e):
        if i >= len(mocvara):
            return 0

        energija = e + mocvara[i]
        mozni = [aux(i + dolzina, energija - dolzina) 
                for dolzina in range(1, energija + 1)]
        return min(mozni) + 1 
    
    return aux(0, 0)

print(zabica([10 for _ in range(50)]))
