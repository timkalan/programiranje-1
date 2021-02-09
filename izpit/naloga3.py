from functools import lru_cache

# =============================================================================
# Psička Nara po njivi preganja krokarje. Opazila je, da jo lastnik čaka na
# drugem koncu polja, zato hiti k njemu, pri tem pa hoče prestrašiti kar se da
# veliko ubogih ptičev.
#
# Njivo predstavimo kot matriko, ki v vsakem polju vsebuje število krokarjev,
# ki jih pasja navihanka prežene, če teče preko tega polja.
# =============================================================================

primer = [
    [2, 3, 0, 2, 9],
    [8, 3, 5, 1, 2],
    [1, 2, 7, 2, 0],
    [4, 3, 6, 5, 5],
]

# (a)
# =============================================================================
# Nara se nahaja v zgornjem levem kotu njive (polje `(0, 0)`). Ker se ji mudi
# k lastniku, se vztrajno premika desno. Na vsakem koraku se lahko premakne:
#   - desno
#   - diagonalno desno-gor
#   - diagonalno desno-dol
#
# Pregon krokarjev zaključi na poljubnem skrajno desnem polju njive. Napišite
# funkcijo, ki izračuna največje število krokarjev, ki jih lahko nagajivka
# prežene.
# =============================================================================

def nara(njiva):
    h = len(njiva)
    w = len(njiva[0])

    def aux(i, j):
        if (i >= h) or (i < 0) or (j >= w):
            return 0
        else:
            desno = aux(i, j+1) 
            desno_gor = aux(i+1, j+1)
            desno_dol = aux(i-1, j+1)
            return njiva[i][j] + max(desno, desno_dol, desno_gor)

    return aux(0, 0)


# (b)
# =============================================================================
# Funkcijo iz točke (a) prilagodite tako, da ji dodatno podate indeks vrstice,
# v kateri Nara začne, in indeks vrstice, v kateri Nara konča.
#
# Funkcija naj vrne seznam VSEH optimalnih poti, kjer pot predstavimo s
# seznamom indeksov polj, preko katerih Nara teče.
# =============================================================================

def nara_pot(njiva, vrs, sto):
    h = len(njiva)
    w = len(njiva[0])

    poti = []

    def aux(i, j):
        pdesno, pdol, pgor = [], [], []
        if (i >= h) or (i < 0) or (j >= w):
            return 0, []
        else:
            desno, pdesno = aux(i, j+1) 
            desno_gor, pgor = aux(i+1, j+1)
            desno_dol, pdol = aux(i-1, j+1)
            if desno == max(desno, desno_dol, desno_gor):
                pdesno.append((i, j+1))
                return njiva[i][j] + desno, pdesno
            elif desno_gor == max(desno, desno_dol, desno_gor):
                pgor.append((i+1, j+1))
                return njiva[i][j] + desno_gor, pgor
            else:
                pdol.append((i-1, j+1))
                return njiva[i][j] + desno_dol, pdol

    return aux(vrs, sto)

print(nara_pot(primer, 0, 0))
