primer = [350, 230, 370, 920, 620, 80, 520, 410, 780, 630]

def rozle(gore):
    mozne = []
    opt = gore[0]

    for gora in gore: 
        if gora < opt:
            opt = gora
        mozne.append(opt)

    return max([(gora - mozna) for (gora, mozna) in zip(gore, mozne)])

print(rozle(primer))

