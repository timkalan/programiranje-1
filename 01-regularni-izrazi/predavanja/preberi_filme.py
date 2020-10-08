import re

with open('01-regularni-izrazi/predavanja/250-najbolj-znanih-filmov.html') as f:
    vsebina = f.read()

vzorec = (
    r'<a href="/title/tt'
    r'(?P<id>\d{7})'  # ID ima sedem števk
    r'/\?ref_=adv_li_tt"\n>'  # neka šara vmes med id-jem in naslovom
    r'(?P<naslov>.*?)'  # zajamemo naslov
    r'</a>'
    r'\s+'
    r'<span class="lister-item-year text-muted unbold">'
    r'(\([IVXLCDM]+\) )?'
    r'\((?P<leto>.*?)\)'
)

count = 0
for zadetek in re.finditer(vzorec, vsebina):
    print(zadetek.groupdict())
    count += 1
print(count)


# ?P nam omogoča, da poimenujemo skupino - do poimenovanih z groupdict
# r pred nizom, da python ve, naj ignorira ubežne znake
# za . in * vprašaj, da povemo naj dela v šibkem načinu - čim krajši pattern
# re.finditer nek poseben objekt - damo.group(n), da da n-to stvar
# re.findall, da se najde vse nize, ki ustrezajo