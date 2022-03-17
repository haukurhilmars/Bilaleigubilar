# Rafvæðing Bílaleigubíla á Íslandi

Niðurstaða Þarfa- og Kostnaðargreining fyrir innviði á ferðamannastöðum og við Keflavíkurflugvöll

Skýrslu má finna [hér](https://www.stjornarradid.is/library/02-Rit--skyrslur-og-skrar/Rafv%C3%A6%C3%B0ing%20b%C3%ADlaleigub%C3%ADla%20%C3%A1%20%C3%8Dslandi%20-febr%C3%BAar%202021.pdf)

Gagnvirt líkan má finna [hér](https://haukur.shinyapps.io/Rafbilar/)

![](https://github.com/haukurhilmars/Bilaleigubilar/blob/main/shiny_screenshot.png)

## Aðferðarfræði

Forsendur hvern ferðamannastað:
+ 3 tengistaðir
+ 3 mismunandi líkur um fjölda sem kemur frá/fer til tengistaðs
+ Hlutfall bíla sem koma tómir
+ Opnunartími
+ Lengd sem er opið
+ Toppur yfir daginn
+ Staðalrávik dreifingar
+ Hámarksfjöldi í mánuði yfir sumartíma
+ Hámarksfjöldi í mánuði yfir vetrartíma

Input í líkan:
+ Ferðamannastaður
+ Meðal afl á hvern bíl
+ Fjöldi ferðamanna í bíl
+ Hlutfall ferðamanna á einkabíl
+ Hlutfall bílaleigubílar sem eru rafbílar
+ Uppbygging hleðsluinnviða við gististaði (já/nei)
+ Bílar teknir úr hleðslu um leið og þeir eru búnir að hlaða? (já/nei)
+ Viðmið þegar bílar eru teknir úr hleðslu (80%/100%)
  
Skref útreiknings fyrir einstaka bíl:
1. Tímasetning komutíma hvers bíls dregin úr líkindadreifingu byggt á opnunartíma, lengd sem er opið, toppi yfir daginn og staðalfráviki dreifingar (truncated normal dreifing)
2. Hvort að bíll sé rafbíll eða ekki dregið úr binomial dreifingu byggt á hlutfalli rafbíla
3. Upphafsstaða bíls dregin úr líkindadreifingu
	a. Truncnorm(95,5) ef uppbygging er til staðar á gististöðum
	b. Truncnorm(60,25) ef uppbygging ekki til staðar á gististöðum
4. Tegund bíls ákvörðuð
	a. Í líkaninu er gert ráð fyrir 3 mismunandi stærðum rafbíla, líkum er samt stillt þannig að það eru 100% líkur á að velja tegund 2 sem er með 100 kwh rafhlöðu, getur tekið við 200 kw
5. Upphafstaða rafhlöðu í kWh ákvörðuð út frá upphafsstöðu rafhlöðu í prósentum og stærð rafhlöðu skv. tegund bíls
6. Eknir km við komu á ferðamannastað dregnir útfrá líkum á tengistað (3 mismunandi staðir)
7. Eyðsla vegna ekinna km fundið út frá Eknum km og tegund bíls
8. Stada kWh og hlutfallsleg staða rafhlöðu við komu á ferðamannastað ákvörðuð út frá upphafsstöðu og eyðslu vegna ekinna km 
9. kWh sem þarf að hlaða byggt á hvort að hlaða á bíl í 80% eða 100%
10. Tími sem tekur að hlaða, byggir annaðhvort á tíma sem ferðamaður stoppar, eða hversu langan tíma tekur að hlaða bílinn. Ræðst af því hvort að bílar séu teknir úr hleðslu um leið og þeir eru búnir að hlaða eða ekki
11. Ákvörðun um hvort að bíll hlaði eða ekki út frá hlutfallslegri stöðu rafhlöðu
	a. Ef staða x<25% -> 100%
	b. Ef staða 25%<x<50% -> 80%
	c. Ef staða 50%<x<80% -> 50%
	d. Annars 0%
	e. Aðrar líkindaforsendur skilgreindar fyrir Keflavík en ferðamannastaði
12. Ef bíll hleður tekur hann hleðslustöð í þann tíma sem hann er tengdur (ræðst annaðhvort af dvalarlengd eða þar til hann hefur náð ákveðinni hlutfallsstöðu rafhlöðu)

Skref fyrir ferðamannastað:
1. Farið er í gegnum skref útreiknings fyrir hvern bílaleigubíl sem kemur að ferðamannastað á dag reiknað út frá fjöldi ferðamanna á mánuði skipt jafnt niður á mánuði margfaldað með hlutfalli ferðamanna sem ferðast á einkabíl deilt með fjölda í hverjum bíl
2. Reiknað hversu margir bílaleigubílar þurfa að hlaða samtímis byggt á komutíma og lengd sem þeir vilja vera í sambandi
Monte carlo hermun, þar sem mismunandi dagar eru reiknaðir til að átta sig á hversu mikil sveifla getur verið í fjölda ferðamann sem vill hlaða
