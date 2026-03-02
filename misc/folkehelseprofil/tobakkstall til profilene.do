*bruk rusundersøkelsen

*5-års aldersgrupper

recode Alder (15/19=1)(20/24=2)(25/29=3)(30=34=4)(35/39=5)(40/44=6)(45/49=7)(50/54=8)(55/59=9)(60/64=10)(65/69=11)(70/74=12)(75/80=13)



*use "H:\Documents and Settings\tfv\Mine Dokumenter\Prosjekter\Undersøkelser\Røykevaneundersøkelsene\Stata\Merge\Røykevaneundersøkelsen 1973-2019_red.dta", clear

*use "N:\Tobakksundersøkelsen (datafiler og spørreskjema)\Siste versjon\Røykevaneundersøkelsen 1973-2021.dta", clear

*use "N:\Tobakksundersøkelsen (datafiler og spørreskjema)\Siste versjon\Røykevaneundersøkelsen 1973-2023.dta", clear

*snapshot erase _all
*snapshot save 

*snapshot restore 1

*drop if aar==1997 & kvartal==1
*drop if aar==1998 & kvartal==3
*drop if aar==2000 & kvartal==2
*drop if aar==2002 & kvartal==1
*drop if aar==2016 & kvartal==2


*drop royk_daglig royk_avogtil
*drop snus_daglig snus_avogtil snus_aldri

recode royk_mfm_1973 (8/9=.) //n=313
*recode royk_mfm_1973 (8/9=3) //n=313 

*tab royk_mfm_1973, gen(royk)
*rename royk1 royk_daglig
*rename royk2 royk_avogtil
*rename royk3 royk_tidligere_aldri

recode Tob2 (1=1) (2=2) (8/9=.) (.=3), gen (royktot) 
recode Tob60 (1=1) (2=2) (3=3) (8/9=.), gen (snustot)

tab royktot, gen(royk)
rename royk1 royk_daglig
rename royk2 royk_avogtil
rename royk3 royk_tidligere_aldri

tab snustot, gen(snus)
rename snus1 snus_daglig
rename snus2 snus_avogtil
rename snus3 snus_tidligere_aldri

*tab snus_1985, gen(snus)
*rename snus1 snus_daglig
*rename snus2 snus_avogtil
*rename snus3 snus_tidligere_aldri




*recode alder (16/20=1)(21/25=2)(26/30=3)(31/35=4)(36/40=5)(41/45=6)(46/50=7)(51/55=8)(56/60=9)(61/65=10)(66/70=11)(71/75=12)(76/80=13), gen(alder_5)

recode Alder (15/19=1)(20/24=2)(25/29=3)(30/34=4)(35/39=5)(40/44=6)(45/49=7)(50/54=8)(55/59=9)(60/64=10)(65/69=11)(70/74=12)(75/80=13), gen(alder_5)
lab define alder_5 1 "15-19" 2 "20-24" 3 "25-29" 4 "30-34" 5 "35-39" 6 "40-44" 7 "45-49" 8 "50-54" 9 "55-59" 10 "60-64" 11 "65-69" 12 "70-74" 13 "75-80", replace
lab values alder_5 alder_5

drop if alder>74
drop if kjonn==.

*recode utdanning_mfm (.=97)

*snapshot save

keep royk_daglig royk_avogtil royk_tidligere_aldri snus_daglig snus_avogtil snus_tidligere_aldri Kjonn alder_5  Utdann_4gr

collapse (sum) royk_daglig royk_avogtil royk_tidligere_aldri snus_daglig snus_avogtil snus_tidligere_aldri  kjonn alder_5  Utdann_4gr


*keep royk_daglig royk_avogtil royk_tidligere_aldri snus_daglig snus_avogtil snus_tidligere_aldri  kjonn alder_5 kvartal aar utdanning_mfm

*collapse (sum) royk_daglig royk_avogtil royk_tidligere_aldri snus_daglig snus_avogtil snus_tidligere_aldri , by(kjonn alder_5 aar utdanning_mfm)


*replace snus_tidligere=. if aar<2003


lab var alder_5 "Alder 5-delt"

lab var royk_daglig "Røyker daglig"
lab var royk_avogtil "Røyker av og til"
lab var royk_tidligere_aldri "Røykte tidligere eller aldri"
lab var snus_daglig "Bruker snus daglig"
lab var snus_avogtil "Bruker snus av og til"
lab var snus_tidligere_aldri "Brukte snus tidligere eller aldri"
lab var utdanning_mfm "Utdanning"


bysort aar : egen antall_royk=total(royk_daglig + royk_avogtil + royk_tidligere_aldri)
bysort aar : egen antall_snus=total(snus_daglig + snus_avogtil + snus_tidligere_aldri)

sort  aar  kjonn  utdanning_mfm alder 


snapshot restore 1

*drop if aar==1997 & kvartal==1
*drop if aar==1998 & kvartal==3
*drop if aar==2000 & kvartal==2
*drop if aar==2002 & kvartal==1
*drop if aar==2016 & kvartal==2


drop royk_daglig royk_avogtil
drop snus_daglig snus_avogtil snus_aldri

recode royk_mfm_1973 (8/9=.) //n=313
*recode royk_mfm_1973 (8/9=3) //n=313 

tab royk_mfm_1973, gen(royk)
rename royk1 royk_daglig
rename royk2 royk_avogtil
rename royk3 royk_tidligere_aldri


tab snus_1985, gen(snus)
rename snus1 snus_daglig
rename snus2 snus_avogtil
rename snus3 snus_tidligere_aldri


*recode alder (16/20=1)(21/25=2)(26/30=3)(31/35=4)(36/40=5)(41/45=6)(46/50=7)(51/55=8)(56/60=9)(61/65=10)(66/70=11)(71/75=12)(76/80=13), gen(alder_5)

recode alder (15/19=1)(20/24=2)(25/29=3)(30/34=4)(35/39=5)(40/44=6)(45/49=7)(50/54=8)(55/59=9)(60/64=10)(65/69=11)(70/74=12)(75/80=13), gen(alder_5)
lab define alder_5 1 "15-19" 2 "20-24" 3 "25-29" 4 "30-34" 5 "35-39" 6 "40-44" 7 "45-49" 8 "50-54" 9 "55-59" 10 "60-64" 11 "65-69" 12 "70-74" 13 "75-80", replace
lab values alder_5 alder_5

*drop if alder>79
*drop if kjonn==.
keep if aar>1996

*recode utdanning_mfm (9=.)


keep royk_daglig royk_avogtil royk_tidligere_aldri snus_daglig snus_avogtil snus_tidligere_aldri Kjonn alder_5  Utdann_4gr fylke_2019



tab1 royk_daglig royk_avogtil royk_tidligere_aldri snus_daglig snus_avogtil snus_tidligere_aldri  Kjonn alder_5 Utdann_4gr fylke_2019


collapse (sum) royk_daglig royk_avogtil royk_tidligere_aldri snus_daglig snus_avogtil snus_tidligere_aldri , by(Kjonn alder_5 Utdann_4gr fylke_2019)

*replace snus_tidligere=. if aar<2003



lab var alder_5 "Alder 5-delt"

lab var royk_daglig "Røyker daglig"
lab var royk_avogtil "Røyker av og til"
lab var royk_tidligere_aldri "Røykte tidligere eller aldri"
lab var snus_daglig "Bruker snus daglig"
lab var snus_avogtil "Bruker snus av og til"
lab var snus_tidligere_aldri "Brukte snus tidligere eller aldri"
lab var utdanning_mfm "Utdanning"
lab var fylke_mfm "Fylke"

bysort aar : egen antall_royk=total(royk_daglig + royk_avogtil + royk_tidligere_aldri)
bysort aar : egen antall_snus=total(snus_daglig + snus_avogtil + snus_tidligere_aldri)

sort  aar kjonn fylke_mfm  utdanning_mfm alder 

