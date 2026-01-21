*******************************************************************************

	* Filen importerer og ser på data fra forsyningsundersøkelsen
	* Filen er opprettet av Hdir
	* Opprettet 12/11-2025

********************************************************************************

*** Importerer fil
import spss using "O:\Prosjekt\Rusdata\Forsyningsundersokelsen\Data_Forsyn_2015_2024\FU_veid_2015_2025.sav"

*** Lager tidsvariabler, i tråd med filer fra FHI:

	tostring _v1, generate(datostring)

	gen double dato=date(datostring, "YMD")

	format dato %tdDD-MM-YYYY
	format dato %td

	gen dag = day(dato)
	gen maned = month(dato)
	gen aar = year(dato)


numlabel, add force // tvinger stata til å bruke labels og nummer.

tab aar

*** Lager variabler i tråd med FHI, kjønn og alder:

	rename _v5 alder
	rename Akjonn kjonn

	drop if kjonn==.
	drop if alder>74
	drop if alder<16

	gen alder_4=alder if alder>15 & alder<75
	recode alder_4 (16/24=1)(25/49=2)(50/64=3)(65/74=4)
	lab define alder_4 1 "16-24" 2 "25-49" 3 "50-64" 4 "65-74", replace
	lab val alder_4 alder_4

	gen alder_10=alder if alder>15 & alder<=75
	recode alder_10 (16/25=1)(26/35=2)(36/45=3)(46/55=4)(56/65=5)(66/75=6)
	lab define alder_10 1 "16-25" 2 "26-35" 3 "36-45" 4 "46-55" 5 "56-65" 6 "66-75", replace
	lab val alder_10 alder_10

tab alder_4

*** Lager variabler for røyk, snus og damp i tråd med FHIs koder:

	gen royk=Aq1
	gen snus=Aq2
	gen damp=Aq3
	
	/* OBS, årene 2021 og 2023 har missing på en del på alle spørsmål for noen */
	
	
	* Ulike røykevaner variabler:
	recode Aq1 (1=1)(2/3=2)(4/6=3), 		gen(royk_3) // Daglig; av og til; tidligere/aldri
	recode Aq1 (1=1)(2/3=2)(4/5=3)(6=4), 	gen(royk_4) // Daglig; av og til; tidligere; aldri
	recode Aq1 (1=1)(2/3=2)(4=3)(5=4)(6=5), gen(royk_5) // Daglig; av og til; daglig-tidligere; avogtil-tidligere; aldri
	
	lab define hyppig_3 1 "Daglig" 2 "Av og til" 3 "Tidligere/aldri"
	label values royk_3 hyppig_3
	
	* Ulike snusvaner variabler:
	recode Aq2 (1=1)(2/3=2)(4/6=3), 		gen(snus_3) // Daglig; av og til; tidligere/aldri
	recode Aq2 (1=1)(2/3=2)(4/5=3)(6=4), 	gen(snus_4) // Daglig; av og til; tidligere; aldri
	label values snus_3 hyppig_3
	
	* Ulike dampvaner variabler:
	recode Aq3 (1=1)(2/3=2)(4/6=3), 		gen(damp_3) // Daglig; av og til; tidligere/aldri
	recode Aq3 (1=1)(2/3=2)(4/5=3)(6=4), 	gen(damp_4) // Daglig; av og til; tidligere; aldri
	label values damp_3 hyppig_3
	
	recode Aq3 (1/3=1)(4/6=0), 				gen(damp_regelmessig)	// daglig/avogtil; tidligere/aldri
	recode Aq3 (1=1)(2/6=0), 				gen(damp_daglig)		// daglig; avogtil/tidligere/aldri
	recode Aq3 (2/3=1)(1 4/6=0), 			gen(damp_avogtil)		// avogtil; daglig/tidligere/aldri
	recode Aq3 (4/5=1)(1/3 6=0), 			gen(damp_tidligere)		// tidligere; daglig/avogtil/aldri

	recode Ae1 (2=1)(3=2), 					gen(e_type)				// 
	recode Ae2 (3/4=3)(5=4)(6=5), 			gen(damp_innhold)		//
	

*******************************************************************************
* Om utvalget

* Utvalg (n) per år
tabulate aar if Aq1 !=. // Fordi 2021 + 2023 har en del missing.
bysort aar: tabulate kjonn if Aq1 !=.
bysort aar: tabulate alder_10 if Aq1 !=.

tab aar royk_3, row
tab aar snus_3, row
tab aar damp_3, row

* 2025-tall:
tabulate aar if Aq1 !=. & aar == 2025
tabulate kjonn if Aq1 !=. & aar == 2025
tabulate alder_10 if Aq1 !=. & aar == 2025

tab aar Aq1 if aar == 2025, row

tab aar royk_3 if aar == 2025, row
tab aar snus_3 if aar == 2025, row
tab aar damp_3 if aar == 2025, row

*******************************************************************************
*******************************************************************************
*******************************************************************************

	
	*** Lager variabler som omkoder individer som ikke har svart på kjøpskilde om hvor man har kjøpt røyk og snus siste 24t for personer som røyker/snuser daglig eller av og til, til 0 for å få total N i hver variabl til å refletere alle som er daglig/avogtil røykere/snusere. 
	

		/* ~=1 tilsvarer !=1*/
		
	gen temp_s=0 if royk<=3 								//MERK! Gir 0 for individer som røyker daglig/avogtil. Missing er fortsatt missing.
	
	/* Setter daglig og avogtil røykere til at de ikke har svart på kjøpskilden (0) for alle variabler As11-As19 for å for tot N daglig/avogtil røykere inn når man skal rapportere kjøpskilden for denne gruppen og få fordeling/andel. */
	
		forvalues n=1/9 {									// For variablene om hvor man har kjøpt røyk siste 24t
			replace As1`n'=0 if temp_s==0 & As1`n'~=1 		// endrer til kjøpsvar til 0 hvis man røyker daglig/avogtil og kjøpsvar ikke == 1.
		}

	/* Tilsvarende for snus*/
	gen temp_n=0 if snus<=3 								//MERK!
		forvalues n=1/9 {
			replace An1`n'=0 if temp_n==0 & An1`n'~=1 
		}
		

	elabel variable (As11-As19) ("") // fjerner variabellabel

	elabel variable (An11-An19) ("") // fjerner variabellabel

	numlabel, add force

	
********************************************************************************
** Beregner med kode fra FHI:

	********************************					
	******* Forsyningskilder *******
	********************************				

*** OBS. Koden fra FHI under er gir ikke tabellene som den sier, gir ikke prosent. Må sjekkes nærmere.


	**************************************************
	*Dagligrøykere, andel kjøp ulike steder
	**************************************************

	preserve
	keep if royk==1


		*Vedleggstabell 3.X: Andel som svarte hvor sigaretter røykt siste 24 timer var kjøpt, 2015-2020, dagligrøykere i alderen 16-74

			table aar if As19==0 & (alder>15 & alder<75), statistic(mean As11 As12 As13 As14 As15 As16 As17 As18) //statistic(count As19) // 0= de som ikke har svart at de IKKE har røykt noen sigaretter de siste 24 timene. GJør her MEAN på en kategorisk variabel. 

			*Med vekt
/*
			table aar if As19==0 & (alder>15 & alder<75) [pweight=W1], statistic(mean As11 As12 As13 As14 As15 As16 As17 As18) //statistic(count As19)
			*/
	restore
	
	/*
** Tester, Bente:
	table aar if As19==0 & (alder>15 & alder<75), ///
    statistic(count As11 As12 As13 As14 As15 As16 As17 As18) 
    statistic(count As11)
	
	table aar if As19==0 & (alder>15 & alder<75), ///
    statistic(percent As11 As12 As13 As14 As15 As16 As17 As18) 
	
	
	* Antall som har krysset av på flere kilder til hvor røyken som er røyk siste 24t er kjøpt:
	gen As1_antall = .
	replace As1_antall = (As11+ As12+ As13+ As14+ As15+ As16+ As17+ As18) if As19==0 // legger sammen kun hos de som rapporterer å ha røykt siste 24t.
	tab As1_antall // Viser at de fleste (90%) krysser av på ett alternativ, men noen krysser av på flere (har flere kilder til røyekn røkt siste 24 t)
	
	*/
	
********************************************************************************
* FHIs kategorier:
********************************************************************************
/* Usikker på hva jeg prøver på her

keep if royk==1

egen nor = rowtotal(As21 As27)
gen sve = As24
gen den = As25 
gen taxfree = As22
gen utland = As26
egen nettpostannet = rowtotal(As23 As28)
egen total = rowtotal(nor sve den taxfree utland nettpostannet)

** Antall sig fra de ulike kildene per år: 
	bysort aar: egen roykkjopt_nor=sum(nor) if alder>15 & alder<75
	bysort aar: egen roykkjopt_sve=sum(sve) if alder>15 & alder<75
	bysort aar: egen roykkjopt_dan=sum(den) if alder>15 & alder<75
	bysort aar: egen roykkjopt_tax=sum(taxfree) if alder>15 & alder<75
	bysort aar: egen roykkjopt_utl=sum(utland) if alder>15 & alder<75
	bysort aar: egen roykkjopt_inu=sum(nettpostannet) if alder>15 & alder<75
	egen roykkjopt_tot = rowtotal(roykkjopt_nor roykkjopt_sve roykkjopt_dan roykkjopt_tax roykkjopt_utl roykkjopt_inu) if alder>15 & alder<75
	
	
	
	table aar if alder>15 & alder<75, statistic(mean roykkjopt_nor roykkjopt_tax roykkjopt_sve roykkjopt_dan roykkjopt_utl roykkjopt_inu)
	*/
	
**************************************************
	*Dagligrøykere, andel antall DENNE!
	**************************************************
preserve
keep if royk==1
	** Regner ut totalt antall sigaretter kjøpt fra de ulike kildene per år:
		bysort aar: egen roykkjopt_nor=sum(As21) if alder>15 & alder<75
		bysort aar: egen roykkjopt_tax=sum(As22) if alder>15 & alder<75
		bysort aar: egen roykkjopt_smu=sum(As23) if alder>15 & alder<75
		bysort aar: egen roykkjopt_sve=sum(As24) if alder>15 & alder<75
		bysort aar: egen roykkjopt_dan=sum(As25) if alder>15 & alder<75
		bysort aar: egen roykkjopt_utl=sum(As26) if alder>15 & alder<75
		bysort aar: egen roykkjopt_inn=sum(As27) if alder>15 & alder<75
		bysort aar: egen roykkjopt_inu=sum(As28) if alder>15 & alder<75

		gen n_royk_nor =As21
		replace n_royk_nor=1 if As21>0&As21<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_tax =As22
		replace n_royk_tax=1 if As22>0&As22<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_smu =As23
		replace n_royk_smu=1 if As23>0&As23<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_sve =As24
		replace n_royk_sve=1 if As24>0&As24<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_dan =As25
		replace n_royk_dan=1 if As25>0&As25<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_utl =As26
		replace n_royk_utl=1 if As26>0&As26<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_inn =As27
		replace n_royk_inn=1 if As27>0&As27<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_inu =As28
		replace n_royk_inu=1 if As28>0&As28<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk=1 if n_royk_nor==1 | n_royk_tax==1 | n_royk_smu==1 | n_royk_sve==1 | n_royk_dan==1 | n_royk_utl==1 | n_royk_inn==1 | n_royk_inu==1 	// Varibel som markerer individer som har kjøpt minst 1 røyk fra en av de ulike kildene (As21-As28).

		*Prosentandel av siste døgns sigarettforbruk blant dagligrøykere som ble kjøpt i Norge og i utlandet, 2015-2023, personer i alderen 16-74

			table aar if alder>15 & alder<75, statistic(mean roykkjopt_nor roykkjopt_tax roykkjopt_smu roykkjopt_sve roykkjopt_dan roykkjopt_utl roykkjopt_inn roykkjopt_inu)
			table aar if alder>15 & alder<75 , statistic(sum n_royk)
/* Tabellen rett over gir antall sigaretter kjøpt per år fra de ulike kildene, som videre kan benyttes til å beregne fordeling av hvor sigaretter røykt siste 24t er kjøpt - se excelfil.*/
			
			*Med vekt
		
			table aar if alder>15 & alder<75 [pweight=W1], statistic(mean roykkjopt_nor roykkjopt_tax roykkjopt_smu roykkjopt_sve roykkjopt_dan roykkjopt_utl roykkjopt_inn roykkjopt_inu)
			table aar if alder>15 & alder<75 [pweight=W1], statistic(sum n_royk)

			
restore		


*** Etter alder:
preserve
keep if royk==1
count
	** Regner ut totalt antall sigaretter kjøpt fra de ulike kildene per år:
		bysort aar alder_4: egen roykkjopt_nor=sum(As21) if alder>15 & alder<75
		bysort aar alder_4: egen roykkjopt_tax=sum(As22) if alder>15 & alder<75
		bysort aar alder_4: egen roykkjopt_smu=sum(As23) if alder>15 & alder<75
		bysort aar alder_4: egen roykkjopt_sve=sum(As24) if alder>15 & alder<75
		bysort aar alder_4: egen roykkjopt_dan=sum(As25) if alder>15 & alder<75
		bysort aar alder_4: egen roykkjopt_utl=sum(As26) if alder>15 & alder<75
		bysort aar alder_4: egen roykkjopt_inn=sum(As27) if alder>15 & alder<75
		bysort aar alder_4: egen roykkjopt_inu=sum(As28) if alder>15 & alder<75

		gen n_royk_nor =As21
		replace n_royk_nor=1 if As21>0&As21<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_tax =As22
		replace n_royk_tax=1 if As22>0&As22<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_smu =As23
		replace n_royk_smu=1 if As23>0&As23<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_sve =As24
		replace n_royk_sve=1 if As24>0&As24<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_dan =As25
		replace n_royk_dan=1 if As25>0&As25<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_utl =As26
		replace n_royk_utl=1 if As26>0&As26<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_inn =As27
		replace n_royk_inn=1 if As27>0&As27<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk_inu =As28
		replace n_royk_inu=1 if As28>0&As28<. // Variabel som markerer indiver som har kjøpt minst 1 røyk.

		gen n_royk=1 if n_royk_nor==1 | n_royk_tax==1 | n_royk_smu==1 | n_royk_sve==1 | n_royk_dan==1 | n_royk_utl==1 | n_royk_inn==1 | n_royk_inu==1 	// Varibel som markerer individer som har kjøpt minst 1 røyk fra en av de ulike kildene (As21-As28).


	* By age:
			table aar alder_4 if alder>15 & alder<75, statistic(mean roykkjopt_nor roykkjopt_tax roykkjopt_smu roykkjopt_sve roykkjopt_dan roykkjopt_utl roykkjopt_inn roykkjopt_inu)
			table aar alder_4 if alder>15 & alder<75 , statistic(sum n_royk)
			
restore


****

** Snus:
preserve
keep if snus == 1

** Regner ut totalt antall sigaretter kjøpt fra de ulike kildene per år:
		bysort aar: egen snuskjopt_nor=sum(An21) if alder>15 & alder<75
		bysort aar: egen snuskjopt_tax=sum(An22) if alder>15 & alder<75
		bysort aar: egen snuskjopt_smu=sum(An23) if alder>15 & alder<75
		bysort aar: egen snuskjopt_sve=sum(An24) if alder>15 & alder<75
		bysort aar: egen snuskjopt_dan=sum(An25) if alder>15 & alder<75
		bysort aar: egen snuskjopt_utl=sum(An26) if alder>15 & alder<75
		bysort aar: egen snuskjopt_inn=sum(An27) if alder>15 & alder<75
		bysort aar: egen snuskjopt_inu=sum(An28) if alder>15 & alder<75

		gen n_snus_nor =An21
		replace n_snus_nor=1 if An21>0&An21<. // Variabel som markerer indiver som har kjøpt minst 1 snu.

		gen n_snus_tax =An22
		replace n_snus_tax=1 if An22>0&An22<. // Variabel som markerer indiver som har kjøpt minst 1 snus.

		gen n_snus_smu =An23
		replace n_snus_smu=1 if An23>0&An23<. // Variabel som markerer indiver som har kjøpt minst 1 snus.

		gen n_snus_sve =An24
		replace n_snus_sve=1 if An24>0&An24<. // Variabel som markerer indiver som har kjøpt minst 1 snus.

		gen n_snus_dan =An25
		replace n_snus_dan=1 if An25>0&An25<. // Variabel som markerer indiver som har kjøpt minst 1 snus.

		gen n_snus_utl =An26
		replace n_snus_utl=1 if An26>0&An26<. // Variabel som markerer indiver som har kjøpt minst 1 snus.

		gen n_snus_inn =An27
		replace n_snus_inn=1 if An27>0&An27<. // Variabel som markerer indiver som har kjøpt minst 1 snus.

		gen n_snus_inu =An28
		replace n_snus_inu=1 if An28>0&An28<. // Variabel som markerer indiver som har kjøpt minst 1 snus.

		gen n_snus=1 if n_snus_nor==1 | n_snus_tax==1 | n_snus_smu==1 | n_snus_sve==1 | n_snus_dan==1 | n_snus_utl==1 | n_snus_inn==1 | n_snus_inu==1 	// Varibel som markerer individer som har kjøpt minst 1 snus fra en av de ulike kildene (An21-An28).

		*Prosentandel av siste døgns sigarettforbruk blant dagligsnusere som ble kjøpt i Norge og i utlandet, 2015-2025, personer i alderen 16-74

			table aar if alder>15 & alder<75, statistic(mean snuskjopt_nor snuskjopt_tax snuskjopt_smu snuskjopt_sve snuskjopt_dan snuskjopt_utl snuskjopt_inn snuskjopt_inu)
			table aar if alder>15 & alder<75 , statistic(sum n_snus)
/* Tabellen rett over gir antall snus kjøpt per år fra de ulike kildene, som videre kan benyttes til å beregne fordeling av hvor sigaretter røykt siste 24t er kjøpt - se excelfil.*/
			
restore		
			
			


********************************************************************************
* E-sigaretter:
preserve

keep if Aq3==1
drop if Ae2 == 1


****

/*

** BENTE FORTSETT HER: 
	/* Denne utregningen gir samme som EXCEL-fil*/
	
gen roykkjopt_tot_test = .
replace roykkjopt_tot_test = (roykkjopt_nor + roykkjopt_tax + roykkjopt_smu + roykkjopt_sve + roykkjopt_dan + roykkjopt_utl + roykkjopt_inn + roykkjopt_inu) if aar == 2015 & As19==0
replace roykkjopt_tot_test = (roykkjopt_nor + roykkjopt_tax + roykkjopt_smu + roykkjopt_sve + roykkjopt_dan + roykkjopt_utl + roykkjopt_inn + roykkjopt_inu) if aar == 2016 & As19==0
replace roykkjopt_tot_test = (roykkjopt_nor + roykkjopt_tax + roykkjopt_smu + roykkjopt_sve + roykkjopt_dan + roykkjopt_utl + roykkjopt_inn + roykkjopt_inu) if aar == 2017 & As19==0
replace roykkjopt_tot_test = (roykkjopt_nor + roykkjopt_tax + roykkjopt_smu + roykkjopt_sve + roykkjopt_dan + roykkjopt_utl + roykkjopt_inn + roykkjopt_inu) if aar == 2018 & As19==0
replace roykkjopt_tot_test = (roykkjopt_nor + roykkjopt_tax + roykkjopt_smu + roykkjopt_sve + roykkjopt_dan + roykkjopt_utl + roykkjopt_inn + roykkjopt_inu) if aar == 2019 & As19==0
replace roykkjopt_tot_test = (roykkjopt_nor + roykkjopt_tax + roykkjopt_smu + roykkjopt_sve + roykkjopt_dan + roykkjopt_utl + roykkjopt_inn + roykkjopt_inu) if aar == 2020 & As19==0
replace roykkjopt_tot_test = (roykkjopt_nor + roykkjopt_tax + roykkjopt_smu + roykkjopt_sve + roykkjopt_dan + roykkjopt_utl + roykkjopt_inn + roykkjopt_inu) if aar == 2021 & As19==0
replace roykkjopt_tot_test = (roykkjopt_nor + roykkjopt_tax + roykkjopt_smu + roykkjopt_sve + roykkjopt_dan + roykkjopt_utl + roykkjopt_inn + roykkjopt_inu) if aar == 2022 & As19==0
replace roykkjopt_tot_test = (roykkjopt_nor + roykkjopt_tax + roykkjopt_smu + roykkjopt_sve + roykkjopt_dan + roykkjopt_utl + roykkjopt_inn + roykkjopt_inu) if aar == 2023 & As19==0
replace roykkjopt_tot_test = (roykkjopt_nor + roykkjopt_tax + roykkjopt_smu + roykkjopt_sve + roykkjopt_dan + roykkjopt_utl + roykkjopt_inn + roykkjopt_inu) if aar == 2024 & As19==0
replace roykkjopt_tot_test = (roykkjopt_nor + roykkjopt_tax + roykkjopt_smu + roykkjopt_sve + roykkjopt_dan + roykkjopt_utl + roykkjopt_inn + roykkjopt_inu) if aar == 2025 & As19==0

gen andel_royk_nor = (roykkjopt_nor/roykkjopt_tot_test) if As19==0
gen andel_royk_tax = (roykkjopt_tax/roykkjopt_tot_test) if As19==0
gen andel_royk_smu = (roykkjopt_smu/roykkjopt_tot_test) if As19==0
gen andel_royk_sve = (roykkjopt_sve/roykkjopt_tot_test) if As19==0
gen andel_royk_dan = (roykkjopt_dan/roykkjopt_tot_test) if As19==0
gen andel_royk_utl = (roykkjopt_utl/roykkjopt_tot_test) if As19==0
gen andel_royk_inn = (roykkjopt_inn/roykkjopt_tot_test) if As19==0
gen andel_royk_inu = (roykkjopt_inu/roykkjopt_tot_test) if As19==0


table aar if alder>15 & alder<75 & As19==0, statistic(mean andel_royk_nor andel_royk_tax andel_royk_smu andel_royk_sve andel_royk_dan andel_royk_utl andel_royk_inn andel_royk_inu)

/* Vektings-tallene er helt like den rett over*/
table aar if alder>15 & alder<75 & As19==0 [pweight=W1], statistic(mean andel_royk_nor andel_royk_tax andel_royk_smu andel_royk_sve andel_royk_dan andel_royk_utl andel_royk_inn andel_royk_inu)


*******************
	*** Bente, beregner andel sig fra ulike kilder av røyk siste 24t

* 1. Antall dagligrøykere er allerede valgt gjennom keep if royk == 1
keep if royk==1
* 2. Beregner totalt antall sigaretter siste døgn: 
egen n_royk_tot = rowtotal(As21 As22 As23 As24 As25 As26 As27 As28) if As19==0 // rowtotal pga missing
* 3. andel per kilde for hvert individ:
gen andel_royk_nor = (As21/n_royk_tot) if As19==0
gen andel_royk_tax = (As22/n_royk_tot) if As19==0
gen andel_royk_smu = (As23/n_royk_tot) if As19==0
gen andel_royk_sve = (As24/n_royk_tot) if As19==0
gen andel_royk_dan = (As25/n_royk_tot) if As19==0
gen andel_royk_utl = (As26/n_royk_tot) if As19==0
gen andel_royk_inn = (As27/n_royk_tot) if As19==0
gen andel_royk_inu = (As28/n_royk_tot) if As19==0

egen andel_royk = rowtotal(andel_royk_nor andel_royk_tax andel_royk_smu andel_royk_sve andel_royk_dan andel_royk_utl andel_royk_inn) if As19==0 

/*
foreach varlist andel_royk_nor andel_royk_tax andel_royk_smu andel_royk_sve andel_royk_dan andel_royk_utl andel_royk_inn andel_royk_inu {
    collapse (mean) `k' [aw=W1], by(aar)
}
	*/
	collapse (mean) andel_royk_nor andel_royk_tax andel_royk_smu andel_royk_sve andel_royk_dan andel_royk_utl andel_royk_inn andel_royk_inu [aw=W1], by(aar)
	
	collapse (mean) andel_royk_nor andel_royk_tax andel_royk_smu andel_royk_sve andel_royk_dan andel_royk_utl andel_royk_inn andel_royk_inu [aw=W1], by(aar)
			
/*
** Bente tester, dette er mer riktig. 

collapse (count) RespondentID (sum) As11 As12 As13 As14 As15 As16 As17 As18 As19 As21 As22 As23 As24 As25 As26 As27 As28, by(aar)

gen n_royk = (As21+ As22+ As23+ As24+ As25+ As26+ As27+ As28)

	
gen n_royk_nor = (As21/n_royk)*100




