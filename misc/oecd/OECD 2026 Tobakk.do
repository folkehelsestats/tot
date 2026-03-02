/*Beregning av antall sigaretter og andel røykere til OECD for 2025*/

  use "O:\Prosjekt\Rusdata\Rusundersokelsen\Datasets\Rusus_2025\rusus2025_20251126.dta" 


  tab Tob1

/*Fra Tord:
Når det gjelder de andre tallene brukte jeg vekter. Jeg antar riktig vekt i 2024 er «nyvekt2».


Da blir det noe sånt:

use "N:\Tobakksundersøkelsen (datafiler og spørreskjema)\Siste versjon\Med vekter og tilleggsutvalg\Røykevaneundersøkelsen 1973-2024 MED VEKTER & UTVALG.dta", clear*/

drop if kjonn==.

*drop if alder>74
*drop if alder<16


rename Tob1 tob1_hender
rename Tob2 tob2_daglig
rename Tob13 tob4_avogtil_tidligere_daglig 
rename Tob14 tob3_tidligere   

gen roykestatus_2025=0
replace roykestatus_2025=1 if tob1_hender==1 & tob2_daglig==1 
replace roykestatus_2025=2 if tob1_hender==1 & tob2_daglig==2 
replace roykestatus_2025=4 if tob1_hender==2 & tob3_tidligere==1
replace roykestatus_2025=5 if tob1_hender==2 & tob3_tidligere==2
replace roykestatus_2025=3 if tob1_hender==1 & tob2_daglig==2 & tob4_avogtil_tidligere_daglig==1
replace roykestatus_2025=6 if tob1_hender==2 & tob3_tidligere==3

recode roykestatus_2025 (0=8)


label var roykestatus_2025 "Røykestatus, 6-delt (2025)"

label define roykestatus_2025 ///
  1 "Daglig" ///
  2 "Av og til, aldri daglig" ///
  3 "Av og til, tidligere daglig" ///
  4 "Ikke nå, men tidligere daglig" ///
  5 "Ikke nå, men tidligere av og til" ///
  6 "Aldri" ///
  8 "Vil ikke svare" ///
  9 "Vet ikke", replace

label values roykestatus_2025 roykestatus_2025

*Jeg tror dette skal stemme:

gen royk_daglig= roykestatus_2025
recode royk_daglig (1=1)(0 2/6=0)(8/9=.)
tab royk_daglig

rename Tob31a Antsig_daglig

gen antsig_daglig_red= Antsig_daglig                
replace antsig_daglig_red=. if royk_daglig ~=1
replace antsig_daglig_red=. if Antsig_daglig>997
replace antsig_daglig_red=. if Antsig_daglig<1
                               



*Sjekk om dette blir riktig – det er del av en mye større syntax:

gen damp=tobe
replace damp=4 if tobe1==1 | tobe1==2

recode damp (1/2=1)(3/4=0)(8/9=.), gen(damp_regelmessig)

*OECD

//Bruk nyvekt2 i stedet for vekt_SSB_k1234


                            table aar [pweight=vekt_SSB_k1234] , statistic(mean antsig_daglig_red) nformat(%9.3f) 
                            
                            table aar kjonn [pweight=vekt_SSB_k1234], statistic(mean royk_daglig) nformat(%9.3f)

                            table aar kjonn [pweight=vekt_SSB_k1234] if alder<25, statistic(mean royk_daglig) nformat(%9.3f)*/                   
                            
            
	/*Analyser av dagligrøykere til OECD*/		
drop if kjonn==.


 tab1 Tob1 Tob2 Tob3
tab1 Tob13 Tob14

/*1=daglig, 2=AOT, 3=AOT nå, tidligere daglig, 4=røyker ikke nå, men tidligere daglig, 5=røyker ikke nå, ikke røykt tidligere daglig, 6=aldri røykt*/


                                      
gen aar=2025

tab Kjonn

/*antall innbyggere 15 år pluss i 2025 er 4691770*/

tab vekt
describe vekt
destring vekt, replace ignore(", .")

tab vekt
**# Bookmark #2
gen nyvekt2 = (vekt/4691770)*4614


table aar Kjonn [pweight=nyvekt2], statistic(mean antsig_daglig_red) nformat(%9.3f)

      	  
						    table aar [pweight=nyvekt2] , statistic(mean royk_daglig) nformat(%9.3f) 
                            
                            table aar Kjonn [pweight=nyvekt2], statistic(mean royk_daglig) nformat(%9.3f)

**# Bookmark #1
                            table aar Kjonn [pweight=nyvekt2] if Alder<25, statistic(mean royk_daglig) nformat(%9.3f)     
							
							table aar [pweight=nyvekt2] if Alder<25, statistic(mean royk_daglig) nformat(%9.3f)                  


							
						bysort kjonn: tab royk_daglig

						tab royk_daglig if alder<25

tab alder Can9
