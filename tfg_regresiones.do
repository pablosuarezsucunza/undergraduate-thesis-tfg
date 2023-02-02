clear all
use "tfg_datos.dta" 


gen lattendance = log(attendance) if attendance>0
replace lattendance=0 if attendance==0

gen lcoachexp = log(coachexp) if coachexp>0
replace lcoachexp=0 if coachexp==0

gen scoachexp = coachexp^2


************DEPENDENT VARIABLE**********************************************
*EPA
gen ldegree_epa = log(abs(degree_epa)*100) if degree_epa!=0
replace ldegree_epa = - ldegree_epa   if degree_epa<0
replace ldegree_epa=0 if degree_epa==0

*WPA
gen ldegree_wpa = log(abs(degree_wpa)*100) if degree_wpa!=0
replace ldegree_wpa = - ldegree_wpa   if degree_wpa<0
replace ldegree_wpa=0 if degree_wpa==0


************************************************************************************
*************REGRESSIONS************************************************************
*EPA
reghdfe ldegree_epa coachexp lattendance home regseason , absorb(season coach right_play_epa down) cluster(season)
reghdfe ldegree_epa coachexp home, absorb(game_id coach right_play_epa ) cluster(season)



*WPA
reghdfe ldegree_wpa coachexp lattendance home regseason, absorb(season coach right_play_wpa down) cluster(season)
reghdfe ldegree_wpa coachexp home, absorb(game_id coach wins right_play_wpa player_id) cluster(season)



*************************************************************************************
*HETEROGENEITY TEST*******************************************************************
	*Pass vs run  SHOULD
levelsof right_play_epa, local(groups)
foreach group of local groups {
reghdfe ldegree_epa coachexp home  if right_play_epa==`group', absorb(game_id coach down) cluster(season)
}



	*playofffs vs regular season
levelsof season_type, local(groups)
foreach group of local groups {
reghdfe ldegree_wpa coachexp home  if season_type==`group', absorb(game_id coach right_play_epa) cluster(season)
}

	*home vs. away
levelsof home, local(groups)
foreach group of local groups {
reghdfe ldegree_epa coachexp  if home==`group', absorb(season season_type coach wins) cluster(season)

}

		

*******************************************************************************************************************
*******************************************************************************************************************
*OUTPUT TABLES and ALL
*******************************************************************************************************************
*******************************************************************************************************************
lab var coachexp "coach experience"
lab var play_type "play type"
lab var season_type "season type"
lab var right_play_epa "right choice (epa)"
lab var right_play_wpa "right choice (wpa)"
lab var regseason "regular season"
lab var lattendance "log(attendance)"
lab var degree_epa "Degree (epa)"
lab var degree_wpa "Degree (wpa)"
lab var ldegree_epa "log(degree epa)"
lab var ldegree_wpa "log(degree wpa)"
lab var scoachexp "coach experience \textasciicircum 2"




****************************************
*Summary stats table********************
est clear 
estpost sum degree_epa degree_wpa coachexp attendance season home regseason right_play_epa right_play_wpa,
esttab using "sumtable1.tex", replace /// 
   cells("mean sd min max count") ///
   title(Summary statistics \label{sumtable}) ///
   addnotes("") ///
   nonote noobs label collabels("Mean" "SD" "Min" "Max" "N")







***************************************
*regressions output tables*************************

 
*epa and wpa*****
est clear  
eststo: reghdfe ldegree_epa coachexp home, absorb(game_id coach right_play_epa ) cluster(season) 
 estadd local CFE "Yes"
 estadd local GFE "Yes"
 estadd local WIFE "Yes"
 estadd local RPFE "Yes"
eststo: reghdfe ldegree_epa coachexp home, absorb(season right_play_epa coach ) cluster(season)
 estadd local CFE "Yes"
 estadd local SFE "Yes"
 estadd local WIFE "Yes"
 estadd local RPFE "Yes"
eststo: reghdfe ldegree_epa coachexp lattendance home regseason, absorb(season right_play_epa coach ) cluster(season)
 estadd local CFE "Yes"
 estadd local SFE "Yes"
 estadd local WIFE "Yes"
 estadd local RPFE "Yes"
eststo: reghdfe ldegree_wpa coachexp home, absorb(game_id coach right_play_wpa ) cluster(season) 
 estadd local CFE "Yes"
 estadd local GFE "Yes"
 estadd local WIFE "Yes"
 estadd local RPFE "Yes"
eststo: reghdfe ldegree_wpa coachexp home, absorb(season right_play_wpa coach ) cluster(season)
 estadd local CFE "Yes"
 estadd local SFE "Yes"
 estadd local WIFE "Yes"
 estadd local RPFE "Yes"
eststo: reghdfe ldegree_wpa coachexp lattendance home regseason, absorb(season right_play_wpa coach ) cluster(season)
 estadd local CFE "Yes"
 estadd local SFE "Yes"
 estadd local WIFE "Yes"
 estadd local RPFE "Yes"
 
esttab using "reg epa and wpa.tex", replace  ///
 b(3) se(3) r2(4) nomtitle nocons label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title(Impact of coach experience on degree \label{regewpa})  ///
 scalars("CFE Coach FE"  "RPFE Right choice FE" "GFE Game FE" "SFE Season FE" ) ///
 mgroups("Degree EPA" "Degree WPA", pattern(1 0 0 1 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))


 *output for poster
esttab using "reg poster.tex", replace  ///
 b(3) se(3) r2(4) nomtitle nocons label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title(Impact of coach experience on degree \label{regewpa})  ///
 scalars("CFE Coach FE"  "RPFE Right choice FE" "GFE Game FE" "SFE Season FE" ) ///
 mgroups("Degree EPA" "Degree WPA", pattern(1 0  1  0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))



 
 
*******************************************************************
*regressions for heterogeeity output tables*************************
 
*regular season vs. playoffs
est clear
		*epa regressions
levelsof season_type, local(groups) 
foreach group of local groups {
qui eststo: reghdfe ldegree_epa coachexp home  if season_type==`group', absorb(game_id coach right_play_epa ) cluster(season)
 estadd local CFE "Yes"
 estadd local GFE "Yes"
 estadd local WIFE "Yes"
 estadd local RCFE "Yes"
qui eststo: reghdfe ldegree_epa coachexp lattendance home if season_type==`group', absorb(season coach right_play_epa ) cluster(season)
 estadd local CFE "Yes"
 estadd local SFE "Yes"
 estadd local WIFE "Yes"
 estadd local RCFE "Yes"
} 
		*wpa regressions
levelsof season_type, local(groups) 
foreach group of local groups {
qui eststo: reghdfe ldegree_wpa coachexp home  if season_type==`group', absorb(game_id coach right_play_wpa ) cluster(season)
 estadd local CFE "Yes"
 estadd local GFE "Yes"
 estadd local WIFE "Yes"
 estadd local RCFE "Yes"
qui eststo: reghdfe ldegree_wpa coachexp lattendance home if season_type==`group', absorb(season coach right_play_wpa ) cluster(season)
 estadd local CFE "Yes"
 estadd local SFE "Yes"
 estadd local WIFE "Yes"
 estadd local RCFE "Yes"
} 


esttab using "het_seatype.tex", replace  ///
 b(3) se(3) r2(4) nocons nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title(Impact of coach experience on degree: \\ heterogeneity by regular season and playoffs \label{hetseatype})   ///
 addnotes("") ///
 scalars("CFE Coach FE" "RCFE Right choice FE" "GFE Game FE" "SFE Season FE"  ) ///
 mgroups("Playoffs (EPA)" "Regular season (EPA)" "Playoffs (WPA)" "Regular season (WPA)", pattern(1 0 1 0 1 0 1 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))


 
 
 
 
*right choice: pass vs run

est clear
		*EPA
levelsof right_play_epa, local(groups)
foreach group of local groups {
eststo: reghdfe ldegree_epa coachexp home  if right_play_epa==`group', absorb(game_id coach) cluster(season)
 estadd local CFE "Yes"
 estadd local GFE "Yes"
 
eststo: reghdfe ldegree_epa coachexp lattendance home regseason if right_play_epa==`group', absorb(season coach) cluster(season)
 estadd local CFE "Yes"
 estadd local SFE "Yes"

} 
		*WPA
levelsof right_play_wpa, local(groups)
foreach group of local groups {
eststo: reghdfe ldegree_wpa coachexp home  if right_play_wpa==`group', absorb(game_id coach) cluster(season)
 estadd local CFE "Yes"
 estadd local GFE "Yes"

eststo: reghdfe ldegree_wpa coachexp lattendance home regseason if right_play_wpa==`group', absorb(season coach) cluster(season)
 estadd local CFE "Yes"
 estadd local SFE "Yes"
} 

esttab using "het_rightplay.tex", replace  ///
 b(3) se(3) r2(4) nocons nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title(Impact of coach experince on degree: \\ heterogeinity by optimal option \label{hetrightplay})   ///
 addnotes("") ///
 scalars("CFE Coach FE" "GFE Game FE" "SFE Season FE" ) ///
 mgroups("Pass (EPA)" "Run (EPA)" "Pass (WPA)" "Run (WPA)", pattern(1 0 1 0 1 0 1 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))



 
 
 
 
 ****************************************************************************************************
 *** APPENDIX****************************************************************************************
 
 
 *Apendix A********************
est clear
eststo: reghdfe ldegree_epa coachexp scoachexp home, absorb(game_id coach  right_play_epa) cluster(season)
  estadd local CFE "Yes"
  estadd local GFE "Yes"
  estadd local RPFE "Yes"
eststo: reghdfe ldegree_epa coachexp scoachexp home, absorb(season coach right_play_epa) cluster(season)
  estadd local CFE "Yes"
  estadd local SFE "Yes"
  estadd local RPFE "Yes"
eststo: reghdfe ldegree_epa coachexp scoachexp lattendance home regseason, absorb(season coach right_play_epa) cluster(season)
  estadd local CFE "Yes"
  estadd local SFE "Yes"
  estadd local RPFE "Yes"
eststo: reghdfe ldegree_wpa coachexp scoachexp home, absorb(game_id coach right_play_wpa) cluster(season)
  estadd local CFE "Yes"
  estadd local GFE "Yes"
  estadd local RPFE "Yes"
eststo: reghdfe ldegree_wpa coachexp scoachexp home, absorb(season coach right_play_wpa) cluster(season)
  estadd local CFE "Yes"
  estadd local SFE "Yes"
  estadd local RPFE "Yes"
eststo: reghdfe ldegree_wpa coachexp scoachexp lattendance home regseason, absorb(season coach right_play_wpa) cluster(season)
  estadd local CFE "Yes"
  estadd local SFE "Yes"
  estadd local RPFE "Yes"
  
esttab using "reg expsqrd.tex", replace  ///
 b(4) se(3) r2(4) nocons nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs ///
 title(Impact of coach experience on degree: non-linear effects \label{regsqrd})   ///
 addnotes("") ///
 scalars("CFE Coach FE" "RPFE Right choice FE" "GFE Game FE" "SFE Season FE") ///
 mgroups("Degree EPA" "Degree WPA", pattern(1 0 0 1 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))



*Appendix B***********************************************

est clear  
eststo: reghdfe ldegree_epa coachexp home, absorb(game_id coach right_play_epa down ) cluster(season) 
 estadd local CFE "Yes"
 estadd local GFE "Yes"
 estadd local RPFE "Yes"
 estadd local DFE "Yes"
eststo: reghdfe ldegree_epa coachexp home, absorb(season right_play_epa coach down ) cluster(season)
 estadd local CFE "Yes"
 estadd local SFE "Yes"
 estadd local RPFE "Yes"
 estadd local DFE "Yes"
eststo: reghdfe ldegree_epa coachexp lattendance home regseason, absorb(season right_play_epa coach down ) cluster(season)
 estadd local CFE "Yes"
 estadd local SFE "Yes"
 estadd local RPFE "Yes"
 estadd local DFE "Yes" 
eststo: reghdfe ldegree_wpa coachexp home, absorb(game_id coach right_play_wpa down ) cluster(season) 
 estadd local CFE "Yes"
 estadd local GFE "Yes"
 estadd local RPFE "Yes"
 estadd local DFE "Yes"
eststo: reghdfe ldegree_wpa coachexp home, absorb(season right_play_wpa coach down ) cluster(season)
 estadd local CFE "Yes"
 estadd local SFE "Yes"
 estadd local RPFE "Yes"
 estadd local DFE "Yes"
eststo: reghdfe ldegree_wpa coachexp lattendance home regseason, absorb(season right_play_wpa coach down ) cluster(season)
 estadd local CFE "Yes"
 estadd local SFE "Yes"
 estadd local RPFE "Yes"
 estadd local DFE "Yes"
 
esttab using "reg apendix B.tex", replace  ///
 b(3) se(3) r2(4) nomtitle nocons label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title(Impact of coach experience on degree: including Down fixed effects \label{regappb})  ///
 scalars("CFE Coach FE"  "RPFE Right choice FE" "GFE Game FE" "SFE Season FE" "DFE Down FE") ///
 mgroups("Degree EPA" "Degree WPA", pattern(1 0 0 1 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))

 
