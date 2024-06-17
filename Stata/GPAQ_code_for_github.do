** WHO GPAQ Processing Code
** Originated from Regina Guthold 2018 Comparable Estiamtes
** Used by Tessa Strain/Gretchen Stevens in the 2024 update to generate estimates

* This codehas been simplified for sharing 
* (removed loops to process multiple surveys at once and additional quality control steps)

* relabel GPAQ physical activity variables as p1 to p15.
* p1: do you do vigorous intensity work?
* p2: how many days do you do vigorous intensity work?
* p3a: how many hours do you do vigorous intensity work per day?
* p3b: how many minutes do you do vigorous intensity work per day?
* p4: do you do moderate intensity work?
* p5: how many days do you do moderate intensity work?
* p6a: how many hours do you do moderate intensity work per day?
* p6b: how many minutes do you do moderate intensity work per day?
* p7: do you do active travel?
* p8: how many days do you do active travel?
* p9a: how many hours do you do active travel per day?
* p9b: how many minutes do you do active travel per day?
* p10: do you do vigorous intensity leisure activity?
* p11: how many days do you do vigorous intensity leisure activity?
* p12a: how many hours do you do vigorous intensity leisure activity per day?
* p12b: how many minutes do you do vigorous intensity leisure activity per day?
* p13: do you do moderate intensity leisure activity?
* p14: how many days do you do moderate intensity leisure activity?
* p15a: how many hours do you do moderate intensity leisure activity per day?
* p15b: how many minutes do you do moderate intensity leisure activity per day?


* generate new set of original variables to clean (keep original variables in tact)
foreach x in 1 2 3a 3b 4 5 6a 6b 7 8 9a 9b 10 11 ///
			 12a 12b 13 14 15a 15b {
	gen p`x'_c = p`x'
	}
	
* Clean the time variables	
* Missing codes: . 7&77 8&88 9&99 - if others are used, the code below will need to be altered
foreach z in 3 6 9 12 15 {
		
		* deals with when minutes have been entered as hours
		* 15, 30, 45 or 60 hours (should be mins)
		* transfer them to mins, replace hours = 0 assuming no other data
		gen p`z'_hours = 15 if p`z'a_c==15 & ///
							  (p`z'b_c==.  | p`z'b_c==0  | p`z'b_c==15 | ///
							   p`z'b_c==77 | p`z'b_c==88 | p`z'b_c==99)
		replace p`z'a_c = 0 if p`z'_hours == 15
		replace p`z'b_c = 15 if p`z'_hours == 15
		
		replace p`z'_hours = 30 if p`z'a_c ==30 & ///
								  (p`z'b_c==.  | p`z'b_c==0  | p`z'b_c==30 | ///
								   p`z'b_c==77 | p`z'b_c==88 | p`z'b_c==99)
		replace p`z'a_c = 0 if p`z'_hours == 30
		replace p`z'b_c = 30 if p`z'_hours == 30		
		
		replace p`z'_hours = 45 if p`z'a_c ==45 & ///
								  (p`z'b_c==.  | p`z'b_c==0  | p`z'b_c==45 | ///
								   p`z'b_c==77 | p`z'b_c==88 | p`z'b_c==99)
		replace p`z'a_c = 0 if p`z'_hours == 45
		replace p`z'b_c = 45 if p`z'_hours == 45	
		
		replace p`z'_hours = 60 if p`z'a_c ==60 & ///
								  (p`z'b_c==. | p`z'b_c==0 | p`z'b_c==60 | ///
								   p`z'b_c==77 | p`z'b_c==88 | p`z'b_c==99)
		replace p`z'a_c = 0 if p`z'_hours == 60
		replace p`z'b_c = 60 if p`z'_hours == 60
		
		* Deals with missing values across hours/mins variables
		* recode values of 7 & 77, 8 & 88, 9 & 99
		replace p`z'_hours = 789 if ((p`z'a_c==7 & p`z'b_c==77) | ///
									 (p`z'a_c==8 & p`z'b_c==88) | ///
									 (p`z'a_c==9 & p`z'b_c==99))
		replace p`z'a_c = 0 if p`z'_hours == 789
		replace p`z'b_c = 0 if p`z'_hours == 789
		replace p`z'a_c = 0 if (p`z'a_c==77 | p`z'a_c==88 | p`z'a_c==99)
		replace p`z'b_c = 0 if (p`z'b_c==77 | p`z'b_c==88 | p`z'b_c==99)
		
		* generates total time in minutes
		gen p`z'amin=(p`z'a_c*60)
		replace p`z'amin=0 if p`z'a_c==.
		tab p`z'amin, miss

		gen p`z'bmin=p`z'b_c
		replace p`z'bmin=0 if p`z'b_c==.
		tab p`z'bmin, miss

		gen p`z'_c=p`z'amin+p`z'bmin 
		replace p`z'_c = p`z'_c if p`z'_c!=.
	}
	
* Clean the day variable
* Missing codes: . 99
foreach y in 2 5 8 11 14 {
		local x = `y'-1
		
		* Replace days = 0 if missing for the time being
		replace p`y'_c = 0 if (p`y'_c==. | p`y'_c==99)
		
		* Generate variable to decide if valid (1=Valid, 2=Not valid)
		* Either has valid days after reporting Yes to participation
		* Or has 0 days after reporting No to participation
		gen p`y'CLN=2
		replace p`y'CLN=1 if ((p`x'_c==1 & p`y'_c>0 & p`y'_c<8) | ///
								(p`x'_c==2 & p`y'_c==0))
								
	}		

* Generate tags for whether time variable is valid
foreach y in 2 5 8 11 14 {
		local x = `y'-1
		local z = `y'+1

		* Time variable is valid if Days are between 1-7 and time is between 9 and 961 mins
		* Or if days are 0 and time is 0
		* And the previously generated days variable is valid
		gen p`z'CLN=2
		replace p`z'CLN=1 if ((p`y'CLN==1 & p`y'_c>0 & p`y'_c<8 & ///
								p`z'_c>9 & p`z'_c<961) ///
								| (p`y'CLN==1 & p`y'_c==0 & p`z'_c==0))
}

* Generate tags for whether whole domain is valid
foreach y in 2 5 8 11 14 {
		local x = `y'-1
		local z = `y'+1
								
		* Domain is valid if time is valid
		* Or if the participation variable is missing but days and time are 0 
		
		gen p`x't`z'CLN=2
		replace p`x't`z'CLN=1 if (p`z'CLN==1)
		replace p`x't`z'CLN=1 if (p`x'_c==. & p`y'_c==0 & p`z'_c==0)
	}
	
* Generate MET mins for domains	
foreach y in 2 5 8 11 14 {
		local x = `y'-1
		local z = `y'+1

		* For the vigorous intensity domains, MET value = 8
		* Days * Mins * MET
		* Replace with missing if the domain is not Valid
		if `y'==2 | `y'==11 {
			gen p`x't`z'=(p`y'_c  *p`z'_c * 8)
			replace p`x't`z'=. if p`x't`z'CLN!=1
		}
		
		* For the moderate intensity domains, MET value = 4
		* Days * Mins * MET
		* Replace with missing if the domain is not Valid
		if `y'==5 | `y'==8 | `y'==14 {
			gen p`x't`z'=(p`y'_c * p`z'_c * 4)
			replace p`x't`z'=. if p`x't`z'CLN!=1
		}
}

* Total MET values
gen ptotal_c = p1t3 + p4t6 + p7t9 + p10t12 + p13t15


* Generate meeting recommendations with 600 METmins/week threshold
gen meet_recs_c=0 if ptotal_c<600
replace meet_recs_c=1 if ptotal_c>=600 & ptotal_c!=.

* Generate fail to meet recommendations
gen fail_meet_recs_c=0 if meet_recs_c==1
replace fail_meet_recs_c=1 if meet_recs_c==0
tab meet_recs_c, miss
tab fail_meet_recs_c, miss





