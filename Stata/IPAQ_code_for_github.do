******************************************************************************
****** 						    IPAQ PROCESSING 					 *********
******************************************************************************

* This code processes the IPAQ short for the 2024 update of the comparable estimates
* Originated from Regina Guthold 2018 Comparable Estiamtes
* Modified by Tessa Strain/Gretchen Stevens in the 2024 update (explanation of methods in paper appendices)

* This pipeline covers both IPAQ short and short modified.
* When checking for outliers, these are separated.

* This covers the code to generate compliance with the 150 min guideline
* And IPAQ scoring protocol categories (not technically used in the 2024 analyses)

* Note that we have not truncated for 180 mins but this would not affect the % meeting guidelines

**** THIS FILE HAS BEEN SLIGHTLY SIMPLIFIED FOR SHARING (AND QUALITY CONTROL CHECKS REMOVED) ****

/******************** HOUSE KEEPING *****************************************/

local clean 			= 1
local tidymissings 		= 1
local mismatch 			= 1
local totsesstime 		= 1
local totweektime 		= 1
local backfill			= 1
local inconsistent 		= 1
local flagmiss			= 1 
local guideline600 		= 1
local ipaqlow 			= 1
local check 			= 1

/******************** DATASET ***********************************************/
use [enter dataset], clear

* To make this code run
* RENAME YOUR VARIABLES TO THESE NAMES
* The standard IPAQ short would have
	* vig_days
	* vig_hr_session
	* vig_mod_session
	* mod_days
	* mod_hr_session
	* mod_mins_session
	* walk_days
	* walk_hr_session
	* walk_min_session
* If your survey has already combined the hours and minutes variables then you may have
	* vig_totmins_session
	* mod_totmins_session
	* walk_totmins_session
* Some surveys may have already multiplied out the days and session time. Then you will have
	* vig_totmins_week
	* mod_totmins_week
	* walk_totmins_week

* GENERATE ANY VARIABLES THAT YOU DO NOT HAVE AS MISSING
* remove as appopriate
gen vig_days = .
gen vig_hr_session = .
gen vig_mod_session = .
gen mod_days = .
gen mod_hr_session = .
gen mod_mins_session = .
gen walk_days = .
gen walk_hr_session = .
gen walk_min_session = .
gen vig_totmins_session = .
gen mod_totmins_session = .
gen walk_totmins_session = .
gen vig_totmins_week = .
gen mod_totmins_week = .
gen  walk_totmins_week = .

lab var vig_days "Number of days of vig activity per week (orig var)"
lab var mod_days "Number of days of vig activity per week (orig var)"
lab var walk_days "Number of days of vig activity per week (orig var)"
lab var vig_totmins_session "Time per vig session in minutes (orig var)"
lab var mod_totmins_session "Time per mod session in minutes (orig var)"
lab var walk_totmins_session "Time per walk session in minutes (orig var)"
lab var vig_totmins_week "Vig time per week in minutes (orig var)"
lab var mod_totmins_week "Mod time per week in minutes (orig var)"
lab var walk_totmins_week "Walk time per week in minutes (orig var)"
lab var vig_hr_session "Time per vig session - hours only (orig var)"
lab var mod_hr_session "Time per mod session - hours only (orig var)"
lab var walk_hr_session "Time per walk session - hours only (orig var)"
lab var vig_min_session "Time per vig session - minutes only (orig var)"
lab var mod_min_session "Time per mod session - minutes only (orig var)"
lab var walk_min_session "Time per walk session - minutes only (orig var)"

order vig* mod* walk*


/******************** PROCESS VARIABLES *************************************/

* Generate clean copies of variables
if `clean' == 1 {
	
	* Generate clean set of variables
	foreach x in vig mod walk {
	gen `x'_days_c = `x'_days
	gen `x'_totmins_week_c = `x'_totmins_week
	gen `x'_totmins_session_c = `x'_totmins_session
	gen `x'_hr_session_c = `x'_hr_session
	gen `x'_min_session_c = `x'_min_session
}
}

* Sort out missing values in variables
if `tidymissings' == 1 {
	**** 	DAYS VARIABLES

		* Check number of days for missing/implausible
		foreach x in vig mod walk {
			tab `x'_days_c, mi
			replace `x'_days_c=. if `x'_days_c<0
			replace `x'_days_c=. if `x'_days_c>7
			tab `x'_days_c, mi
		}

	**** 	TIME VARIABLES - HRS AND MINS PER SESSION REPORTED SEPARATELY 

		* check for implausible values bearing in mind the "hours as mins" code below
		foreach x in vig mod walk {
			tab `x'_min_session_c, mi
			replace `x'_min_session_c=. if `x'_min_session_c<0
			replace `x'_min_session_c=. if `x'_min_session_c>1440
			replace `x'_min_session_c=. if `x'_min_session_c==88 | `x'_min_session_c==99
			tab `x'_min_session_c, mi
		}

		foreach x in vig mod walk {
			tab `x'_hr_session_c, mi
			replace `x'_hr_session_c=. if `x'_hr_session_c<0
			replace `x'_hr_session_c=. if `x'_hr_session_c==88 | `x'_hr_session_c==99
			tab `x'_hr_session_c, mi
		}

		su vig_days_c vig_hr_session_c vig_min_session_c mod_days_c mod_hr_session_c ///
		mod_min_session_c walk_days_c walk_hr_session_c walk_min_session_c

		* Check if hours is 15, 30, 45, 60 (i.e. mins in wrong place)
		* Replace mins with this value if mins is either 0 or missing
		* Aware 20 could be a candidate for including here but not in GPAQ protocol that we are using
		foreach x in vig mod walk {
			tab `x'_hr_session_c if `x'_hr_session_c==15 | `x'_hr_session_c==30 | ///
									`x'_hr_session_c==45 | `x'_hr_session_c==60 
			tab `x'_min_session_c if `x'_hr_session_c==15 | `x'_hr_session_c==30 | ///
									`x'_hr_session_c==45 | `x'_hr_session_c==60, mi 
			replace `x'_min_session_c = `x'_hr_session_c if (`x'_hr_session_c==15 | ///
									`x'_hr_session_c==30 | `x'_hr_session_c==45 | ///
									`x'_hr_session_c==60) & (`x'_min_session_c==. | ///
									`x'_min_session_c==0)
		}

		* redo check for implausible values for hours after the "mins as hours" code
		* put 24h as upper limit for now, outliers will get dealt with later anyway
		foreach x in vig mod walk {
			tab `x'_hr_session_c, mi
			replace `x'_hr_session_c=. if `x'_hr_session_c>24
			tab `x'_hr_session_c, mi
		}


}

* Sort out hour mins variables when one or other is missing
if `mismatch' == 1 {		
	
	* Replace mins = 0 if hours is valid and mins is missing
	foreach x in vig mod walk {
			tab `x'_min_session_c if `x'_hr_session_c!=., mi
			replace `x'_min_session_c = 0 if `x'_hr_session_c!=. & `x'_min_session_c==.
			tab `x'_min_session_c if `x'_hr_session_c!=., mi
		}

	* Replace hours = 0 if mins is valid and hours is missing	
	foreach x in vig mod walk {
			tab `x'_hr_session_c if `x'_min_session_c!=., mi
			replace `x'_hr_session_c = 0 if `x'_min_session_c!=. & `x'_hr_session_c==.
			tab `x'_hr_session_c if `x'_min_session_c!=., mi
		}
	
}

* Calculate total session time (hr*60 + mins) 
if `totsesstime' == 1 {

	**** 	TIME VARIABLES - TOTAL TIME PER SESSION 

		* see which surveys already have total session duration in mins
		* IPS and Eurobarometer
		/*

		su vig_totmins_session_c mod_totmins_session_c walk_totmins_session_c
		tab survey if walk_totmins_session_c!=.

		* check no overlap
		tab vig_totmins_session_c if vig_hr_session_c!=.
		tab vig_hr_session_c if vig_totmins_session_c!=.
		*/

		* Calculate total time in mins	
		* Baggy code but just to allow for manual checks
		foreach x in vig mod walk {
			gen `x'_totmins_session_temp = (`x'_hr_session_c*60)+`x'_min_session_c
		}

		su vig_hr_session_c vig_min_session_c vig_totmins_session_temp mod_hr_session_c ///
		mod_min_session_c mod_totmins_session_temp walk_hr_session_c walk_min_session_c walk_totmins_session_temp

		* list walk_hr_session_c walk_min_session_c walk_totmins_session_temp if walk_totmins_session_temp!=. 
		foreach x in vig mod walk {
			replace `x'_totmins_session_c= `x'_totmins_session_temp if ///
									`x'_totmins_session_c==. & `x'_totmins_session_temp!=.
		}

		su vig_days_c vig_totmins_session_c mod_days_c mod_totmins_session_c walk_days_c walk_totmins_session_c

		* check for implausible values here
		foreach x in vig mod walk {
			tab `x'_totmins_session_c, mi
			replace `x'_totmins_session_c= . if `x'_totmins_session_c<0
			replace `x'_totmins_session_c= . if `x'_totmins_session_c>1440
		}

		su vig_days_c vig_totmins_session_c mod_days_c mod_totmins_session_c walk_days_c walk_totmins_session_c

	* Convert time to 0 if days = 0 and time ==. 
	foreach x in vig mod walk {
		tab `x'_totmins_session_c if `x'_days_c==0, mi
		replace `x'_totmins_session_c=0 if `x'_days_c==0 & `x'_totmins_session_c==.
	}

	* Convert days to 0 if days = . and time ==0
	foreach x in vig mod walk {
		tab `x'_days_c if `x'_days_c==. & `x'_totmins_session_c==0, mi
		tab `x'_hr_session_c `x'_min_session_c if `x'_days_c==. & `x'_totmins_session_c==0, mi
		replace `x'_days_c=0 if `x'_days_c==. & `x'_totmins_session_c==0
	}

	su vig_days_c vig_totmins_session_c mod_days_c mod_totmins_session_c walk_days_c walk_totmins_session_c

	* Inconsistent responses: days>0 & time==0
	* Also replace the time to missing at the same time
	foreach x in vig mod walk {
		tab `x'_days_c if `x'_totmins_session_c==0, mi
		gen temp = 1 if `x'_days_c>0 & `x'_days_c!=. & `x'_totmins_session_c==0
		replace `x'_days_c = . if temp==1
		replace `x'_totmins_session_c = . if temp==1
		drop temp
		tab `x'_days_c if `x'_totmins_session_c==0, mi
	}

	* Inconsistent responses: time>0 & days
	* Also replace the days to missing at the same time
	foreach x in vig mod walk {
		tab `x'_totmins_session_c if `x'_days_c==0, mi
		gen temp = 1 if `x'_totmins_session_c>0 & `x'_totmins_session_c!=. & `x'_days_c==0
		replace `x'_totmins_session_c = . if temp==1
		replace `x'_days_c = . if temp==1
		drop temp
		tab `x'_totmins_session_c if `x'_days_c==0, mi
	}

	* Apply maximum value of 960 mins
	foreach x in vig mod walk {
		tab `x'_totmins_session_c if `x'_totmins_session_c>960 & `x'_totmins_session_c!=., mi
		tab `x'_days_c if `x'_totmins_session_c>960 & `x'_totmins_session_c!=., mi
		gen temp = 1 if `x'_totmins_session_c>960 & `x'_totmins_session_c!=.
		replace `x'_totmins_session_c=. if temp==1
		replace `x'_days_c =. if temp==1
		drop temp
	}

	su vig_days_c vig_totmins_session_c mod_days_c mod_totmins_session_c walk_days_c walk_totmins_session_c

}

* Calculate total weekly time (days * session time) 
if `totweektime' == 1 {

	**** 	TIME VARIABLES - TOTAL TIME PER WEEK

		* Again baggy but just to allow for checks
		foreach x in vig mod walk {
			gen `x'_totmins_week_temp = `x'_totmins_session_c * `x'_days_c
		}
		*list walk_totmins_week_temp walk_totmins_session_c walk_days_c if walk_totmins_week_temp!=.
		foreach x in vig mod walk {
			replace `x'_totmins_week_c = `x'_totmins_week_temp if `x'_totmins_week_c==. & `x'_totmins_week_temp!=.
		}

		su vig_days_c vig_totmins_week_c vig_totmins_session_c mod_days_c ///
		mod_totmins_week_c mod_totmins_session_c walk_days_c walk_totmins_week_c walk_totmins_session_c 

		* clean variable, remove high outliers (7*960) for now
		foreach x in vig mod walk {
			tab `x'_totmins_week_c, mi
			replace `x'_totmins_week_c = . if `x'_totmins_week_c==9999
			replace `x'_totmins_week_c = . if `x'_totmins_week_c>6720
			replace `x'_totmins_week_c = . if `x'_totmins_week_c<0
		}

		* Convert time to 0 if days = 0 and time ==. 
		foreach x in vig mod walk {
			tab `x'_totmins_week_c if `x'_days_c==0, mi
			replace `x'_totmins_week_c=0 if `x'_days_c==0 & `x'_totmins_week_c==.
		}

		* Convert time to 0 if days = . and time ==0
		foreach x in vig mod walk {
			tab `x'_days_c if `x'_days_c==. & `x'_totmins_week_c==0, mi
			replace `x'_days_c=0 if `x'_days_c==. & `x'_totmins_week_c==0
		}

		* Inconsistent responses: days>0 & time==0
		foreach x in vig mod walk {
			tab `x'_days_c if `x'_totmins_week_c==0, mi
			replace `x'_days_c = . if `x'_days_c>0 & `x'_days_c!=. & `x'_totmins_week_c==0
		}

		* Inconsistent responses: time>0 & days
		foreach x in vig mod walk {
			tab `x'_totmins_week_c if `x'_days_c==0, mi
			replace `x'_totmins_week_c = . if `x'_totmins_week_c>0 & `x'_totmins_week_c!=. & `x'_days_c==0
		}

		su vig_days_c vig_totmins_session_c vig_totmins_week_c mod_days_c ///
		mod_totmins_session_c mod_totmins_week_c walk_days_c walk_totmins_session_c walk_totmins_week_c

}

* Fill in session time if only got weekly time 
if `backfill' == 1 {

	**** 	TIME VARIABLES - BACKFILL TIME PER SESSION IF ONLY GIVEN TIME PER WEEK

		* This variable (x_totmins_session_c) also needed for IPAQ low category calcs
		gen temp = survey if vig_totmins_week!=. | ///
							mod_totmins_week!=. | ///
							walk_totmins_week!=.
		tab temp // make sure not got more than ENFR here

		levelsof temp, local(levels)
		foreach level of local levels {
			tab survey if survey=="`level'"
				foreach x in vig mod walk {
					replace `x'_totmins_session_c = `x'_totmins_week_c/`x'_days_c if survey=="`level'"
					replace `x'_totmins_session_c = 0 if (`x'_totmins_week_c==0 | `x'_days_c==0) & survey=="`level'"
		}
		}

		drop temp

		su vig_days_c vig_totmins_session_c vig_totmins_week_c mod_days_c ///
		mod_totmins_session_c mod_totmins_week_c walk_days_c walk_totmins_session_c walk_totmins_week_c

		* Proper outlier sorting at session level
		foreach x in vig mod walk {
			tab `x'_totmins_week_c, mi
			tab survey if `x'_totmins_session_c>960 & `x'_totmins_session_c!=.
			replace `x'_totmins_week_c = . if `x'_totmins_session_c>960 & `x'_totmins_session_c!=.
			replace `x'_totmins_session_c = . if `x'_totmins_session_c>960 & `x'_totmins_session_c!=.
		}

}

* Make all responses across variables consistent (missings, zeros etc)
if `inconsistent' == 1 {

	**** 	OVERALL TIDY UP NOW ALL SURVEYS HAVE SESSION

		* Check inconsistent missings
		* Weekly and Session time=. if days == .
		foreach x in vig mod walk {
			tab `x'_totmins_week_c if `x'_days_c==., mi
			replace `x'_totmins_week_c = . if `x'_days_c==.
			tab `x'_totmins_session_c if `x'_days_c==., mi
			replace `x'_totmins_session_c = . if `x'_days_c==.
		}
		* n for session and week vars should be the same here
		* previously session was higher because most of the week vars were already . if days=.
		* due to it being calcualted by days*session
		su vig_days_c vig_totmins_session_c vig_totmins_week_c mod_days_c ///
		mod_totmins_session_c mod_totmins_week_c walk_days_c walk_totmins_session_c walk_totmins_week_c

		* Days = . if weekly/seesion time = .
		foreach x in vig mod walk {
			tab `x'_days_c if `x'_totmins_week_c==. & `x'_totmins_session_c==.
			replace `x'_days_c=. if `x'_totmins_week_c==. & `x'_totmins_session_c==.
		}
		su vig_days_c vig_totmins_session_c vig_totmins_week_c mod_days_c ///
		mod_totmins_session_c mod_totmins_week_c walk_days_c walk_totmins_session_c walk_totmins_week_c

		}

* Flag if missing one or all domains, replace domain = 0 if missing but others are valid
if `flagmiss' == 1 {

	* Generate flag variables for missing one domain and missing all domains
	gen anymiss_i = .
	foreach x in vig mod walk {
		replace anymiss_i = 1 if `x'_days_c==.
	}
	gen allmiss_i = . 
	replace allmiss_i = 1 if vig_days_c==. & mod_days_c==. & walk_days_c==.

	* Replace domains = 0 if at least one domain is valid
	foreach x in vig mod walk {
		replace `x'_days_c=0 if `x'_days_c==. & allmiss_i!=1
		replace `x'_totmins_week_c=0 if `x'_totmins_week_c==. & allmiss_i!=1
		replace `x'_totmins_session_c=0 if `x'_totmins_session_c==. & allmiss_i!=1
	}

	su vig_days_c vig_totmins_week_c vig_totmins_session_c mod_days_c mod_totmins_week_c mod_totmins_session_c walk_days_c walk_totmins_week_c walk_totmins_session_c

	* Make sure all are missing if missing across all domains
	foreach x in vig mod walk {
		replace `x'_days_c = . if allmiss_i==1
		replace `x'_totmins_week_c = . if allmiss_i==1
		replace `x'_totmins_session_c = . if allmiss_i==1
	}

	su vig_days_c vig_totmins_week_c vig_totmins_session_c mod_days_c ///
	mod_totmins_week_c mod_totmins_session_c walk_days_c walk_totmins_week_c walk_totmins_session_c


	
}

* tidy up dataset
drop vig_totmins_session_temp mod_totmins_session_temp walk_totmins_session_temp ///
vig_totmins_week_temp mod_totmins_week_temp walk_totmins_week_temp

* drop those with no PA data
drop if vig_days_c==. 
* mdesc vig_days_c vig_totmins_week_c vig_totmins_session_c mod_days_c ///
* mod_totmins_week_c mod_totmins_session_c walk_days_c walk_totmins_week_c walk_totmins_session_c

* Generate compliance with 600 MET min guideline
if `guideline600' == 1 {

	*** TO GENERATE 150 MIN COMPLIANCE (I.E. 600 METS)
		* Note, there was extensive discussion around use of 4 METs for walking as IPAQ protocol 
		* is 3.3
		* However, IPAQ protocol is not intended generate compliance to 150 min guideline
		* For consistency with GPAQ processing we allocate 4 METs to walking
		
		* MET mins
		gen walkMETminweek150mins = 	walk_totmins_week_c *4.0
		gen modMETminweek		  = 	mod_totmins_week_c  *4.0
		gen vigMETminweek 		  = 	vig_totmins_week_c  *8.0

		gen totalMETminweek150mins = walkMETminweek150mins + modMETminweek + vigMETminweek

		* Meet guidelines
		gen guidelines = .
		replace guidelines = 0 if totalMETminweek150mins<600 
		replace guidelines = 1 if totalMETminweek150mins>=600 & totalMETminweek150mins!=.

		* Not meet guidelines
		gen fail_meet_recs = 1 if guidelines == 0
		replace fail_meet_recs = 0 if guidelines == 1

}

* Generate compliance with ipaq low category
* Note this code does use the 3.3 MET for walking as per the IPAQ protocol

if `ipaqlow' == 1 {
	
	* This code is from RG 2018 paper, just replaced with our variable names

		gen walkMETminweek530 = walk_totmins_week_c *3.3
		gen totalMETminweek530 = walkMETminweek530 + modMETminweek + vigMETminweek

		*** low_vigorous_activity
			* lowvig = 1 if meet the "Category 2 Moderate" criteria for vigorous activity 

			gen lowvig=1 	 if 	vig_days_c>=3 & vig_days_c!=. & ///
									vig_totmins_session_c>=20 & vig_totmins_session_c!=.
			replace lowvig=0 if 	vig_days_c<3 | vig_totmins_session_c<20
			replace lowvig=. if		vig_days_c==. | vig_totmins_session_c==. 

		***low_moderate_activity
			* lowmod = 1 if meet the "Category 2 Moderate" criteria for moderate activity

			gen lowmod=1 if 		( mod_days_c>=5 & mod_days_c!=. & ///
									  mod_totmins_session_c>=30 & mod_totmins_session_c!=.	   ) | ///
									( walk_days_c>=5 & walk_days_c!=. & ///
									  walk_totmins_session_c>=30 & walk_totmins_session_c!=.   )
			replace lowmod=1 if 	( (mod_days_c+walk_days_c)>=5 & ///
									  (mod_days_c+walk_days_c!=.)							   ) & ///
									( (mod_totmins_session_c>=30 & mod_totmins_session_c!=.) & ///
									  (walk_totmins_session_c>=30 & walk_totmins_session_c!=.) )
			* the above line is open to interpretation in the protocol
			* one could say that the walk session and mod session need to add up to 30
			* but I can see how this interpretation is probably more likely correct
			* it is also what Regina did before so we will stick with that	
			replace lowmod=0 if 	( mod_days_c<5 | mod_totmins_session_c<30 				   ) & ///
									( walk_days_c<5 | walk_totmins_session_c<30				   ) & ///
									( (mod_days_c+walk_days_c)<5 | mod_totmins_session_c<30 | ///
									   walk_totmins_session_c<30 ) 
			* i have added code to lines above to make sure criteria are mutually exclusive
			replace lowmod=. if 	( mod_days_c==. | mod_totmins_session_c==.) & ///
									( walk_days_c==. | walk_totmins_session_c==.)   

		***low_walking_OR_moderate_OR_vigorous_activity
			* low_vmw = 1 if meeting the "Category 2 Moderate" criteria that combines all domains

			gen totdays=(vig_days_c+mod_days_c+walk_days_c)

			* I have simplified this code but it ends up in the same place
			gen low_vmw=1 if 		totdays>=5 & totalMETminweek530>=600 
			replace low_vmw= 0 if 	totdays<5 | totalMETminweek530<600 

			replace low_vmw=. if (vig_days_c==. | vigMETminweek==.) & ///
								 (mod_days_c==. | modMETminweek==.) & ///
								 (walk_days_c==. | walkMETminweek530==.)          

		***low_total_activity
			* lowtot = 1 if any of the criteria are met

			generate lowtot=1 if lowvig==1 | lowmod==1 | low_vmw==1
			replace lowtot=0 if lowvig==0 & lowmod==0 & low_vmw==0
			replace lowtot=. if lowvig==. & lowmod==. & low_vmw==.
			lab def lowtot 0 "Low activity" 1"Not low activity"
			lab val lowtot lowtot

		***sufficient_vigorous_activity

			gen suffvig=1 		if vig_days_c>=3 & vig_days_c!=. & totalMETminweek530>=1500 & totalMETminweek530!=.
			replace suffvig=0 	if vig_days_c<3 | totalMETminweek530<1500
			replace suffvig=. 	if vig_days_c==. | totalMETminweek530==.

		***sufficient_level_of_moderate_and_vigorous_activity_combined

			* TS simplified code here but gets to same answer
			generate suff_vmw=1 	if totdays>=7 | totalMETminweek530>=3000
			replace suff_vmw=0 	if totdays<7 | totalMETminweek530<3000
			replace suff_vmw=. if (vig_days_c==. | vigMETminweek==.) & ///
			(mod_days_c==. | modMETminweek==.) & (walk_days_c==. | walkMETminweek530==.)        

		***sufficient_total_activity

			generate sufftot=1 if suffvig==1 | suff_vmw==1
			replace sufftot=0 if suffvig==0 & suff_vmw==0
			replace sufftot=. if suffvig==. & suff_vmw==.
			lab def sufftot 0 "Not high" 1 "high"				
			lab val sufftot sufftot

		*****	single_variable_for_activity_category ****
		**************************************************

			tab lowtot sufftot
			* Noted n= 2 are classified as both low and high. 
			* list vig_days_c vig_totmins_week_c vig_totmins_session_c mod_days_c ///
			* mod_totmins_week_c mod_totmins_session_c walk_days_c walk_totmins_week_c ///
			* walk_totmins_session_c totdays totalMETminweek530 if lowtot==0 & sufftot==1
			
			* A very odd combination of short durations of vig activity (4 and 10 min sessions respectively) yet high enough
			* walking to make the total MET mins about 1500
			* We will allocate as sufficient activity 

			gen ipaqcat=1 if lowtot==0
			replace ipaqcat=2 if lowtot==1
			replace ipaqcat=3 if sufftot==1
			replace ipaqcat=1 if totalMETminweek530==0 // no changes at present - that should be the case
			replace ipaqcat=. if lowtot==. & sufftot==.		

			**generation of variables for prevalences**
			gen low=1 		if ipaqcat==1
			replace low=0 	if ipaqcat!=1
			replace low=. if ipaqcat==.
}

compress
save [enter file name], replace	

