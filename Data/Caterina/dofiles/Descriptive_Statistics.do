//  program:    master.do
//  task:       Set file paths
//  project:	CoVideo
//  author:     AV / Created: 17Aug2020

drop _all
clear matrix
clear mata

** Or put whatever version you have here
version 14.2

**********************************************************************
************************* Set paths **********************************
**********************************************************************
if "`c(username)'"=="alain.vandormael" {
  global user = "C:/Users/`c(username)'"
  global home = "$user/Seafile/Heidelberg/Projects/CoVideo/Data"
} 
else {
  global home = "C:\Users\Caterina\Seafile\Data"
}


global Derived "$home/Derived"


**********************************************************************
**********************************************************************
**********************************************************************

webdoc init Descriptive_Statistics, replace logdir(Descriptive_Statistics) prefix(des) header(st(standard) title("DESCRIPTIVE STATISTICS"))

/***
<h3>DESCRIPTIVE STATISTICS</h3>
***/

/***
<h3>How many people had an ERROR/NA and dropped?  343</h3>
***/

webdoc stlog
use "$Derived\VideoTime_ID.dta", clear
count
tab VideoTime_category , miss nolab 
webdoc stlog close

/***
<h3>How many people had VideoTime=0?  2,611</h3>
***/

webdoc stlog
use "$Derived\VideoTime_ID.dta", clear
drop if VideoTime_category==4
bysort ID: keep if VideoTime_max150==0
count
webdoc stlog close

/***
<h3>How many people had VideoTime>0?  6,731</h3>
***/

use "$Derived\VideoTime_ID.dta", clear
webdoc stlog
bysort ID: keep if VideoTime_max150>0 & VideoTime_max150!=.
count
webdoc stlog close

/***
<h3>For those that watched the CoVideo, what was the average time watched?</h3>
***/

use "$Derived\VideoTime_ID.dta", clear
webdoc stlog
bysort ID: keep if VideoTime_max150!=0 & VideoTime_max150!=.
sum VideoTime_max150
webdoc stlog close

use "$Derived\dat_all.dta", clear
drop if VideoArm=="Treatment" 
merge 1:1 ID using "$Derived\VideoTime_ID.dta"
bysort ID: keep if VideoTime_max150!=0 & VideoTime_max150!=.

/*** 
<h3>Distribution of VideoTime (for those who decided to watch the CoVideo) by Age</h3>
***/

webdoc stlog
graph hbar VideoTime_max150, over( Age )
webdoc graph

sum VideoTime_max150 if Age=="18-24 years"
sum VideoTime_max150 if Age=="25-34 years"
sum VideoTime_max150 if Age=="35-44 years"
sum VideoTime_max150 if Age=="45-54 years"
sum VideoTime_max150 if Age=="55-59 years"
webdoc stlog close

/*** 
<h3>Distribution of VideoTime (for those who decided to watch the CoVideo) by Gender</h3>
***/

webdoc stlog
graph hbar VideoTime_max150, over( Gender )
webdoc graph

sum VideoTime_max150 if Gender=="Female"
sum VideoTime_max150 if Gender=="Male"
sum VideoTime_max150 if Gender=="Other"
webdoc stlog close

/*** 
<h3>Distribution of VideoTime (for those who decided to watch the CoVideo) by Country</h3>
***/

webdoc stlog
graph hbar VideoTime_max150, over( Country )
webdoc graph

sum VideoTime_max150 if Country=="Germany"
sum VideoTime_max150 if Country=="Mexico"
sum VideoTime_max150 if Country=="Spain"
sum VideoTime_max150 if Country=="United Kingdom"
sum VideoTime_max150 if Country=="United States"
webdoc stlog close

/*** 
<h3>Distribution of VideoTime (for those who decided to watch the CoVideo) by Education</h3>
***/

webdoc stlog
graph hbar VideoTime_max150, over( Educ2 )
webdoc graph

sum VideoTime_max150 if Educ2=="Completed High School"
sum VideoTime_max150 if Educ2=="MA, PhD"
sum VideoTime_max150 if Educ2=="Primary or less"
sum VideoTime_max150 if Educ2=="Some College, BA"
webdoc stlog close

/*** 
<h3>Distribution of VideoTime (for those who decided to watch the CoVideo) by Language</h3>
***/

webdoc stlog
graph hbar VideoTime_max150, over( Language )
webdoc graph

sum VideoTime_max150 if Language=="DE"
sum VideoTime_max150 if Language=="EN"
sum VideoTime_max150 if Language=="MX"
sum VideoTime_max150 if Language=="SP"
webdoc stlog close

/*** 
<h3>Distribution of VideoTime (when the 150 second-limit is not set)</h3>
***/
use "$Derived\VideoTime_ID.dta", clear
webdoc stlog
bysort ID: keep if VideoTime!=0 & VideoTime!=.
tab VideoTime_category
webdoc stlog close
