******************************
// negativity_code.do: Code for recreating paper models in Stata
// Note: Data come from ICPSR #34895, "Congressional Candidate Websites," collected
	//by James Druckman, Michael Parkin, and Martin Kifer. 
// Data Webpage: https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/34895
// Author: Redacted
******************************
version 16.0
clear all
macro drop _all
log using negativity_code.log, replace text
set more off  
cd "/file/path/here"


**************************
***COMMANDS BEGIN HERE ***
**************************
//Load in data
use campaign.dta, clear // This is the raw data download from ICPSR. You will need to download
                        // the dataset (https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/34895),
                        // as I do not have authorization to distribute it. Note that I renamed the 
                        // file to "campaign."
					
					
//Recode some variables
	//Generate a new candidate name variable with a few of the names corrected
		//so that rows match when using clustered SEs later
	gen CANDNAME_TEMP = CANDNAME
	replace CANDNAME_TEMP="Butch Otter" if CANDNAME==`"CL "Butch" Otter"'
	replace CANDNAME_TEMP="Robin Weirauch" if CANDNAME=="Robin Weirauck"
	replace CANDNAME_TEMP="Patrick Kennedy" if CANDNAME=="Patrick J. Kennedy"
	replace CANDNAME_TEMP="Richard Romeo" if CANDNAME=="Richard M. Romeo"
	replace CANDNAME_TEMP="Gary Page" if CANDNAME=="Gary R. Page"
	replace CANDNAME_TEMP="Bernie Sanders" if CANDNAME=="Bernhard Sanders"
	replace CANDNAME_TEMP="Frank LoBiondo" if CANDNAME=="Frank Lobiondo"
	replace CANDNAME_TEMP="Mike Sodrel" if CANDNAME=="Mike Soarel"
	replace CANDNAME_TEMP="Paul Gillmor" if CANDNAME=="Paul Fillmor"
	replace CANDNAME_TEMP="Rosa DeLauro" if CANDNAME=="Rosa Delauro"
	replace CANDNAME_TEMP="Diana DeGette" if CANDNAME=="Diana Degette"
	encode CANDNAME_TEMP, gen(CANDNAME_RECODE)
	
	//Generate a white/non-white binary variable
	gen NONWHITE=.
	replace NONWHITE=1 if BLACK==1 | LATINO==1 | API==1
	replace NONWHITE=0 if BLACK==0 & LATINO==0 & API==0
	
	//Get district/state pop per 1000
	gen DPOP2 = DPOP/1000
	
	//Generate "high-status negative" variable
	gen PRESNEGATIVE=0
	replace PRESNEGATIVE=1 if GONEGBUSH==1 | GONEGCHENEY==1 | GONEGKERRY==1 | ///
		GONEGDEM==1 | GONEGREP==1
		
	//Remove independents
	gen DEMOCRAT2=.
	replace DEMOCRAT2=1 if PARTY==2
	replace DEMOCRAT2=0 if PARTY==1
	
	//Log fundraising variable
	gen LOGFRAISED = log(FRAISED + 1)
	
//Get analytical sample
qui probit BIOFAMIL2 BIOTOWN BIOOCCUP BIOORG BIOVOLUN BIOVET ///
	FEMALE GONEG ISSUEOWNER ISSSAL DEMOCRAT2 NONWHITE ///
	LOGFRAISED SENATE COOKS CHALL OPEN DPOP2 DEDPERHS ///
	DINCMEDF PRESNEGATIVE YEAR OPPGONEG
	
gen flag = e(sample)
	
	//Turn the state variable into a numeric variable so we can treat it as a 
		//factor for fixed effects
	encode STATE, gen(STATE2)
	
	//MCA to get primary predictor and instrument
	//Personal availability
	mca BIOFAMIL2 BIOTOWN BIOOCCUP BIOORG BIOVOLUN BIOVET if flag==1, ///
		method(indicator) normalize(principal)
		//Note: Plotted via ggplot2 in R (I ran the analyses in Stata because
			//I couldn't seem to figure out how to use clustered SEs in an 
			//R-based ivprobit model)
		
	predict mca1 if flag==1, rowscores
	egen mca_sd = std(mca1)
	gen mca_sd2 = mca_sd*-1
	drop mca_sd
	gen mca_sd = mca_sd2
	drop mca_sd2	
	
	//Generate standardized issues variables based on analytical sample
	egen SDISSUEOWNER = std(ISSUEOWNER) if flag==1
	
	
	//Fix a gender coding error
	replace FEMALE=1 if CANDNAME=="Katherine Harris"

	
//Probit models
	//Study #1
	probit OPPGONEG c.mca_sd i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR, ///
		vce(cluster CANDNAME_RECODE)
		
	//Study #2
	probit OPPGONEG c.mca_sd##ib1.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR, ///
		vce(cluster CANDNAME_RECODE)	
		
	//Predicted probabilities for Study #2
	margins if e(sample), at(GONEG=0 SDISSUEOWNER=0 ISSSAL=13.23 ///
		LOGFRAISED=13.38 COOKS=0.24 DPOP2=1985.61 DEDPERHS=82.11 DINCMEDF=51210.01 ///
		DEMOCRAT2=0 NONWHITE=0 SENATE=0 CHALL=0 OPEN=0 PRESNEGATIVE=0 ///
		YEAR=2004 mca_sd=(-1(1)1) FEMALE=(0 1)) predict(pr)
		
	//AMEs
	margins if e(sample), dydx(mca_sd) at(GONEG=0 SDISSUEOWNER=0 ISSSAL=13.23 ///
		LOGFRAISED=13.38 COOKS=0.24 DPOP2=1985.61 DEDPERHS=82.11 DINCMEDF=51210.01 ///
		DEMOCRAT2=0 NONWHITE=0 SENATE=0 CHALL=0 OPEN=0 PRESNEGATIVE=0 ///
		YEAR=2004 FEMALE=(0 1))
		
	//Discussion Section: Separating challenger and incumbent samples
	probit OPPGONEG c.mca_sd##i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR if CHALL==1, /// Challengers
		vce(cluster CANDNAME_RECODE)
		
	margins if e(sample), at(GONEG=0 SDISSUEOWNER=0 ISSSAL=13.23 ///
		LOGFRAISED=13.38 COOKS=0.24 DPOP2=1985.61 DEDPERHS=82.11 DINCMEDF=51210.01 ///
		DEMOCRAT2=0 NONWHITE=0 SENATE=0 PRESNEGATIVE=0 ///
		YEAR=2004 mca_sd=(-1(1)1) FEMALE=(0 1)) predict(pr)
		
	probit OPPGONEG c.mca_sd##i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR if (CHALL==0 & OPEN==0), /// Incumbents
		vce(cluster CANDNAME_RECODE)
		
	margins if e(sample), at(GONEG=0 SDISSUEOWNER=0 ISSSAL=13.23 ///
		LOGFRAISED=13.38 COOKS=0.24 DPOP2=1985.61 DEDPERHS=82.11 DINCMEDF=51210.01 ///
		DEMOCRAT2=0 NONWHITE=0 SENATE=0 PRESNEGATIVE=0 ///
		YEAR=2004 mca_sd=(-1(1)1) FEMALE=(0 1)) predict(pr)
		

//Getting fit statistics
nestreg, lr: probit OPPGONEG (c.mca_sd i.FEMALE ///
	GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
	c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
	ib2004.YEAR)(c.mca_sd#i.FEMALE), ///
	vce(cluster CANDNAME_RECODE) //Note sig. incremental LR test
	
	//AIC_c
	display "AIC_c1 = " r(lr)[1,5] + (((2*r(lr)[1,3])*(r(lr)[1,3]+1)) / (e(N)-r(lr)[1,3]-1))
	display "AIC_c2 = " r(lr)[2,5] + (((2*(r(lr)[1,3]+1))*((r(lr)[1,3]+1)+1)) / ///
		(e(N)-(r(lr)[1,3]+1)-1))
		
	//Deviance
	display "-2LL_1 = " -2*r(lr)[1,1]
	display "-2LL_2 = " -2*r(lr)[2,1]

		
//Descriptives
univar OPPGONEG GONEG SDISSUEOWNER ISSSAL SENATE DEMOCRAT2 ///
	FRAISED NONWHITE FEMALE COOKS CHALL OPEN DPOP2 DEDPERHS ///
	DINCMEDF PRESNEGATIVE mca_sd BIOFAMIL2 BIOTOWN BIOOCCUP BIOORG ///
	BIOVOLUN BIOVET if flag==1
	

//Robustness checks
	//Separate study #2 model by election year.
probit OPPGONEG i.FEMALE##c.mca_sd ///
	GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 LOGFRAISED SENATE ///
	c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF ///
	if (YEAR==2002 & flag==1), ///
	vce(cluster CANDNAME_RECODE) //Effect only present for 2002 election cycle.
est store m2002

probit OPPGONEG i.FEMALE##c.mca_sd ///
	GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 LOGFRAISED NONWHITE SENATE ///
	c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
	if (YEAR==2004 & flag==1), ///
	vce(cluster CANDNAME_RECODE)
est store m2004

probit OPPGONEG i.FEMALE##c.mca_sd ///
	GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 LOGFRAISED NONWHITE SENATE ///
	c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
	if (YEAR==2006 & flag==1), ///
	vce(cluster CANDNAME_RECODE)
est store m2006
		
	//Predicted probabilities for robustness check
	est restore m2002
	margins if e(sample), at(GONEG=0 SDISSUEOWNER=0 ISSSAL=13.23 ///
		LOGFRAISED=13.38 COOKS=0.24 DPOP2=1985.61 DEDPERHS=82.11 DINCMEDF=51210.01 ///
		DEMOCRAT2=0 SENATE=0 CHALL=0 OPEN=0 ///
		mca_sd=(-1(1)1) FEMALE=(0 1)) predict(pr)
	
	est restore m2004
	margins if e(sample), at(GONEG=0 SDISSUEOWNER=0 ISSSAL=13.23 ///
		LOGFRAISED=13.38 COOKS=0.24 DPOP2=1985.61 DEDPERHS=82.11 DINCMEDF=51210.01 ///
		DEMOCRAT2=0 NONWHITE=0 SENATE=0 PRESNEGATIVE=0 CHALL=0 OPEN=0 ///
		mca_sd=(-1(1)1) FEMALE=(0 1)) predict(pr)
	
	est restore m2006
	margins if e(sample), at(GONEG=0 SDISSUEOWNER=0 ISSSAL=13.23 ///
		LOGFRAISED=13.38 COOKS=0.24 DPOP2=1985.61 DEDPERHS=82.11 DINCMEDF=51210.01 ///
		DEMOCRAT2=0 NONWHITE=0 SENATE=0 PRESNEGATIVE=0 CHALL=0 OPEN=0 ///
		mca_sd=(-1(1)1) FEMALE=(0 1)) predict(pr)

	//Models with additive index versions
		//Additive index for personal availability
		alpha BIOFAMIL2 BIOTOWN BIOOCCUP BIOORG BIOVOLUN BIOVET if flag==1 //Low Cronbach's alpha; 
				//more than one dimension (see eigenvectors for pca below)
		gen index = BIOFAMIL2+BIOTOWN+BIOOCCUP+BIOORG+BIOVOLUN+BIOVET

	probit OPPGONEG c.index i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR, ///
		vce(cluster CANDNAME_RECODE) //Results stable

	probit OPPGONEG c.index##i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR, ///
		vce(cluster CANDNAME_RECODE) //Results stable
		
	//Models with PCA versions of latent variables.
		//PCA for personal availability
		pca BIOFAMIL2 BIOTOWN BIOOCCUP BIOORG BIOVOLUN BIOVET if flag==1
		predict pca1 if e(sample), score //Note that FactoMineR returns principal 
		              //coordinates while Stata returns standard coordinates
					  
		egen pca_sd = std(pca1)

	
	probit OPPGONEG c.pca_sd i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR, ///
		vce(cluster CANDNAME_RECODE) //Results stable	

	probit OPPGONEG c.pca_sd##i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR, ///
		vce(cluster CANDNAME_RECODE) //Results stable
		
	//Logit models
	logit OPPGONEG c.mca_sd i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR, ///
		vce(cluster CANDNAME_RECODE) //Similar results
		
	logit OPPGONEG c.mca_sd##i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR, ///
		vce(cluster CANDNAME_RECODE) //Similar results
		
	//LPM models
	reg OPPGONEG c.mca_sd i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR, ///
		vce(cluster CANDNAME_RECODE) //Similar results
		
	reg OPPGONEG c.mca_sd##i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR, ///
		vce(cluster CANDNAME_RECODE) //Similar results
		
	//Should we include constituency fixed effects?
	tostring DISTRICT, gen(DISTRICT2)
	gen HOUSE = STATE + DISTRICT2
	encode HOUSE, gen(HOUSE2)
	
	probit OPPGONEG c.mca_sd i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR i.HOUSE2, ///
		vce(cluster CANDNAME_RECODE) //Similar results, and...
		
	estat ic
	display "AIC_c1 = " r(S)[1,5] + (((2*r(S)[1,4])*(r(S)[1,4]+1)) / ///
		(r(S)[1,1]-r(S)[1,4]-1)) //Fit isn't as good when looking at AIC_c and 
				//BIC. Decided to go with the constrained model for Study #1.
	
	probit OPPGONEG c.mca_sd##i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR i.HOUSE2, ///
		vce(cluster CANDNAME_RECODE) //Similar results, and...
		
	estat ic
	display "AIC_c2 = " r(S)[1,5] + (((2*r(S)[1,4])*(r(S)[1,4]+1)) / ///
		(r(S)[1,1]-r(S)[1,4]-1)) //Fit isn't as good when looking at AIC_c and 
				//BIC. Decided to go with the constrained model for Study #2.
		
//Should we use a multilevel model?
xtset CANDNAME_RECODE YEAR
				
xtprobit OPPGONEG c.mca_sd##i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR, re
est store two_level

probit OPPGONEG c.mca_sd##i.FEMALE ///
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		ib2004.YEAR
est store one_level

lrtest one_level two_level, force //Two levels fits no better
						
xtreg OPPGONEG c.mca_sd##i.FEMALE ///
	GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
	c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
	ib2004.YEAR, be //but... (see below)
	
xtprobit OPPGONEG if flag==1, re //Only about 11% of the total variance in OPPGONEG
									//is accounted for at the candidate level.
									
									
//Trying to leverage longitudinal structure of data to estimate a causal effect

	//Synchronous causal effect: Data are too unbalanced, and no instrument
		//that is simulataneously relevant and exogenous.
		
	//Lagged causal effect
		//Generate lagged variables
		sort CANDNAME_RECODE YEAR
		by CANDNAME_RECODE: gen mca_prev = mca_sd[_n-1]		
		by CANDNAME_RECODE: gen OPPGONEG_prev = OPPGONEG[_n-1]
		
		//Manually create interaction terms with factor variables
		gen COOKS_CHALL = COOKS*CHALL
		gen COOKS_OPEN = COOKS*OPEN
		
		by CANDNAME_RECODE: gen mca_female_prev = mca_female[_n-1]
		
	#delimit ; //Run a cross-lagged probit model (see alternative at ver end of
					//this script).

	gsem 

		(OPPGONEG <- mca_prev FEMALE mca_female_prev OPPGONEG_prev  
		GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE 
		COOKS CHALL OPEN COOKS_CHALL COOKS_OPEN DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE 
		Y2002 Y2006, family(binomial) link(probit))
		
		(mca_sd <- OPPGONEG_prev mca_prev FEMALE mca_female_prev
		GONEG DEMOCRAT2 NONWHITE SENATE 
		COOKS CHALL OPEN COOKS_CHALL COOKS_OPEN DPOP2 DEDPERHS DINCMEDF 
		Y2002 Y2006, family(gaussian) link(identity)), 
		
		nocapslatent ;
		
	#delimit cr //Sample appears to be too small relative to the number of 
					//estimated parameters to give reliable results.
			
							
//Supplemental analysis: individual indicators for the main IV
probit OPPGONEG (i.BIOFAMIL2 i.BIOTOWN i.BIOOCCUP i.BIOORG ///
	i.BIOVOLUN i.BIOVET)##i.FEMALE GONEG c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 ///
	NONWHITE LOGFRAISED SENATE c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS ///
	DINCMEDF PRESNEGATIVE ib2004.YEAR, ///
	vce(cluster CANDNAME_RECODE)
est store interact
	
margins if e(sample), at(BIOFAMIL2=(0 1) FEMALE=(0 1)) predict(pr) asobserved post
test 1._at = 3._at
test 2._at = 4._at
test 1._at = 2._at
test 3._at = 4._at
	
est restore interact
margins if e(sample), at(BIOVOLUN=(0 1) FEMALE=(0 1)) predict(pr) asobserved post
test 1._at = 3._at
test 2._at = 4._at
test 1._at = 2._at
test 3._at = 4._at


//Version of the model with DV limited only to those candidates who received
	//*personal* negativity explicitly
tostring YEAR, gen(YEAR2)
gen HOUSE3 = HOUSE + YEAR2
egen OPPNEGPERSON = total(NEGPERSON) if flag==1, by(HOUSE3)
replace OPPNEGPERSON = OPPNEGPERSON - 1 if ((OPPNEGPERSON==1 & ///
	NEGPERSON==1) | OPPNEGPERSON==2)
replace OPPNEGPERSON = . if NEGPERSON==.
replace OPPNEGPERSON = . if (CANDNAME_RECODE == 542 & YEAR == 2006) //Missing data for Sam Johnson's 
													//competition

foreach i in 2004 2006 {
	probit OPPNEGPERSON c.mca_sd##i.FEMALE ///
		NEGPERSON c.SDISSUEOWNER##c.ISSSAL DEMOCRAT2 NONWHITE LOGFRAISED SENATE ///
		c.COOKS##(i.CHALL i.OPEN) DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
		if YEAR==`i', ///
		vce(cluster CANDNAME_RECODE) 
}
	/*Not statistically sig. BUT, here's the issue:
	the main model for Study #2 is only statisitically sig. for the 2002 election
	year---the one year in the dataset that does not code GONEG by personal vs.
	issue negativity. As such, for the one year that exhibits a statistically
	sig. relationship, there is no way to disentangle the personal from the issue
	negativity.*/
	
	
//Probably a more technically-correct cross-lagged probit model
keep OPPGONEG mca_sd FEMALE GONEG SDISSUEOWNER ISSSAL DEMOCRAT2 NONWHITE ///
LOGFRAISED SENATE COOKS CHALL OPEN DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE ///
YEAR CANDNAME

reshape wide OPPGONEG mca_sd GONEG SDISSUEOWNER ISSSAL DEMOCRAT2 LOGFRAISED ///
	SENATE COOKS CHALL OPEN DPOP2 DEDPERHS DINCMEDF PRESNEGATIVE NONWHITE, ///
	i(CANDNAME) j(YEAR)

gen COOKS_CHALL_02 = COOKS2002*CHALL2002
gen COOKS_CHALL_04 = COOKS2004*CHALL2004
gen COOKS_CHALL_06 = COOKS2006*CHALL2006

gen COOKS_OPEN_02 = COOKS2002*OPEN2002
gen COOKS_OPEN_04 = COOKS2004*OPEN2004
gen COOKS_OPEN_06 = COOKS2006*OPEN2006

gen mcaf_02 = mca_sd2002*FEMALE
gen mcaf_04 = mca_sd2004*FEMALE
gen mcaf_06 = mca_sd2006*FEMALE


#delimit ; //Run a cross-lagged probit model

gsem 

	(OPPGONEG2004 <- mca_sd2002 FEMALE mcaf_02 OPPGONEG2002 
	GONEG2004 c.SDISSUEOWNER2004##c.ISSSAL2004 DEMOCRAT22004 NONWHITE2004 
	LOGFRAISED2004 SENATE2004 COOKS2004 CHALL2004 OPEN2004 COOKS_CHALL_04 
	COOKS_OPEN_04 DPOP22004 DEDPERHS2004 DINCMEDF2004 PRESNEGATIVE2004, 
	family(binomial) link(probit))
	
	(OPPGONEG2006 <- mca_sd2004 FEMALE mcaf_04 OPPGONEG2004 
	GONEG2006 c.SDISSUEOWNER2006##c.ISSSAL2006 DEMOCRAT22006 NONWHITE2006 
	LOGFRAISED2006 SENATE2006 COOKS2006 CHALL2006 OPEN2006 COOKS_CHALL_06 
	COOKS_OPEN_06 DPOP22006 DEDPERHS2006 DINCMEDF2006 PRESNEGATIVE2006, 
	family(binomial) link(probit))
	
	(mca_sd2004 <- OPPGONEG2002 mca_sd2002 FEMALE mcaf_02
	GONEG2004 DEMOCRAT22004 NONWHITE2004 SENATE2004 
	COOKS2004 CHALL2004 OPEN2004 COOKS_CHALL_04 COOKS_OPEN_04 DPOP22004 
	DEDPERHS2004 DINCMEDF2004, 
	family(gaussian) link(identity))
	
	(mca_sd2006 <- OPPGONEG2004 mca_sd2004 FEMALE mcaf_04
	GONEG2006 DEMOCRAT22006 NONWHITE2006 SENATE2006 
	COOKS2006 CHALL2006 OPEN2006 COOKS_CHALL_06 COOKS_OPEN_06 DPOP22006 
	DEDPERHS2004 DINCMEDF2004, 
	family(gaussian) link(identity)),
	
	nocapslatent ;
	
#delimit cr // //Sample appears to be too small relative to the number of 
				//estimated parameters to give reliable results.

**************************
*** COMMANDS END HERE  ***
**************************
log close
exit
