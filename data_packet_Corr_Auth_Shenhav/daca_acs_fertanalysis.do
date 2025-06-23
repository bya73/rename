** Set globals to directories
global figures "C:\Users\YOGA\Desktop\metria\expo\data_packet_Corr_Auth_Shenhav\figures"
global tables "C:\Users\YOGA\Desktop\metria\expo\data_packet_Corr_Auth_Shenhav\tables"
global data "C:\Users\YOGA\Desktop\metria\expo\data_packet_Corr_Auth_Shenhav\data"

** e-class program for automating tables with SE adjustment
capture program drop myrepost
program myrepost, eclass
ereturn repost V = `1'			
end

set matsize 10000
set more off, perm


** Read in data

use $data/daca_acs.dta, clear

/*************************************
Descriptive Fertility Graphs
*************************************/ 

** Compare likelihood of not having child for:
** Hispanic born outside US, citizen
** Hispanic born outside US, non-citizen
	
preserve 

collapse child0 [aweight=perwt], by(everimmig noncitizen year)


gr tw  ///
	line child0 year if everimmig==1 & noncitizen ==0,  lcolor(black) lp(dash) || ///
	line child0 year if everimmig==1 & noncitizen ==1,  lcolor(black) lp(solid) scheme(s1mono) ///
	legend( label(1 "Hispanic Immig. Citizens") label(2 "Hispanic Immig. Non-Citizens") ) xtitle("Year") yline(2011.5, lcolor(red) lp(dash)) ///
	ytitle("Share of 15-20 with No Children")

gr export $figures/figure1.pdf, replace

restore	



***** Regressions

/*****************************************
Prep Data
*******************************************/

// Limit to:
* (1) eligible or citizens that arrived before 2007 & before age 16
* (2) arrived before age 10 
* (3) foreign-born (should be implied by 1+2)

keep if eligible==1|c_b42007_b416==1
keep if ageimmig<=10
keep if yrimmig!=0 & yrimmig!=.

/**********************************************
Regressions
***********************************************/

** (1) First set of regressions removes trends from data

// controls
local controls 		"noncitizen_p gr_ageimmig*p gr_yrim*p gr_ag*p  gr_st*p gr_arr*p  "

// need to make dummies in advance for de-trending

** age-year, yrimmig
foreach x in ageimmig yrimmig ageyr { 
	qui tab `x', gen(gr_`x')
}

tab state, g(s)
local maxstate = r(r)
tab year, g(y)
		local maxyear = r(r)

** state-year fe
forvalues s= 1/`maxstate' {
forvalues y = 1/`maxyear' {
	gen gr_st`s'yr`y' = s`s'*y`y'
}
}

qui tab ageimmig
	local maxagearr = r(r)
qui tab yrimmig
	local maxyrimmig = r(r)

** age-immig-noncitizen fe
forvalues arr= 1/`maxagearr' {
	gen gr_arrag`arr'noncit = gr_ageimmig`arr'*noncitizen
	
}


eststo clear

foreach outcome in fertyr nchild child0 {
									
		
	** Estimate trend pre-2012 then residualize
	foreach x of varlist `outcome' eligible noncitizen  gr_* {
		qui xi: reg `x' eligible year_elig year_nonelig if year<2012 [aweight=perwt]
		predict `x'_p, residuals
	}	
				
				
	eststo `outcome': xi: reg `outcome'_p eligiblepost eligible_p  `controls'  [pweight=perwt], cluster(statefip)

	** Re-calculate s.e. taking account of estimated parameters for trends

	local dof = e(df_r)
	local se_eligiblepost = _se[eligiblepost]*((`dof')/(`dof'-3))^.5

	*Get dimensions of variance matrix
	matrix v_copy = e(V)					
	matrix v_calc = J(rowsof(v_copy), colsof(v_copy), 0)
	matrix v_calc[1,1] = (`se_eligiblepost')^2
					

	*run program at beginning of dofile to replace e(v) with adjusted se
	myrepost  v_calc
	eststo `outcome' // store again		
	
	drop *_p

}


	esttab  fertyr child0 nchild ///
	using "${tables}/table1.tex", replace keep(eligiblepost) se r2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant /// 
	stat(, labels( "") fmt(3 0)) mtitle("Last Year" "Zero" "Number") postfoot("") ///
	mgroups("Children", pattern( 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	varlabels(eligiblepost "Elig*Post" , blist(eligiblepost "\midrule \it{\underline{A. With Trend}} \\ "))	


					

** (2) Second set of regressions does not remove trends from data

eststo clear

local controls i.age*i.year i.yrimmig i.statefip*i.year i.ageimmig*i.noncitizen 

foreach outcome in fertyr nchild child0 {
	
	eststo `outcome': xi: reg `outcome' eligiblepost eligible  `controls'  [pweight=perwt] , cluster(statefip)
	summ `outcome' if eligible==1 & `outcome' !=.
	estadd scalar eligmean = r(mean)

	}
	
		
	esttab  fertyr child0 nchild ///
	using "${tables}/table1.tex", append keep(eligiblepost) se r2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant /// 
	stat(eligmean N, labels("Elig Mean" "Individuals") fmt(3 0)) nomtitle prehead("") posthead("") ///
	varlabels(eligiblepost "Elig*Post" , blist(eligiblepost "\midrule \it{\underline{B. Without Trend}} \\ "))	


				
