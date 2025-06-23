** Set globals to directories
global data "C:\Users\YOGA\Desktop\metria\expo\data_packet_Corr_Auth_Shenhav\data"
global tables "C:\Users\YOGA\Desktop\metria\expo\data_packet_Corr_Auth_Shenhav\tables"


** e-class program for automating tables with SE adjustment
capture program drop myrepost
program myrepost, eclass
ereturn repost V = `1'			
end

use "$data/daca_yrbss.dta", replace

eststo clear

// need to make dummies in advance for de-trending

foreach x in year statefip  { 
	qui tab `x', gen(gr_`x')
}

gen year_elig=0
	replace year_elig=year if highhispelig==1
gen year_nonelig=0
	replace year_nonelig=year if highhispelig==0
	
	
eststo clear
foreach outcome in noprot condom pill withd sexact iudshot notsure  eversex {

	
	*** Detrend data first
	foreach x of varlist `outcome' highhispelig gr_* {
		qui xi: reg `x' highhispelig year_elig year_nonelig if year<2012 [aweight=weight]		
		predict `x'_p, residuals
	}					
				
	
	*DND
		eststo `outcome': xi: reg `outcome'_p highhispeligpost highhispelig_p gr_* [aweight=weight], cluster(statefip)
		estadd ysumm
		sum `outcome' if highhispelig== 1
		estadd scalar eligmean = r(mean)
		
	** Re-calculate s.e. taking account of estimated parameters for trends
		local dof = e(df_r)
		local se_hispeligpost = _se[highhispeligpost]*((`dof')/(`dof'-3))^.5
					
	** Re-calculate s.e. taking account of estimated parameters for trends
		matrix v_copy = e(V)					
		matrix v_calc = J(rowsof(v_copy), colsof(v_copy), 0)
		matrix v_calc[1,1] = (`se_hispeligpost')^2
					
	** Run program at beginning of dofile to replace e(v) with adjusted se
		myrepost  v_calc
		eststo `outcome' // store again

	cap drop *_p
						

}


	esttab noprot pill condom iudshot withd  eversex sexact using ///
	"$tables/table2.tex", ///
	replace keep(high*post ) se r2 se(3) b(3)  star (* .10 ** .05 *** .01) nonum nonotes noconstant ///
	stat(eligmean N, labels("Eligible Mean" "Individuals") fmt(3 0)) label	///
	mtitles("None" "Pill" "Condom" "IUD/Shot" "Withdraw/Oth."  "Ever" "Last 3 Mos.") ///
	 mgroups("Last Time Had Sex, Pregnancy Protection:" "Had Sex", pattern(1 0 0 0 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))





