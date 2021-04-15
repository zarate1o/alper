program alper
/*
 /﹋\
(҂`_´)         ________
<,︻╦╤─ ҉ - -  | LATEX |
/﹋\           ¯¯¯¯¯¯¯¯ 

*/

cd "D:\Users\suuser\Desktop\PS3"
use "Problem Set III.dta", clear   
save "Problem Set III_v2.dta", replace
use "Problem Set III_v2.dta", clear

*Q3
recode condition (2=1 "Treatment") (1=0 "Control"), gen(bi_condition)
lab var bi_condition "Treatment"

*Q4
gen trumpvclinton = trumpft-clintonft

*Q5
eststo m1: reg trumpvclinton racresent oldfash if bi_condition==0, noomit
eststo m2: reg trumpvclinton racresent oldfash if bi_condition==1, noomit

*Q6
gen txrs = bi_condition*racresent
lab var txrs "Treatment $\times$ Racial Resentment"
gen txof = bi_condition*oldfash
lab var txof "Treatment $\times$ Old-Fashioned Racism"
eststo m3: reg trumpvclinton bi_condition racresent oldfash txrs txof, noomit

*Q7
eststo m4: reg trumpv* i.condition##c.racresent i.condition##c.oldfash, noomit

*Q8
esttab m* using Q8.tex, replace cells(b(star fmt(3)) se(par fmt(2))) legend label nonumbers varlabels(_cons Constant) stats(r2 N, fmt(3 0)  label("\$R^{2}$")) mtitle("Model 1" "Model 2" "Model 3" "Model 4") longtable noomit title("The Effects of the Explicit Politician Condition on Difference in Trump/Clinton Feeling Thermometer\label{Q8}") collab(none)

*Q9
est rest m3
sum oldfash if e(sample)
rename (oldfash bi_condition txof racresent txrs) (or_oldfash or_bi_condition or_txof or_racresent or_txrs)
recode or_oldfash (nonm=`r(mean)'), gen(oldfash)
sum or_ra* if e(sample),d
gen racresent=((_n-1)*.05)+round(`r(min)', 0.05)
replace racresent=. if _n>((round(`r(max)', 0.05)-round(`r(min)', 0.05))/.05)+1
gen bi_condition=0 
replace bi_condition=1 if mod(_n,2)
replace bi_condition=. if _n> ((round(`r(max)', 0.05)-round(`r(min)', 0.05))/.05)+1
gen txof = bi_condition*oldfash
lab var txof "Treatment $\times$ Old-Fashioned Racism"
gen txrs=bi_condition*racresent
lab var txrs "Treatment $\times$ Racial Resentment"
predict yhat, xb
predict stdp, stdp
gen ub=yhat+invttail(`e(df_r)', .05)*stdp
gen lb=yhat+invttail(`e(df_r)', .95)*stdp

*Q10
twoway  (rarea ub lb racresent if bi_condition==1, color(grey%50) sort)(rarea ub lb racresent if bi_condition==0, color(grey%50) sort)  (line yhat racresent if bi_condition==1, lcolor(red%50) sort) (line yhat racresent if bi_condition==0, lcolor(blue%50) sort) (hist or_racresent if e(sample), yaxis(2) percent lcolor(gs12) fcolor(gs12%25)), scheme(plotplain) ///
xtitle("Racial Resentment") ytitle("Affect toward Difference in" "Trump/Clinton Feeling Thermometer" " ") ytitle("Racial Resentment Percentage", axis(2)) ///
legend(pos(6) rows(4) order(1 "90% Confidence Intervals" 3 "Racial Resentment when Condition=Treatment" 4 "Racial Resentment when Condition=Control" 5 "Racial Resentment Distribution"))  
graph export Q10.pdf, replace

*Q12
est rest m3
test txof

*Q13
replace racresent=or_racresent
replace oldfash=or_oldfash
lab var oldfash "Old-Fashioned Racism"
lab var racresent "Racial Resentment"
eststo q5: reg trumpvclinton oldfash racresent c.oldfash#c.racresent if bi_condition==0

*Q14
gen rac_old=racresent*oldfash
reg trumpvclinton racresent oldfash rac_old  
matrix b=e(b)
matrix V=e(V)
scalar b1=b[1,1]
scalar b3=b[1,3]
scalar varb1=V[1,1]
scalar varb3=V[3,3]
scalar covb1b3=V[1,3]
sum oldfash if e(sample),d
generate fake_oldf=((_n-1)*.125)+round(`r(min)', 0.125)
replace fake_oldf=. if _n>((round(`r(max)', 0.125)-round(`r(min)', 0.125))/.125)+1
gen conbx=b1+b3*fake_oldf if _n<=((round(`r(max)', 0.125)-round(`r(min)', 0.125))/.125)+1  									/* Marginal Effects*/
gen consx=sqrt(varb1+varb3*(fake_oldf^2)+2*covb1b3*fake_oldf) if _n<=((round(`r(max)', 0.125)-round(`r(min)', 0.125))/.125)+1		/* Standard Errors*/
gen contx=conbx/consx																									/* T-Statistics*/
gen upperx=conbx+invttail(`e(df_r)', .025)*consx
gen lowerx=conbx+invttail(`e(df_r)', .975)*consx

*Q15
estimates restore m3				
margins, dydx(racresent) at(oldfash=(0 1))

*Q16
twoway  (rarea upperx lowerx fake_oldf, color(grey%50) sort) (line conbx fake_oldf, lcolor(red%50) sort), scheme(plotplain) ///
xtitle("Racial Resentment") ytitle("Affect toward Difference in" "Trump/Clinton Feeling Thermometer") ///
legend(pos(6) rows(2) order(1 "95% Confidence Interval" 2 "Marginal Effect of Racial Resentment"))
graph export Q16.pdf, replace

*Q18
lab var oldfash "Old-Fashioned Racism"
lab var racresent "Racial Resentment"
eststo q6: reg trumpvclinton c.racresent##c.oldfash if bi_condition==1

*Q19
reg trumpvclinton racresent oldfash
sum racresent,d
disp `r(mean)'-2*`r(sd)'
disp `r(mean)'+2*`r(sd)'
disp 2*`r(sd)'
margins, at(racresent=(.07456117(.51111438)1.0967900) (asobs) oldfash)

*Q20
marginsplot, scheme(plotplain) recastci(rarea) recast(line) plot1opts(lpattern(dash) lcolor(gs10)) addplot(hist oldfash, below percent lcolor(gs12) fcolor(gs12%25) yaxis(2))  xtitle("Racial Resentment") xlabel(.0745612 "5th Percentile" .5856755 "Mean" 1.09679 "95th Percentile", angle(vertical)) ytitle("Marginal Effect of Old-Fashioned" "Racism on Difference in" "Trump/Clinton Feeling Thermometer") title("The Effects of the Old-Fashioned Racism on" "Difference in Trump/Clinton Feeling Thermometer across Racial Resentment") legend(on pos(6) rows(2) order(1 "Marginal Effect of Old-Fashioned Racism" 2 "Distribution of Old-Fashioned Racism")) legend(off)
graph export Q20.pdf, replace


*Q22
esttab q* using Q22.tex, replace cells(b(star fmt(3)) se(par fmt(2))) legend label nonumbers varlabels(_cons Constant) stats(r2 N, fmt(3 0)  label("\$R^{2}$")) mtitle("Model 1" "Model 2") longtable noomit title("Difference in Trump/Clinton Feeling Thermometer on Old-Fashioned Racism and Racial Resentment\label{Q22}") collab(none)

save "Problem Set III_v3.dta", replace
/*

      ________
     | LATEX |                                           c=====ↄ
      ¯¯¯¯¯¯¯¯                                              H
   ____________                                         _,,_H__
  (__((__((___()                                       //|     |
 (__((__((___()()_____________________________________// |ACME |
(__((__((___()()()------------------------------------'  |_____|
*/

end alper program
