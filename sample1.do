use http://www.ats.ucla.edu/stat/stata/dae/crime, clear
summarize crime poverty single

regress crime poverty single

lvr2plot, mlabel(state)

predict d1, cooksd
clist state crime poverty single d1 if d1>4/51, noobs
