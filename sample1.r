#R Data Analysis Examples: Robust Regression
require(foreign)
require(MASS)

#Description of the example data----
cdata <- read.dta("http://www.ats.ucla.edu/stat/data/crime.dta")
summary(cdata)

#Using robust regression analysis----
summary(ols <- lm(crime ~ poverty + single, data = cdata))

#plot results----
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)

#Test interaction term
summary(ols2 <- lm(crime ~ poverty + single + single*poverty, data = cdata))

# added a comment