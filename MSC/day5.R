library("readxl")
barsteel <- read_excel("barsteel.xlsx")

library(biotools)
boxM(barsteel[1:2],barsteel[,3])

library(MVN)
mvn(data = barsteel[,1:2], mvnTest = "mardia")

mvn(data = barsteel[,1:2], mvnTest = "mardia",multivariatePlot="qq")
mvn(data=barsteel[,1:2], mvnTest="mardia",multivariateOutlierMethod="adj")

# Consider the interaction effects as well
barsteel.mod <- lm(cbind(torque, strain)~velocity*lubricants, data=barsteel_clea
                   n_mcd)
Manova(barsteel.mod, test.statistic="Roy")
