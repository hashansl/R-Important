library(heplots)
data(RootStock)
head(RootStock)
str(RootStock)

#library(devtools)
#install_github("cran/MVN")
library(tidyr)

library(MVN)
mvn(data=RootStock[,2:5], mvnTest = "mardia")

# Adjusted chi-square Q-Q plot
mvn(data=RootStock [,2:5], mvnTest="mardia",multivariateOutlierMethod="adj")
# chi-square Q-Q plot
mvn(data = RootStock [,2:5], mvnTest = "mardia",multivariatePlot="qq")

library(biotools)
boxM(RootStock[2:5],RootStock[,1])

root.manova <- lm(cbind(RootStock$girth4, RootStock$ext4, RootStock$girth15,
                        otStock$weight15)~ rootstock, data = RootStock)
Manova(root.manova)

Manova(root.manova, test='Wilks')
Manova(root.manova, test='Hotelling-Lawley')
Manova(root.manova,test='Roy')


# H and E for Pillai test
summary(Manova(root.manova))

# H and E for Wilks test
summary(Manova(root.manova), test='Wilks')
