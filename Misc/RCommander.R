
oj <- read.table("/Users/sathishrajendiran/Downloads/oj.csv", header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)
names(oj)


with(oj, Hist(price, scale="frequency", breaks="Sturges", col="darkgray"))
with(oj, Hist(price, scale="percent", breaks="Sturges", col="darkgray"))
with(oj, Hist(price, scale="density", breaks=5, col="darkgray"))
with(oj, Hist(price, scale="density", breaks=5, col="darkgray"))
Boxplot(price~brand, data=oj, id=list(method="y"))
scatterplot(logmove~price, regLine=FALSE, smooth=FALSE, boxplots=FALSE, data=oj)
scatterplot(logmove~price, regLine=FALSE, smooth=list(span=0.5, spread=FALSE), 
  boxplots=FALSE, ellipse=list(levels=c(.5, .9)), data=oj)
scatterplot(logmove~price | brand, regLine=FALSE, smooth=list(span=0.5, 
  spread=FALSE), boxplots=FALSE, ellipse=list(levels=c(.5, .9)), by.groups=TRUE, 
  data=oj)
scatterplotMatrix(~INCOME+logmove+price, regLine=TRUE, smooth=list(span=0.5, 
  spread=TRUE), diagonal=list(method="density"), data=oj)
with(oj, plotMeans(price, brand, error.bars="sd", connect=TRUE))
library(lattice, pos=17)
xyplot(logmove ~ price | brand, groups=brand, type="p", pch=16, 
  auto.key=list(border=TRUE), par.settings=simpleTheme(pch=16), 
  scales=list(x=list(relation='same'), y=list(relation='same')), data=oj)
library(rgl, pos=18)
library(nlme, pos=19)
library(mgcv, pos=19)
scatter3d(logmove~AGE60+price, data=oj, fit="linear", residuals=TRUE, bg="white",
   axis.scales=TRUE, grid=TRUE, ellipsoid=FALSE)
scatter3d(logmove~AGE60+price|brand, data=oj, fit="linear", residuals=TRUE, 
  parallel=FALSE, bg="white", axis.scales=TRUE, grid=TRUE, ellipsoid=FALSE)
dev.print(pdf, file="/Users/sathishrajendiran/Downloads/RGraph.pdf", width=10.4, 
  height=4.8, pointsize=12)
library(abind, pos=21)
library(e1071, pos=22)
numSummary(oj[,c("logmove", "price"), drop=FALSE], statistics=c("mean", "sd", 
  "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))
numSummary(oj[,c("logmove", "price"), drop=FALSE], groups=oj$brand, 
  statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))
cor(oj[,c("INCOME","logmove","price")], use="complete")
with(oj, cor.test(logmove, price, alternative="two.sided", method="pearson"))
library(mvtnorm, pos=23)
library(survival, pos=23)
library(MASS, pos=23)
library(TH.data, pos=23)
library(multcomp, pos=23)
AnovaModel.1 <- aov(price ~ brand, data=oj)
summary(AnovaModel.1)
with(oj, numSummary(price, groups=brand, statistics=c("mean", "sd")))
AnovaModel.2 <- aov(price ~ brand, data=oj)
summary(AnovaModel.2)
with(oj, numSummary(price, groups=brand, statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.2, linfct = mcp(brand = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs)) # confidence intervals
  print(cld(.Pairs)) # compact letter display
  old.oma <- par(oma=c(0,5,0,0))
  plot(confint(.Pairs))
  par(old.oma)
})
RegModel.3 <- lm(logmove~AGE60+INCOME+price, data=oj)
summary(RegModel.3)
LinearModel.4 <- lm(logmove ~ AGE60 + INCOME + price + brand, 
  data=oj)
summary(LinearModel.4)
LinearModel.5 <- lm(logmove ~ AGE60 + INCOME + price + brand + 
  price*brand, data=oj)
summary(LinearModel.5)
RegModel.6 <- lm(logmove~AGE60+INCOME+price, data=oj)
summary(RegModel.6)
library(zoo, pos=28)
library(lmtest, pos=28)
resettest(logmove ~ AGE60 + INCOME + price, power=2:3, 
  type="regressor", data=oj)
vif(RegModel.6)
round(cov2cor(vcov(RegModel.6)), 3) 
  # Correlations of parameter estimates
bptest(logmove ~ AGE60 + INCOME + price, varformula = ~ 
  fitted.values(RegModel.6), studentize=FALSE, data=oj)
dwtest(logmove ~ AGE60 + INCOME + price, alternative="greater", 
  data=oj)
outlierTest(RegModel.6)

