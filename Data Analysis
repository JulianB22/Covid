#FACTOR ANALYSIS
#create a new dataset with just IVS
COVIDFA <- read.csv("COVIDFA.csv", stringsAsFactors = FALSE)

#KMO TEST BOX4
# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
KMO(cor(COVIDFA))

# get eigenvalues: eigen() uses a correlation matrix
ev <- eigen(cor(COVIDFA))
ev$values
# plot a scree plot of eigenvalues
#FIGURE 7
plot(ev$values, type="b", col="blue", xlab="variables")
# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", main = "Eigenvalues",
      xlab="number of components", ylab ="cumulative proportion")

# Varimax Rotated Principal Components
# principal() uses a data frame or matrix of correlations
fit <- principal(COVIDFA, nfactors=3, rotate="varimax")
fit

attach(COVID)

# Test dependent variable for normality
# graphically FIGURE 8
qqnorm(Covid_Deaths, xlab = "Theoretical Quantiles: Covid_Deaths" )
qqline(Covid_Deaths, col = 2) ## red color
# K-S test 
ks.test(Covid_Deaths, "pnorm", mean(Covid_Deaths), sd(Covid_Deaths))
# or... Shapiro-Wilk's test
shapiro.test(Covid_Deaths)
