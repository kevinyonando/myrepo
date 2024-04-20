#behaviour diversity analysis (analysis of individual behaviour frequency does not work, what does is behaviour diversity of each bird)

installed.packages("glmm")
install.packages("lme4")
install.packages("lmer")
install.packages("lmerTest")
install.packages("vioplot")


dat <- read.csv("Crow_Data2.csv", header=TRUE) 

dat2 <- dat[,-c(1,7,12,13,16,17,19,21,23,25,27,28,29,31,33,35,37)]

table(dat2$Age)


H <- array()
for(i in 1:12){
  F = dat2[i, 4:ncol(dat2)]/sum(dat2[i, 4:ncol(dat2)])
  H[i] = sum(-F*log(F),na.rm = TRUE)
}

H


summary(glm(H~Weight..g. + Population + Age,data= dat2))

#t-value from 4 above means that we are in good grounds, if the t-value is 3 below then we should have another think about our analysis

#pseudo Rsquared
1-0.22058/0.57209

R <- princomp(scale(t(dat2[,4:ncol(dat2)])))$scores

L <- lmer( H ~ Weight..g. + (1|Age) + (1|Population), data=dat2)
summary(L)

rand(L)

#negative means they do less stuff and positivew they do more behaviour
ranef(L)



#the far ones (they do not/ may not do other behaviour art the same time), the squished one is behaviours that happened often at the same time

plot(R, cex = 0, pch = 21, cex.lab = 1.2, bty = "L", xlab = 'Principal Component 1', ylab = 'Principal Component 2')
text(R, cex = colSums(dat2[,4:ncol(dat2)])^0.35/5, xpd = NA, labels = rownames(R))



vioplot(dat2[,4:ncol(dat2)], cex.axis = 0.65, las = 3, col=hsv(1:17/17),  ylab = "Frequency (n)")


# manual page is ?plot.hclust

d =  dist(t(dat2[,4:ncol(dat2)]))
plot(hclust(d, method="average"), main = "Cluster Dendrogram", ylab = "Height")

#remove all the writing on the diagram, including main y lab and the one at the bottom (ann = FALSE)
     
#y axis distance means that the lower you go, the less common it is = more rare, and the higher you are on the y-axis = more common   








#other codes that might be useful in the future ----------------------------------------------------------


n = c(rep(0,6),rep(1,6))

summary(lm(s[,1] ~ n))

lmer(s[,1] ~ 0 + (1 | as.factor(n)))

is.matrix(s)

d = data.frame(s[,1],n)
d
nlme(s[,1] ~ n,random=n ~ 1)





cbind(1:ncol(dat),colnames(dat))
cbind(1:ncol(dat2),colnames(dat2))
col(dat2)[dat2 <0 ]



R <- princomp(scale(t(dat2[,4:ncol(dat2)])))$loadings
R = cbind(as.matrix(dat[1:12,2:4]),R)
R=data.frame(R)

for(i in 3:ncol(R))
  R[,i]=as.numeric(R[,i])

z <- array()

for(i in 4:ncol(R)){
  sky = paste(colnames(R)[i],"~ Weight..g. ")
  
  z[i] <- summary(glmm( formula(sky) ,random = list(~0 + Population, ~0+ Age), varcomps.names = c("Population","Age"), data=R, family.glmm = poisson.glmm, m=100))$nucoefmat[1,4] 
}


s <- summary(glmm( formula(sky) ,random = list(~0 + Population, ~0+ Age), varcomps.names = c("Population","Age"), data=dat2, family.glmm = poisson.glmm, m=100))





