#behaviour diversity analysis (analysis of individual behaviour frequency does not work, what does is behaviour diversity of each bird)

installed.packages("glmm")
install.packages("lme4")
install.packages("lmer")
install.packages("lmerTest")


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

#pseudo Rsquared
1-0.22058/0.57209

R <- princomp(scale(t(dat2[,4:ncol(dat2)])))$scores

L <- lmer( H ~ Weight..g. + (1|Age) + (1|Population), data=R)
summary(L)

rand(L)


plot(R, cex = 0, pch = 21, xlab = 'Variable X', ylab = 'Variable Y')
text(R, labels = rownames(R))






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





