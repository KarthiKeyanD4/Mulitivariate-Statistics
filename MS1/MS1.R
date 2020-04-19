## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(ggcorrplot)
library(RMaCzek)


## ----echo=FALSE----------------------------------------------------------
sf <- read.csv('./T1-9.csv')
sf1 <- sf[,2:8]
mean = colMeans(sf[sapply(sf, is.numeric)])
mean
variance = var(sf[,-1])


## ----echo=FALSE----------------------------------------------------------
deviation = apply(as.matrix(sf[,-1]),2, sd)
deviation


## ----echo=FALSE,out.height='15%'-----------------------------------------
 x <- colnames(sf[,2:8])
 for(i in 2:8){
  h <-hist(sf[,i], breaks=10, col="red", xlab=x[i],
           main="Histogram")
  xfit <- seq(min(sf[,i]),max(sf[,i]),length=15)
  yfit <- dnorm(xfit,mean=mean(sf[,i]),sd=sd(sf[,i]))
  yfit <- yfit*diff(h$mids[1:2])*length(sf[,i])
  lines(xfit, yfit, col="blue", lwd=2)
}


## ---- out.height='15%',echo=FALSE----------------------------------------
sf_variance <- var(sf[,2:8])
sf_variance
sf_correlation <- cor(sf[,2:8])
sf_correlation
  ggcorrplot::ggcorrplot(sf_correlation, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle",
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlation between different RaceTrack")



## ----scatterplot,out.height='15%',echo=FALSE-----------------------------
colna <- colnames(sf)
for(i in 2:8)
   for(j in 2:8)
       if(j!=i & j>i){
       plot(x = sf[,i], y = sf[,j],col = "red", pch = 19,cex = 1,lty = "solid",lwd = 2,xlab = colna[i],ylab = colna[j]) 
       text(x =  sf[,i],y = sf[,j],labels = as.character(sf[,1]), pos = 1)
       }


## ----echo=FALSE----------------------------------------------------------
sf$X100m_z <- round((sf$X100m - mean(sf$X100m))/sd(sf$X100m), 2)  
sf$X100m_type <- ifelse(sf$X100m_z < 0, "below", "above") 
sf <- sf[order(sf$X100m_z), ]
sf$countries <- factor(sf$countries, levels = sf$countries)
ggplot(sf, aes(x=countries, y=X100m_z, label=X100m_z)) + 
  geom_bar(stat='identity', aes(fill=X100m_type),width = 0.75)  +
  scale_fill_manual(name="Speed", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised Speed of 100 meters of Different Countries", 
       title= "Diverging Bars") + xlab("countries") + ylab("100m") + 
  coord_flip() +  theme(axis.text.x = element_text(size=10),
                        axis.text.y = element_text(size=5))



## ----echo=FALSE----------------------------------------------------------
ggplot(sf, aes(x=sf$countries, y=sf$X100m)) + 
  geom_point(col="tomato2", size=3) +   
  geom_segment(aes(x=sf$countries, 
                   xend=sf$countries, 
                   y=min(sf$X100m), 
                   yend=max(sf$X100m)), 
               linetype="dashed", 
               size=0.1) +  
  labs(title="Dot Plot", 
       subtitle="X100m vs Countries") +  xlab("countries") + ylab("100m") +
  coord_flip() + theme(axis.text.x = element_text(size=10),
                    axis.text.y = element_text(size=5))


## ----echo=FALSE,eval=TRUE------------------------------------------------
centered=scale(x = as.matrix(sf[,2:8]), center = TRUE, scale = FALSE)
squared=centered%*%t(centered)
rownames(squared)<-sf[,1]
colnames(squared)<-sf[,1]
diagonal<-diag(squared)
extrems=order(diagonal, decreasing=TRUE)[1:5]
cat("Countries which have the extreme values",as.character(sf[extrems,1]))
cat("Sweden's rank",which(names(diagonal[order(diagonal, decreasing=TRUE)])=="SWE"))


## ----echo=FALSE,eval=TRUE------------------------------------------------
centered=scale(x = as.matrix(sf[,2:8]), center = TRUE, scale = FALSE)
variance<-var(sf[,2:8])
V<-variance*diag(7)
distance2<-centered%*%solve(V)%*%t(centered)
diagonal<-diag(distance2)
names(diagonal)<-sf[,2:8]
extrems=order(diagonal, decreasing=TRUE)[1:5]
cat("Countries which have the extreme values",as.character(sf[extrems,1]))
cat("Sweden's rank",which(names(diagonal[order(diagonal, decreasing=TRUE)])=="SWE"))


## ----echo=FALSE,eval=TRUE------------------------------------------------
centered=scale(x = as.matrix(sf[,2:8]), center = TRUE, scale = FALSE)
covariance_mat=cov(sf[,2:8])
mahalnob<-centered%*%solve(covariance_mat)%*%t(centered)
diagonal<-diag(mahalnob)
names(diagonal)<-sf[,1]
extrems=order(diagonal, decreasing=TRUE)[1:5]
cat("Countries which have the extreme values",as.character(sf[extrems,1]))
cat("Sweden's rank",which(names(diagonal[order(diagonal, decreasing=TRUE)])=="SWE"))


## ----echo=FALSE,eval=TRUE,out.height='500%',out.width='110%',fig.align='center'----
suppressWarnings(suppressMessages(library(RMaCzek)))
#Czekanowski's diagram
rownames(sf)<-sf[,1]
x<-as.matrix(sf[,2:8])
mat<-czek_matrix(x)
plot(mat)


## ----code = readLines(knitr::purl("C/Users/KarthiKeyan/Documents/Machine Learning/MS1.Rmd",documentation = 1)),echo=TRUE,eval=FALSE----
## 

