dataset=read.csv("JobPreference.csv") # "JobPreference.csv","disasterLoss.csv","crime.csv"
head(dataset)

Y=dataset$Family
X=dataset$Challenge
dev.new();plot(X,Y,ylim = c(0,10),xlim = c(2,10),xlab="Family",ylab="Challenge");text(X, Y, dataset$Company, cex=1, pos=3, col="red")


newData=dataset[,-c(1,2)] 

rownames(newData)=dataset[,1]  # Establish row ID by rownames
.PC= princomp(newData ,cor=TRUE)

dev.new();biplot(.PC,choices = c(1,2),cex=0.8,xlab="PC1",ylab="PC2");abline(h=0,v=0,lty=3,col=("blue"))
dev.new();biplot(.PC,choices = c(1,3),cex=0.8,xlab="PC1",ylab="PC3");abline(h=0,v=0,lty=3,col=("blue"))
dev.new();biplot(.PC,choices = c(2,3),cex=0.8,xlab="PC2",ylab="PC3");abline(h=0,v=0,lty=3,col=("blue"))


dev.new();plot(.PC, col=c("blue"))
dev.new();screeplot(.PC, col=c("lightblue"))

names(summary(.PC, loadings=TRUE))
sigma=(summary(.PC, loadings=TRUE)$sdev)^2
varianceRatio=t(round(sigma/sum(sigma),4))
cumsumVAR=t(cumsum(round(varianceRatio,4)))
colnames(cumsumVAR)=colnames(varianceRatio)=paste0("PC",1:ncol(cumsumVAR))
rownames(varianceRatio)="Proportion Var"
rownames(cumsumVAR)="Cumulative Var"

.PC$sdev

score=predict(.PC) #Both of them are equal. 

round(loadings(.PC),5)
round(unclass(loadings(.PC))[,2],4)
Loadings=round(unclass(loadings(.PC)),4)
colnames(Loadings)=paste0("PC",1:ncol(score))
round(rbind(Loadings,varianceRatio,cumsumVAR),4)
write.csv(Loadings,file=paste0(path,"Loadings.csv"))

cor(score)
cor.test(score[,4],score[,5])

