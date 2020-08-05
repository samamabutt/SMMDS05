


X = as.matrix(read.table("E:/MS DS/SMMDS/Assignment 5/ocr.txt", quote="\"", comment.char="")) 
ocr = as.matrix(read.table("E:/MS DS/SMMDS/Assignment 5/ocr.lables", quote="\"", comment.char=""))
r = X[1,]            #first digit image, i.e., image in row 1 
im = matrix(r,nrow=16,byrow=TRUE)    #convert vector to image 
image(im[,ncol(im):1])   #view image Similarly you can view any image you like for any row of train and test matrices 

XC = X - rep(colMeans(X), rep.int(nrow(X), ncol(X)))


B = XC%*%t(XC)

getEigValues<-function(ei,head, tail, numb){
  
  eigenValue = sort(ei$values, decreasing = TRUE)
  if(head){
    eigenValue = head(eigenValue, numb)
  }
  if(tail){
    eigenValue = tail(eigenValue, numb)
  }
  return(eigenValue)
}
eigValue<-getEigValues(eigen(B),TRUE,FALSE,2)

S<-diag(sqrt(eigValue),2,2)
V<-getEigVectors(eigen(B),getEigValues(eigen(B),TRUE,FALSE,2))
getEigVectors<-function(ei,eigValue){
L<-c()
for (i in eigValue){
  L<-cbind(L,c(ei$vectors[ei$values == i]))
 
}
return (L)
}


Z = V%*% S

Z<-cbind(Z,c(ocr))

labels = Z[,ncol(Z)]  	

twoClass = labels==2			
threeClass = labels==3		
fourClass = labels==4		
twoDat = Z[twoClass,]			
threeDat = Z[threeClass,]		
fourDat = Z[fourClass,]			

plot(twoDat[,1],twoDat[,2],col="blue",xlab="Feature 1",ylab="Feature 2")  
points(threeDat[,1],threeDat[,2],col="green",pch=24) 
points(fourDat[,1],fourDat[,2],col="red",pch=3)

legend("topleft",c("Digit 2","Digit 3","Digit 4"),col=c("blue","green","red"), cex=0.8,pch=1:3)

P<-cbind(X,c(ocr))
class = P[,ncol(P)]
twolabel = class==2
twoRec = P[twolabel,]

threelabel = class==3
threeRec = P[threelabel,]

X2 = twoRec[,-ncol(twoRec)]

X3 = threeRec[,-ncol(threeRec)]

getEigVectors2<-function(ei,flag,numb){
  
  comb_two = data.frame(t(ei$vectors), ei$values)
  comb_reversed_two = comb_two[order(comb_two[,ncol(comb_two)],decreasing=TRUE),]
  if(flag==TRUE){
    result<-head(comb_reversed_two,numb)
  }
  if(flag==FALSE){
    result<-tail(comb_reversed_two,numb)
  }
  
  return (result)
  
}

picture <- function(rt) {
rt =  rt[1:(length(rt) - 1)]
for (j in 1:nrow(rt)){
r = rt[j,]


im = matrix(unlist(r), nrow = 16, byrow = TRUE)

image(im[,ncol(im):1])
}
}

picture(getEigVectors2(eigen(cov(X2)),TRUE,4))
picture(getEigVectors2(eigen(cov(X2)),FALSE,4))
picture(getEigVectors2(eigen(cov(X3)),TRUE,4))
picture(getEigVectors2(eigen(cov(X3)),FALSE,4))


library(EBImage)

j = matrix( nrow=0,ncol=1024)

for (i in 1:10){
  img = readImage(paste("E:/MS DS/SMMDS/Assignment 5/images/",i,".jpg",sep=''))
  colorMode(img) = Grayscale
  display(img, all=TRUE)
  
  u <- resize(img, w = 32, h = 32)
  
  u <- matrix(u, nrow = 1, ncol=1024)
  
  j<-rbind(j,u)
  
}


picture(getEigVectors2(eigen(cov(j)),TRUE,1))
picture(getEigVectors2(eigen(cov(j)),FALSE,1))