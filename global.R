##Needs to generate raw spectrum first and then subset from that given the parameter bounds##



#Raw_Spectrum_Setup<-reactive({
  
#  nreps=100000
#  COMBOS<- data.frame(t0=runif(nreps,0.210,0.210),
#                      Linf=round(runif(nreps,300,400),0),
#                      A=round(runif(nreps,.250,.750),3),
#                      uprop=round(runif(nreps,0.010,0.900),3),
#                      b=round(runif(nreps,3.100,3.500),3),
#                      aui=runif(nreps,-5.6,-5.6))
#  COMBOS$u=COMBOS$uprop*COMBOS$A
#  COMBOS$Z<--log(1-COMBOS$A)
#  COMBOS$Fmort<-(COMBOS$u*COMBOS$Z)/COMBOS$A
#  #lnk<- 6.62-1.32*(log(COMBOS$Linf))
#  COMBOS$k<-exp(6.62-1.32*(log(COMBOS$Linf)))
#  COMBOS$a<-10^COMBOS$aui
#  COMBOS$rLinf<-1
#  COMBOS$rA<-1
#  COMBOS$ruprop<-1
#  COMBOS$rb<-1
#  
#  ###Subsets### To assign bin scores (1,2 or 3) 
#  #rLinf
#  tmp<-subset(COMBOS, Linf>=334 & Linf <=366)
#  tmp$rLinf<-2
#  tmpp<-subset(COMBOS,Linf>=367 & Linf <=400)
#  tmpp$rLinf<-3
#  tmppp<-subset(COMBOS, Linf<=333)
#  COMBOS<-rbind(tmp,tmpp,tmppp)
#  #rA                        
#  tmp<-subset(COMBOS, A>=0.417 & A <=0.583)
#  tmp$rA<-2
#  tmpp<-subset(COMBOS,A>=0.584 & A <=0.750)
#  tmpp$rA<-3
#  tmppp<-subset(COMBOS, A<=0.416)
#  COMBOS<-rbind(tmp,tmpp,tmppp)
#  #ruprop                        
#  tmp<-subset(COMBOS, uprop>=0.310 & uprop <=0.600)
#  tmp$ruprop<-2
#  tmpp<-subset(COMBOS,uprop>=0.601 & uprop <=0.900)
#  tmpp$ruprop<-3
#  tmppp<-subset(COMBOS, uprop<=0.309)
#  COMBOS<-rbind(tmp,tmpp,tmppp)
#  #rb                       
#  tmp<-subset(COMBOS, b>=3.231 & b<=3.360)
#  tmp$rb<-2
#  tmpp<-subset(COMBOS, b>=3.361 & b<=3.500)
#  tmpp$rb<-3
#  tmppp<-subset(COMBOS, rb<=3.230)
#  COMBOS<-rbind(tmp,tmpp,tmppp)
#  return(COMBOS)
#}) 
#
#
#saveRDS(COMBOS,"C:/Users/Andrew Shamaskin/Google Drive/YPR/YPR_NEW/YPR_DN/COMBOS.rds")
#Based on values from Allen & Miranda, 1995
A_and_M_95<-expand.grid("Linf"=c(393,373,353,333),"b"=c(3.6,3.4,3.2),
                        "A"=c(0.7,0.5,0.3), "uprop"=c(0.7,0.5,0.3))

A_and_M_95$t0<-ifelse(A_and_M_95$Linf==393,0.230,ifelse(A_and_M_95$Linf==373,0.215,ifelse(A_and_M_95$Linf==353,.197,.174)))
A_and_M_95$k<-ifelse(A_and_M_95$Linf==393,0.446,ifelse(A_and_M_95$Linf==373,0.416,ifelse(A_and_M_95$Linf==353,.374,.325)))
A_and_M_95$a<-ifelse(A_and_M_95$b==3.6,-5.8,ifelse(A_and_M_95$b==3.4,-5.6,-5.4))
A_and_M_95$a<-10^A_and_M_95$a
A_and_M_95$u=A_and_M_95$uprop*A_and_M_95$A
A_and_M_95$Z<--log(1-A_and_M_95$A)
A_and_M_95$Fmort<-(A_and_M_95$u*A_and_M_95$Z)/A_and_M_95$A
A_and_M_95$rLinf<-ifelse(A_and_M_95$Linf>=373,3,ifelse(A_and_M_95$Linf==353,2,1))
A_and_M_95$rA<-ifelse(A_and_M_95$A==0.7,3,ifelse(A_and_M_95$A==0.5,2,1))
A_and_M_95$ruprop<-ifelse(A_and_M_95$uprop==0.7,3,ifelse(A_and_M_95$uprop==0.5,2,1))
A_and_M_95$rb<-ifelse(A_and_M_95$b==3.6,3,ifelse(A_and_M_95$b==3.4,2,1))



##Crappie Parameters
bhigh<-seq(3.361,3.500,by = .001) #c(low,high)
bmid<-seq(3.231,3.360,by = .001)
blow<-seq(3.000,3.230,by=.001)
Linflow<-seq(300,333,by=.001)
Linfmid<-seq(334,366,by=.001)
Linfhigh<-seq(367,400,by=.001)
Alow<-seq(.250,.416,by=.001)
Amid<-seq(.417,.583,by=.001)
Ahigh<-seq(.584,.750,by=.001)
uproplow<-seq(.010,.309,by=.001)
upropmid<-seq(.310,.600,by=.001)
uprophigh<-seq(.601,.900,by=.001)

#Function to say x is not an element of y
'%!in%' <- function(x,y)!('%in%'(x,y))

#function that checks if a number 'checkfor' is an element of a vector 'tmpvector
ifvector<-function(tmpvector,checkfor){
  tmp<-NULL
  for(i in 1:length(tmpvector)){
    tmp1<-0
    if(tmpvector[i]==checkfor){tmp1<-1}
    tmp[i]<-tmp1
  }
  return(sum(tmp))
}


####REWRITE FUNCTION THAT SUBSETS COMBOS BY PARAMETER BINS, AND THEN APPLIES SCORES TO EACH AND THEN REBINDS IT
COMBOS<-readRDS(file = "C:/Users/Andrew Shamaskin/Google Drive/YPR/YPR_NEW/YPR_DN/COMBOS.rds")
###Make databases for other species
#COMBOS_LMB
#COMBOS_Walleye?
#
#  
##Transparent colors for plotting
trans_black<- rgb(0,0,0,alpha=40,maxColorValue=255)
trans_red<- rgb(228,16,16,alpha=40,maxColorValue=255)
trans_green<- rgb(16,228,16,alpha=40,maxColorValue=255)

