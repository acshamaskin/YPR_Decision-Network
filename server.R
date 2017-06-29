#server.R
library(shiny)
library(plyr)
#library(sm)
source('global.R',local=TRUE)

shinyServer(function(input, output, session) {
  observeEvent(input$Testattribchoices,{
    attribchoice<-input$Testattribchoices
    attribchoice[attribchoice%in%1]<-"Yield"
    attribchoice[attribchoice%in%2]<-"Avge Wt."
    attribchoice[attribchoice%in%3]<-"Harvest Rate"
    attribchoice[attribchoice%in%4]<-"Big Fish Harvest"
    attribchoice[attribchoice%in%5]<-"Harvest Opp."
    attribchoice[attribchoice%in%6]<-"Quality Harvest Opp."
    
    if(length(attribchoice)==1){updateSliderInput(session, "testattrib1",label = attribchoice[1],min = 0, max=100, value = 100,step=1)}
    if(length(attribchoice)==2){updateSliderInput(session, "testattrib2",label = attribchoice[1],min = 0, max=100, value = 50,step=1)
      updateSliderInput(session, "testattrib3",label = attribchoice[2],min = 0, max=100, value = 50,step=1)}
    if(length(attribchoice)==3){updateSliderInput(session, "testattrib4",label = attribchoice[1],min = 0, max=100, value = 33,step=1)
      updateSliderInput(session, "testattrib5",label = attribchoice[2],min = 0, max=100, value = 33,step=1)
      updateSliderInput(session, "testattrib6",label = attribchoice[3],min = 0, max=100, value = 33,step=1)}
    if(length(attribchoice)==4){updateSliderInput(session, "testattrib7",label = attribchoice[1],min = 0, max=100, value = 25,step=1)
      updateSliderInput(session, "testattrib8",label = attribchoice[2],min = 0, max=100, value = 25,step=1)
      updateSliderInput(session, "testattrib9",label = attribchoice[3],min = 0, max=100, value = 25,step=1)
      updateSliderInput(session, "testattrib10",label = attribchoice[4],min = 0, max=100, value = 25,step=1)}
    if(length(attribchoice)==5){updateSliderInput(session, "testattrib11",label = attribchoice[1],min = 0, max=100, value = 20,step=1)
      updateSliderInput(session, "testattrib12",label = attribchoice[2],min = 0, max=100, value = 20,step=1)
      updateSliderInput(session, "testattrib13",label = attribchoice[3],min = 0, max=100, value = 20,step=1)
      updateSliderInput(session, "testattrib14",label = attribchoice[4],min = 0, max=100, value = 20,step=1)
      updateSliderInput(session, "testattrib15",label = attribchoice[5],min = 0, max=100, value = 20,step=1)}
    if(length(attribchoice)==6){updateSliderInput(session, "testattrib16",label = attribchoice[1],min = 0, max=100, value = 16,step=1)
      updateSliderInput(session, "testattrib17",label = attribchoice[2],min = 0, max=100, value = 16,step=1)
      updateSliderInput(session, "testattrib18",label = attribchoice[3],min = 0, max=100, value = 16,step=1)
      updateSliderInput(session, "testattrib19",label = attribchoice[4],min = 0, max=100, value = 16,step=1)
      updateSliderInput(session, "testattrib20",label = attribchoice[5],min = 0, max=100, value = 16,step=1)
      updateSliderInput(session, "testattrib21",label = attribchoice[6],min = 0, max=100, value = 16,step=1)}
    
    AWtable<-reactive({
      if(length(attribchoice)==1){atscore<-c(input$testattrib1)}
      if(length(attribchoice)==2){atscore<-c(input$testattrib2,
                                             input$testattrib3)}
      if(length(attribchoice)==3){atscore<-c(input$testattrib4,
                                             input$testattrib5,
                                             input$testattrib6)}
      if(length(attribchoice)==4){atscore<-c(input$testattrib7,
                                             input$testattrib8,
                                             input$testattrib9,
                                             input$testattrib10)}
      if(length(attribchoice)==5){atscore<-c(input$testattrib11,
                                             input$testattrib12,
                                             input$testattrib13,
                                             input$testattrib14,
                                             input$testattrib15)}
      if(length(attribchoice)==6){atscore<-c(input$testattrib16,
                                             input$testattrib17,
                                             input$testattrib18,
                                             input$testattrib19,
                                             input$testattrib20,
                                             input$testattrib21)}
      
      tmp<-data.frame("Attribute"=attribchoice,
                      "Weight"=atscore)
      return(tmp)
    })
    
    attrib_norm<-reactive({
      tmp<-AWtable()
      # A_init<-c(input$Yieldweight,input$AvgWtweight,input$Hrateweight,input$QHrateweight,input$Harvestoppweight)
      A_init<-tmp$Weight
      A_tot<-sum(A_init)
      A_norm<-A_init
      for(i in 1:length(A_init)){
        A_norm[i]<-A_init[i]/A_tot  
      }
      tmp$adjweight<-A_norm*100
      tmp<-tmp[,c(1,3)]
      colnames(tmp)<-c("Utility","% of Decision")
      return(tmp)
    })
    
    output$AWtest<-renderTable(expr = {
      attrib_norm()
    },bordered = T,hover = T,align = "c",striped = T)
    
  })
  ##SPPSELECT MENU##
#  observeEvent(input$Sppsel, {
#    
#  })
  ##CONTROLS PARAMETER SLIDER RANGES AND WHICH SPECIES TO USE PARAMETERS FOR
  observeEvent(input$initpops,{
    #if(input$Sppsel=="White Crappie"){} WHEN YOU GET MORE SPECIES DATA RANGES
    nlakes<-input$nlakes
    if(is.null(nlakes)==T) {return(NULL)}else{
      updateSliderInput(session, "bproplow",min=0,max = nlakes,value = 0,step =1)
      updateSliderInput(session, "bpropmid",min=0,max = nlakes,value = 0,step =1)
      updateSliderInput(session, "bprophigh",min=0,max = nlakes,value = 0,step =1)
      updateSliderInput(session, "Linfproplow",min=0,max = nlakes,value = 0,step =1)
      updateSliderInput(session, "Linfpropmid",min=0,max = nlakes,value = 0,step =1)
      updateSliderInput(session, "Linfprophigh",min=0,max = nlakes,value = 0,step =1)
      updateSliderInput(session, "Aproplow",min=0,max = nlakes,value = 0,step =1)
      updateSliderInput(session, "Apropmid",min=0,max = nlakes,value = 0,step =1)
      updateSliderInput(session, "Aprophigh",min=0,max = nlakes,value = 0,step =1)
      updateSliderInput(session, "uproplow",min=0,max = nlakes,value = 0,step =1)
      updateSliderInput(session, "upropmid",min=0,max = nlakes,value = 0,step =1)
      updateSliderInput(session, "uprophigh",min=0,max = nlakes,value = 0,step =1)}
  })
  ##CONTROLS USE OF POPULATION PARAMETERS
  observeEvent(input$b,{
    bchoice<-input$b
    if(ifvector(bchoice,1)==1){updateCheckboxGroupInput(session,"b",selected = "1")
                   updateSliderInput(session, "bproplow",value = 0)
                   updateSliderInput(session, "bpropmid",value = 0)
                   updateSliderInput(session, "bprophigh",value = 0)}
    if(ifvector(bchoice,2)==0){updateSliderInput(session, "bproplow",value = 0)}
    if(ifvector(bchoice,3)==0){updateSliderInput(session, "bpropmid",value = 0)}
    if(ifvector(bchoice,4)==0){updateSliderInput(session, "bprophigh",value = 0)}
    
  })
  observeEvent(input$Linf,{
    bchoice<-input$Linf
    if(ifvector(bchoice,1)==1){updateCheckboxGroupInput(session,"Linf",selected = "1")
      updateSliderInput(session, "Linfproplow",value = 0)
      updateSliderInput(session, "Linfpropmid",value = 0)
      updateSliderInput(session, "Linfprophigh",value = 0)}
    if(ifvector(bchoice,2)==0){updateSliderInput(session, "Linfproplow",value = 0)}
    if(ifvector(bchoice,3)==0){updateSliderInput(session, "Linfpropmid",value = 0)}
    if(ifvector(bchoice,4)==0){updateSliderInput(session, "Linfprophigh",value = 0)}
    
  })
  
  observeEvent(input$A,{
    bchoice<-input$A
    if(ifvector(bchoice,1)==1){updateCheckboxGroupInput(session,"A",selected = "1")
      updateSliderInput(session, "Aproplow",value = 0)
      updateSliderInput(session, "Apropmid",value = 0)
      updateSliderInput(session, "Aprophigh",value = 0)}
    if(ifvector(bchoice,2)==0){updateSliderInput(session, "Aproplow",value = 0)}
    if(ifvector(bchoice,3)==0){updateSliderInput(session, "Apropmid",value = 0)}
    if(ifvector(bchoice,4)==0){updateSliderInput(session, "Aprophigh",value = 0)}
    
  })
  
  observeEvent(input$uprop,{
    bchoice<-input$uprop
    if(ifvector(bchoice,1)==1){updateCheckboxGroupInput(session,"uprop",selected = "1")
      updateSliderInput(session, "uproplow",value = 0)
      updateSliderInput(session, "upropmid",value = 0)
      updateSliderInput(session, "uprophigh",value = 0)}
    if(ifvector(bchoice,2)==0){updateSliderInput(session, "uproplow",value = 0)}
    if(ifvector(bchoice,3)==0){updateSliderInput(session, "upropmid",value = 0)}
    if(ifvector(bchoice,4)==0){updateSliderInput(session, "uprophigh",value = 0)}
    
  })
  
  #llinput<-reactive({
  #  
  #  llinput<-as.numeric(unlist(strsplit(input$mll,",")))
    #llinput<-llinput/25.4
    # llinput<-list(llinput)
    
  #  return(llinput)
  #})
  observe({
    
    x<-input$mll
    llinput<-as.numeric(unlist(strsplit(input$mll,",")))
    
    if(length(llinput)==0){updateRadioButtons(session, "llselect", choices = c("No Length Limits Entered"))}
    if(length(llinput)==1){updateRadioButtons(session, "llselect", choices = c(llinput[[1]]))}
    if(length(llinput)==2){updateRadioButtons(session, "llselect", choices = c(llinput[[1]],llinput[[2]],"Show All"))}
    if(length(llinput)==3){updateRadioButtons(session, "llselect", choices = c(llinput[[1]],llinput[[2]],llinput[[3]],"Show All"))}
    if(length(llinput)==4){updateRadioButtons(session, "llselect", choices = c(llinput[[1]],llinput[[2]],llinput[[3]],llinput[[4]],"Show All"))}

    #updateRadioButtons(session, "llselect", choices = c(x[1],x[2],x[3],"Show All"))
  })
  
  output$text1<-renderText({
    paste("You have selected", input$llselect)
  })
######OLD CODE IS IN global.R#######
 #observeEvent(input$llsimulate, {
   #if(is.null(input$b)==T){return()}else{
   #buttonstop<-1
  # input$llsimulate<-NULL
   ##Builds Set of populations
   #if(is.na(nreps)==TRUE){return() }

  Build_Sim<-eventReactive(input$llsimulate,{ 
   nreps<-as.numeric(unlist(strsplit(input$nlakes,",")))  
   Linf<-as.numeric(unlist(strsplit(input$Linf,",")))                        ##1. Unlists values
   if(ifvector(Linf,1)==1){Linf<-as.numeric(unlist(strsplit(input$Linffixed,",")))##2. Checks if fixed
   Linf<-rep(Linf,nreps)}else{Linf2<-sample(Linflow,input$Linfproplow,replace = T) ##3. If not fixed, samples from variable
                              Linf3<-sample(Linfmid,input$Linfpropmid,replace = T)
                              Linf4<-sample(Linfhigh,input$Linfprophigh,replace = T)
                              Linf<-c(Linf2,Linf3,Linf4)
                              Linf<-sample(Linf)}                                                  
                          
   b<-as.numeric(unlist(strsplit(input$b,",")))
   if(ifvector(b,1)==1){b<-as.numeric(unlist(strsplit(input$bfixed,",")))
   b<-rep(b,nreps)}else{b2<-sample(blow,input$bproplow,replace = T)
                          b3<-sample(bmid,input$bpropmid,replace = T)
                          b4<-sample(bhigh,input$bprophigh,replace = T)
                          b<-c(b2,b3,b4)
                          b<-sample(b)}
   
   A<-as.numeric(unlist(strsplit(input$A,",")))
   if(ifvector(A,1)==1){A<-as.numeric(unlist(strsplit(input$Afixed,",")))
   A<-rep(A,nreps)}else{A2<-sample(Alow,input$Aproplow,replace = T)
                        A3<-sample(Amid,input$Apropmid,replace = T)
                        A4<-sample(Ahigh,input$Aprophigh,replace = T)
                        A<-c(A2,A3,A4)
                        A<-sample(A)}
   
   uprop<-as.numeric(unlist(strsplit(input$uprop,",")))
   if(ifvector(uprop,1)==1){uprop<-as.numeric(unlist(strsplit(input$upropfixed,",")))
   uprop<-rep(uprop,nreps)}else{uprop2<-sample(uproplow,input$uproplow,replace = T)
                                uprop3<-sample(upropmid,input$upropmid,replace = T)
                                uprop4<-sample(uprophigh,input$uprophigh,replace = T)
                                uprop<-c(uprop2,uprop3,uprop4)
                                uprop<-sample(uprop)#RANDOMIZES VECTOR ORDER#
                                }
   
   Sim_B<-data.frame("Linf"=Linf,
                     "t0"=runif(nreps,0.15,0.22),
                     "a"=rep(-5.6,nreps),
                     "b"=b,
                     "A"=A,
                     "uprop"=uprop)
   Sim_B$a<-10^Sim_B$a
   Sim_B$Z<--log(1-Sim_B$A)
   Sim_B$u<-Sim_B$uprop*Sim_B$A
   Sim_B$u2<-Sim_B$u-0.02
   Sim_B$Fmort<-(Sim_B$u*Sim_B$Z)/Sim_B$A
   Sim_B$Fmort2<-(Sim_B$u2*Sim_B$Z)/Sim_B$A
  
   #lnk<- 6.62-1.32*(log(Sim_B$Linf))
   Sim_B$k<-exp(6.62-1.32*(log(Sim_B$Linf)))
   mm<-1-exp(Sim_B$u*(log(1-Sim_B$A))/Sim_B$A)
   Sim_B$n<--((Sim_B$A-mm)/(mm-1))
   #Sim_B$k<-0.4
   #Sim_B$t0<- -0.3922-0.2752*log10(Sim_B$Linf) - 1.038*log10(Sim_B$k) #(Pauly 1979)
   #Sim_B$t0<-10^Sim_B$t0
   return(Sim_B)
   })##
   
# buttonpress<-NULL #to keep program from crashing
  sim_dat<- function()
  { 
 
    #sim<- Raw_Spectrum_Setup()
    sim<-Build_Sim()
    #sim<-COMBOS
    #nreps<-as.numeric(unlist(strsplit(input$nlakes,",")))
    #sim<-Sim_B
    #sim<-A_and_M_95
    #sim<-SimulationA 1 Lake vs 3 vs 10 Lakes (Identical cohorts)
    #sim<-SimulationB 30 Lakes with varied VBGF (uniform, normal distributions (low, med, high))
    #sim<-SimulationC 30 Lakes with varied WL (Same 4 distributions as above)
    #sim<-SimulationD 30 Lakes with varied Natural and Fishing Mortalities (Same 4 distributions as above)
    
    #bring in mll
    sim$quality<-as.numeric(unlist(strsplit(input$quality,",")))
    sim$quality<-sim$quality*25.4
    mll<-as.numeric(unlist(strsplit(input$mll,",")))
    mll<-mll*25.4
    #merge mll with big dataframe.  creates a list of dataframes with a ll as a column, then combines all dataframes
    func<-function(x,y){y=data.frame(x,y)}
    mf<-lapply(mll,func,sim)
    mf<-do.call("rbind",mf)
    colnames(mf)[1]<-"mll"
    ###Moved these calculations in the original raw_spectrum build
    #mf$u<-mf$uprop*mf$A ###Calculate Fmort
    #mf$Z<--log(1-mf$A)
    #mf$Fmort<-(mf$u*mf$Z)/mf$A
    ##set time of fishery recruitment from mll
   #mf$tr=((log(1-(mf$mll/mf$Linf))/(-(mf$k)))+mf$t0)
   #mf$tquality<-ifelse(mf$quality<=mf$Linf,((log(1-(mf$quality/mf$Linf))/(-(mf$k)))+mf$t0),999)
    mf$tr<-ifelse(mf$mll<mf$Linf,((log(1-(mf$mll/mf$Linf))/(-(mf$k)))+mf$t0),999) #4/3/17
    mf$tquality<-ifelse(mf$quality<mf$Linf,((log(1-(mf$quality/mf$Linf))/(-(mf$k)))+mf$t0),999) #4/3/17 constrains so NA's dont pop up
    out<-mf
    out<-out[,c(1,2,3,13,4,5,6,9,8,11,14,7,10,12,15,16,17)]
    return(out)
  }
  
  #  Adams-Bashforth
  preppop<-function(){
   
    sim<-sim_dat()
    maxage<-10
    #sim1<-subset.data.frame(sim, lake==1)
    #sim2<-subset.data.frame(sim, lake==2)
    #sim3<-subset.data.frame(sim, lake==3)
    step<- 0.1
    ages<-seq(0,maxage+1,step)
    L1<-L2<-Y1<-Y2<-N1<-N2<-NH1<-QH1<-matrix(0,nrow(sim),length(ages))
    N1[,1]<- 1000 #Initial Recruitment N0
    N2[,1]<- 1000 #Test for Growth Overfishing
    
     
    
    for(i in 2:ncol(N1))
    {
      #    indx1<-ifelse(ages[i]>=sim$min_age_harvested & ages[i]<sim$tr,1,0)
   
      indx2<-ifelse(ages[i]>=sim$tr,1,0)
      indx3<-ifelse(ages[i]>=sim$tquality,1,0)
      harvested<-N1[,i-1]*(sim$Fmort*indx2)
      qharvested<-N1[,i-1]*(sim$Fmort*indx3)
      mortality<- N1[,i-1]*(sim$Z-sim$Fmort)
      harvested2<-N2[,i-1]*(sim$Fmort2*indx2)  #Used to test for growth overfishing
      #mortality2<-N2[,i-1]*(sim$Z-sim$Fmort2)  #Used to test for growth overfishing
      #mortality<-N1[,i-1]*(0.2+sim$Fmort)
      dN<- (mortality+harvested)*step
      dN2<-(mortality+harvested2)*step  #Used to test for growth overfishing
      dNH<-harvested*step
      dQH<-qharvested*step
      Lt<-sim$Linf * (1 - exp(-sim$k* (ages[i-1]-sim$t0)))
      ###WEIGHT AT AGE
      Wt = (sim$a*Lt^sim$b)/1000
      dY<-ifelse(harvested==0,0,(Wt*harvested)*step)
      dY2<-ifelse(harvested2==0,0,(Wt*harvested2)*step) #Used to test for growth overfishing
      # AvgWeightharvested<-ifelse(harvested==0,0,())
      N1[,i]<-N1[,i-1]-dN
      #N1[,2]<-N1[,1]-dN
      #N1[,i+1]<-N1[,i]+(1.5*dN[i])-0.5(dN[i-1])
      Y1[,i]<- Y1[,i-1]+dY
      #Y[,2]<- Y[,1]+dY
      #Y[,i+1]<- Y[,i]+(1.5*dY[i])-0.5(dY[i-1])
      L1[,i]<-  Lt*(1/N1[,i-1])
      NH1[,i]<- NH1[,i-1]+dNH
      QH1[,i]<- QH1[,i-1]+dQH
      
      N2[,i]<-N2[,i-1]-dN2 #Used to test for growth overfishing
      Y2[,i]<- Y2[,i-1]+dY2 #Used to test for growth overfishing
      L2[,i]<-  Lt*(1/N2[,i-1])  #Used to test for growth overfishing
    }
    sim$Yab<- Y1[,ncol(Y1)]  #Total Yield
    sim$Yab2<- Y2[,ncol(Y2)]  #Used to test for growth overfishing 
    sim$AvgWt<-ifelse(sim$Yab>0,sim$Yab/(NH1[,ncol(NH1)]),0) #Average Weight
   # sim$Harvestrate<-ifelse(sim$Yab>0,NH1[,ncol(NH1)]/abs(maxage-sim$tr),0)  #Harvest Rate 
    sim$Harvestrate<-ifelse(sim$Yab>0,NH1[,ncol(NH1)],0)  #TEST Harvest Rate 
    sim$QualityHarvest<-ifelse(QH1[,ncol(QH1)]>0,QH1[,ncol(QH1)],0)  #Quality Harvest Rate
    sim$Harvestopp<-ifelse(sim$Yab>0,1,0) #did harvest occur? 1=Y, 0=N
    sim$QHarvestopp<-ifelse(sim$QualityHarvest>0.01,1,0) #did harvest of specified size class occur? 1=Y, 0=N
    sim$GrowthOverfishing<-ifelse(sim$Yab-sim$Yab2>=0,0,1) #If negative, then Growth Overfishing Occured (given a 1)
    
    #change column names
    colnames(sim)[10]<-"F"
    colnames(sim)[14]<-"F2"
    return(sim)
  }

######PUT SUBSET HERE TO MAKE IT GO FASTER####### #6/15/17 Not USING DURING THESIS SIMULATIONS
vals<-function()
{
 
  tmp<-preppop()

#  tmp<-subset(tmp,
#              (rLinf %in% as.numeric(input$Linf)) &
#              (rA %in% as.numeric(input$A)) &
#              (ruprop %in% as.numeric(input$up)) &
#              (rb %in% as.numeric(input$b)))
               #(Linf>=min(indx1)) & (Linf<=max(indx1)) &
               #(A>=min(indx2)) & (A<=max(indx2)) &
               #(uprop>=min(indx3)) & (uprop<=max(indx3)) &
               #(b>=min(indx4)) & (b<=max(indx4)))
  #tmp$quality<-as.numeric(unlist(strsplit(input$quality,",")))
  #tmp$quality<-tmp$quality*25.4
  #add all your inputs 
  #out$t0<-runif(input$nlakes,min(input$t0),max(input$t0)) example of previous method
  # out$nLakes<-input$nLakes
  sim<-tmp
  return(sim)
}

  ##New Attribute Table##
  AWtables<-function(){
    attribchoice<-input$Testattribchoices
    attribchoice[attribchoice%in%1]<-"Yield"
    attribchoice[attribchoice%in%2]<-"Avge Wt."
    attribchoice[attribchoice%in%3]<-"Harvest Rate"
    attribchoice[attribchoice%in%4]<-"Big Fish Harvest"
    attribchoice[attribchoice%in%5]<-"Harvest Opp."
    attribchoice[attribchoice%in%6]<-"Quality Harvest Opp."
    if(length(attribchoice)==1){atscore<-c(input$testattrib1)}
    if(length(attribchoice)==2){atscore<-c(input$testattrib2,
                                           input$testattrib3)}
    if(length(attribchoice)==3){atscore<-c(input$testattrib4,
                                           input$testattrib5,
                                           input$testattrib6)}
    if(length(attribchoice)==4){atscore<-c(input$testattrib7,
                                           input$testattrib8,
                                           input$testattrib9,
                                           input$testattrib10)}
    if(length(attribchoice)==5){atscore<-c(input$testattrib11,
                                           input$testattrib12,
                                           input$testattrib13,
                                           input$testattrib14,
                                           input$testattrib15)}
    if(length(attribchoice)==6){atscore<-c(input$testattrib16,
                                           input$testattrib17,
                                           input$testattrib18,
                                           input$testattrib19,
                                           input$testattrib20,
                                           input$testattrib21)}
    
    tmp<-data.frame("Attribute"=attribchoice,
                    "Weight"=atscore)
    return(tmp)
  }
    
#####utility scores 11/30/2016 Method to Normalize utility scores

#function to normalize inputs
attrib_norm<-function(){
  tmp<-AWtables()
 # A_init<-c(input$Yieldweight,input$AvgWtweight,input$Hrateweight,input$QHrateweight,input$Harvestoppweight)
  A_init<-tmp$Weight
  A_tot<-sum(A_init)
  A_norm<-A_init
  for(i in 1:length(A_init)){
    A_norm[i]<-A_init[i]/A_tot  
  }
  tmp$adjweight<-A_norm
  return(tmp)
}
#utilities<-function()
#{
#  weights<-list()
#  weights$Yieldweight<-input$Yieldweight
#  weights$AvgWtweight<-input$AvgWtweight
#  weights$Hrateweight<-input$Hrateweight
#  weights$QHrateweight<-input$QHrateweight
#  return(weights)
#}

LLscore<-function(){
  output<-vals()
  A_norm<-attrib_norm()
  ##RANK LENGTH LIMITS BASED ON NormalYield+NormalAvgWt
  scoringYield<-aggregate(Yab~mll,output,mean)
  scoringAvgWt<-aggregate(AvgWt~mll,output,mean)
  scoringHrate<-aggregate(Harvestrate~mll,output,mean)
  scoringQHrate<-aggregate(QualityHarvest~mll,output,mean)
  scoringHopp<-aggregate(Harvestopp~mll,output,mean)#4/3/2017
  scoringQHopp<-aggregate(QHarvestopp~mll,output,mean)#6/12/2017
  scoringGoverfishing<-aggregate(GrowthOverfishing~mll,output,mean) #6/25/2017 NOT YET INCLUDED IN TOTAL SCORE
  Scores<-join_all(list(scoringYield,scoringAvgWt,scoringHrate,scoringQHrate,scoringHopp,scoringQHopp,scoringGoverfishing),by="mll") #4/3/17
  #Scores<-merge(scoringYield,scoringAvgWt,scoringHrate,scoringQHrate,by="mll")
  Scores$Yieldscore<-((Scores$Yab-min(Scores$Yab))/(max(Scores$Yab)-min(Scores$Yab)))*100*A_norm[which(A_norm$Attribute=="Yield"),3]
  Scores$AvgWtscore<-((Scores$AvgWt-min(Scores$AvgWt))/(max(Scores$AvgWt)-min(Scores$AvgWt)))*100*A_norm[which(A_norm$Attribute=="Avge Wt."),3]
  Scores$Hratescore<-((Scores$Harvestrate-min(Scores$Harvestrate))/(max(Scores$Harvestrate)-min(Scores$Harvestrate)))*100*A_norm[which(A_norm$Attribute=="Harvest Rate"),3]
  Scores$QHratescore<-((Scores$QualityHarvest-min(Scores$QualityHarvest))/(max(Scores$QualityHarvest)-min(Scores$QualityHarvest)))*100*A_norm[which(A_norm$Attribute=="Big Fish Harvest"),3]
  Scores$Harvestoppscore<-ifelse((max(Scores$Harvestopp)-min(Scores$Harvestopp))!=0,((Scores$Harvestopp-min(Scores$Harvestopp))/(max(Scores$Harvestopp)-min(Scores$Harvestopp)))*100*A_norm[which(A_norm$Attribute=="Harvest Opp."),3],100*A_norm[which(A_norm$Attribute=="Harvest Opp."),3])
  Scores$QHarvestoppscore<-ifelse((max(Scores$QHarvestopp)-min(Scores$QHarvestopp))!=0,((Scores$QHarvestopp-min(Scores$QHarvestopp))/(max(Scores$QHarvestopp)-min(Scores$QHarvestopp)))*100*A_norm[which(A_norm$Attribute=="Quality Harvest Opp."),3],100*A_norm[which(A_norm$Attribute=="Quality Harvest Opp."),3])
  Scores$Total<-Scores$Yieldscore+Scores$AvgWtscore+Scores$Hratescore+Scores$QHratescore+Scores$Harvestoppscore+Scores$QHarvestoppscore
  Scores$mll<-round((Scores$mll/25.4),0)
  
  return(Scores)
}



##Dynamically gives Attribute selection an updated list of choices
#attribchoice<-reactive({
#  attribchoice<-input$Testattribchoices
#  attribchoice[attribchoice%in%1]<-"Yield"
#  attribchoice[attribchoice%in%2]<-"Avge Wt."
#  attribchoice[attribchoice%in%3]<-"Harvest Rate"
#  attribchoice[attribchoice%in%4]<-"Big Fish Harvest"
#  attribchoice[attribchoice%in%5]<-"Harvest Opp."
#  return(attribchoice)
#})


 

##Dynamically gives LL radio buttons an updated list of choices



data <-function(){
  dat<-vals()
  #if(input$llselect=="Show All"){ ####11/14/16 took out if statement so the plots can have the same axes
    datt <-switch(input$datt,
                  Yield = data.frame(dat$Yab,dat$mll),
                  AverageWt = data.frame(dat$AvgWt,dat$mll),
                  HarvestRate = data.frame(dat$Harvestrate,dat$mll),
                  QualityHarvest = data.frame(dat$QualityHarvest,dat$mll)
    )
    return(datt) 
  #else{llselect<-as.numeric(input$llselect)*25.4 
  #dat<-subset(dat,mll==llselect)
  #datt <-switch(input$datt,
  #              Yield = dat$Yab,
  #              AverageWt = dat$AvgWt,
  #              HarvestRate = dat$Harvestrate,
  #              QualityHarvest = dat$QualityHarvest
  #              )
  #return(datt)}
}

  
  

output$ScoreLL<-renderTable({
  input$gollsimulate
  LLscore()
})

#DENSITY PLOT
output$plot<-renderPlot({
  datt<-input$datt
  llselect<-input$llselect
 # if(input$llselect=="Show All"){
###SHOW ALL DENSITY PLOT###
  den<-data()
  xlim<-range(den[,1])
  mll<-unique(den[,2])
  dena<-subset(den,den[,2]==mll[1])
  denb<-subset(den,den[,2]==mll[2])
  denc<-subset(den,den[,2]==mll[3])
 # dena<-dena[,1]
#  denb<-denb[,1]
 # denc<-denc[,1]
  dena<-density(dena[,1])
  denb<-density(denb[,1])
  denc<-density(denc[,1])
  ylim<-range(dena$y,denb$y,denc$y)
  if(input$llselect=="Show All"){
  plot(dena,col="black", type="l",xlim=xlim,ylim=ylim,xlab=paste(datt),main=paste(datt, 'for all length limits',sep=' '))
  polygon(dena,col=trans_black)
  lines(denb)
  polygon(denb,col=trans_red)
  lines(denc)
  polygon(denc,col=trans_green)} 
  ######
  #if(input$llselect=="Show All") {d<-data()
  #sm.density.compare(d[,1],d[,2], xlab=paste(datt, 'for all length limits',sep=' '))} 
 ####INDIVIDUAL LL DENSITY####
  else{d<-subset(den,den[,2]==(as.numeric(llselect)*25.4))
  d<-density(d[,1])
  plot(d,xlim=xlim,ylim=ylim,xlab=paste(datt),main=paste(datt, 'for',llselect,'inch length limit',sep=' ' ))
  polygon(d, col=trans_black)}
  #hist(data(),main=paste(datt, 'for',llselect,'inch length limit',sep=' ' ))
})

##TABLE OF INPUTS
output$Sim_2<-renderTable(expr={
  preppop()
},bordered = T,hover = T,align = "c",striped = T)
##DOWNLOAD TABLE
output$download_table2<- downloadHandler(
  filename = function(){'YPR_Output.csv'},
  content = function(file) {
    write.csv(preppop(), file)
  })  

#}
   
})
