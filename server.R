
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
rm(list=ls())
library(XLConnect)
library(ggplot2)
library(grid)
library(plyr)
library(shiny)



shinyServer(function(input, output, session) {
  ##### Data directory summary files ###
  output$dataSum<-renderTable({
    #      motor<-list.files(path="/Users/adam/Dropbox/Research/Matthew Pitt/enorms",full.names = F)[grep("CMAP",list.files("/Users/adam/Dropbox/Research/Matthew Pitt/enorms"))]
    #      sensory<-list.files(path="/Users/adam/Dropbox/Research/Matthew Pitt/enorms",full.names = F)[grep("SNAP",list.files("/Users/adam/Dropbox/Research/Matthew Pitt/enorms"))]
    #      EMG<-list.files(path="/Users/adam/Dropbox/Research/Matthew Pitt/enorms",full.names = F)[grep("EMG.xls",list.files("/Users/adam/Dropbox/Research/Matthew Pitt/enorms"))]
    #      
    motor<-list.files(path=input$path,full.names = F)[grep(input$cmapstr,list.files(input$path))]
    sensory<-list.files(path=input$path,full.names = F)[grep(input$sensstr,list.files(input$path))]
    EMG<-list.files(path=input$path,full.names = F)[grep(input$emgstr,list.files(input$path))]
    ma<-max(c(length(motor),length(sensory),length(EMG)))
    
    return(data.frame(
      EMG=c(EMG,rep(" ",ma-length(EMG))),
      Sensory=c(sensory,rep(" ",ma-length(sensory))),
      Motor=c(motor,rep(" ",ma-length(motor)))
    )
    )
  })
  
  ##### CMAP code follows ###
  dataInputCMAP<-reactive({
    #Set path for files and a term specific for SNAPs in the filename
    path<-input$path
    extM<-input$cmapstr
    #path<-"/Users/adam/Dropbox/Research/Matthew Pitt/enorms";extM<-"CMAP"
    
    #Get a list of the files in the above directory which include the term in extM
    flsCMAP<-list.files(path=path,full.names = T)[grep(extM,list.files(path))]
    
    #Create and empty data frame ready to put the file data in, then load in the xls data
    cmaps<-head(readWorksheet(loadWorkbook(flsCMAP[1],create=T),sheet=1),1)[-1,]
    for (i in 1:length(flsCMAP)){
      cmaps<-rbind(cmaps,readWorksheet(loadWorkbook(flsCMAP[i],create=T),sheet = 1))
    } ; cmaps<-cmaps[!duplicated(cmaps),]; cmaps<-cmaps[,-7]
    rm(list=c("i","flsCMAP","path","extM"))
    
    #Clean up the data frame
    names(cmaps)<-c("HospID","TestDate","Gender","Age","AgeYears","Nerve","Side","Onset","Amp","CV","NegDur","Area","Latency")
    cmaps[,2]<-as.Date(cmaps[,2]) 
    cmaps[,3]<-as.factor(cmaps[,3]); cmaps[,6]<-as.factor(cmaps[,6])
    cmaps[,7]<-as.factor(cmaps[,7])
    cmaps[which(nchar(cmaps[,8])>4),8]<-substr( cmaps[which(nchar(cmaps[,8])>4),8],1,4)
    cmaps[,8]<-as.numeric(cmaps[,8])
    cmaps[which(nchar(cmaps[,9])>4),9]<-substr( cmaps[which(nchar(cmaps[,9])>4),9],1,4)
    cmaps[,9]<-as.numeric(cmaps[,9])
    cmaps[which(nchar(cmaps[,10])>4),10]<-substr( cmaps[which(nchar(cmaps[,10])>4),10],1,4)
    cmaps[,10]<-as.numeric(cmaps[,10])
    cmaps[which(nchar(cmaps[,11])>4),11]<-substr( cmaps[which(nchar(cmaps[,11])>4),11],1,4)
    cmaps[,11]<-as.numeric(cmaps[,11])
    cmaps[which(nchar(cmaps[,12])>4),12]<-substr( cmaps[which(nchar(cmaps[,12])>4),12],1,4)
    cmaps[,12]<-as.numeric(cmaps[,12])
    cmaps[which(nchar(cmaps[,13])>4),13]<-substr( cmaps[which(nchar(cmaps[,13])>4),13],1,4)
    cmaps[,13]<-as.numeric(cmaps[,13])
    return(cmaps)
  })
  
  dataInputCMAPlog<-reactive({
    cmaps<-dataInputCMAP()
    cmaps[,8:13]<-log(cmaps[,8:13])
    for(i in 8:13){
      cmaps[which(cmaps[,i]==-Inf),i]<-0
    }
    return(cmaps)
  })
  
  dataInputCMAP1<-reactive({
    if (input$logM) {cmaps<-dataInputCMAPlog()} else {cmaps<-dataInputCMAP()}
    cm<-cmaps[(cmaps$Age>(input$AgeM[1]*365)&cmaps$Age<(input$AgeM[2]*365)&cmaps$Nerve==input$NerveM),] #select subgroup of age and muscle
    #cm<-cmaps[(cmaps$Age>(2*365)&cmaps$Age<(6*365)&cmaps$Nerve=="Medianus Motor"),] #select subgroup of age and muscle
    
    for (i in 8:13){
      # i<-7
      cm<-cm[order(cm[,i]),]
      cm[,(length(cm[1,])+1)]<-c(0,diff(cm[,i]))
      cm[,(length(cm[1,])+1)]<-1:length(cm[,1])
      cm[cm[,i]==0,(length(cm[1,]))]<-NA
    }
    names(cm)[14:25]<-c("OnsetD","orderOn", "AmpD","orderAmp", "CVD","orderCV",
                        "NegDurD","orderND","AreaD","orderArea","LatencyD","orderLat")
    return(cm)
  })
  
  output$sumTableM <- renderTable({
    cmaps<-dataInputCMAP()
    nam<-names(cmaps)
    return(ddply(cmaps[(cmaps$Age>input$AgeM[1]*365 & cmaps$Age<input$AgeM[2]*365),c(6,8:13)],~Nerve,here(summarise),Total=length(get(nam[6])),
                 Onset=length((which(get(nam[8])>0.001) )),
                 Amp=length(which(get(nam[9])>0)),
                 CV=length(which(get(nam[10])>0)),
                 NegDur=length(which(get(nam[11])>0)),
                 Area=length(which(get(nam[12])>0)),
                 Latency=length(which(get(nam[13])>0))
    ))
  })
  
  output$motorContr <- renderUI({
    cm<-dataInputCMAP1()
    i<-as.numeric(input$selectM)
    lo<-as.integer(sum(is.na(cm[,13+(i-7)*2])))
    up<-as.integer(floor(length(cm[,13+(i-7)*2])*0.99))
    
    helpText("Select upper and lower limits:")
    sliderInput ("LimitM",
                 "Order number:",
                 min = floor(lo/10)*10,
                 max = ceiling(up/10)*10,
                 value = c(lo+(up-lo)*0.25,up-(up-lo)*0.25))
  })
  
  output$cumPlotM<- renderPlot({
    # i<-9; ll<-200; ul<-300
    cm<-dataInputCMAP1()
    i<-as.numeric(input$selectM)
    ul<-input$LimitM[2]
    ll<-input$LimitM[1]
    nn<-(floor(length(cm[,13+(i-7)*2])*0.99)-sum(is.na(cm[,13+(i-7)*2])))
    con<-cm[order(cm[,i]),][ul,i]-((cm[order(cm[,i]),][ul,i]-cm[order(cm[,i]),][ll,i])/(ul-ll))*ul
    grad<-(cm[order(cm[,i]),][ul,i]-cm[order(cm[,i]),][ll,i])/(ul-ll)
    plot<-ggplot(cm)+
      geom_line(aes_string(x=names(cm)[13+(i-7)*2],y=names(cm)[i]))+
      #ggtitle(paste(input$NerveS,"; Age ",input$AgeS[1]," to ",input$AgeS[2],"; n ",nn,"; order ",ll," to ",ul,"; measure ",cm[which(cm[,13+(i-7)*2]==ll),i]," to ",cm[which(cm[,13+(i-7)*2]==ul),i],sep=""))+
      ggtitle(paste(input$NerveM,"; Age ",input$AgeM[1]," to ",input$AgeM[2],"; n ",nn,"; order ",ll," to ",ul,"; measure ",cm[which(cm[,13+(i-7)*2]==ll),i]," to ",cm[which(cm[,13+(i-7)*2]==ul),i],sep=""))+
        xlim(sum(is.na(cm[,13+(i-7)*2])),floor(length(cm[,13+(i-7)*2])*0.99))+
        ylim(cm[order(cm[,i]),][ceiling(sum(is.na(cm[,12+(i-7)*2]))+(length(cm[,13+(i-7)*2])*input$XrangeML))+1,i],cm[order(cm[,i]),][floor(length(cm[,13+(i-7)*2])*input$XrangeM),i])+
        geom_abline(intercept=con, slope=grad,col="red")+
        geom_vline(xintercept = c(ll,ul), colour="blue", linetype = "longdash")+
        geom_hline(yintercept = c(cm[which(cm[,13+(i-7)*2]==ll),i],cm[which(cm[,13+(i-7)*2]==ul),i]),colour="green", linetype = "longdash" )
    return(plot)
  })
  
  output$diffPlotM<- renderPlot({
    cm<-dataInputCMAP1()
    i<-as.numeric(input$selectM)
    ul<-input$LimitM[2]
    ll<-input$LimitM[1]
    nn<-(floor(length(cm[,13+(i-7)*2])*0.99)-sum(is.na(cm[,13+(i-7)*2])))
    plot<-ggplot(cm)+
      geom_point(aes_string(x=names(cm)[13+(i-7)*2],y=names(cm)[12+(i-7)*2]),col=2)+
      xlim(sum(is.na(cm[,13+(i-7)*2])),floor(length(cm[,13+(i-7)*2])*0.99))+
      ylim(min(cm[,12+(i-7)*2],na.rm=T),max(cm[order(cm[,12+(i-7)*2]),][1:floor(length(cm[,12+(i-7)*2])*input$YrangeM)-1,12+(i-7)*2]))+
      geom_vline(xintercept = c(ll,ul), colour="blue", linetype = "longdash")
    return(plot)
  })
  
  output$normPlotM<-renderPlot({
    cm<-dataInputCMAP1()
    i<-as.numeric(input$selectM)
    ul<-input$LimitM[2]
    ll<-input$LimitM[1]
    ggplot(cm)+
      stat_ecdf(aes_string(x=names(cm)[i]),col=2)+
      geom_vline(xintercept = c(cm[which(cm[,13+(i-7)*2]==ll),i],cm[which(cm[,13+(i-7)*2]==ul),i]),colour="green", linetype = "longdash" )+
      xlim(cm[order(cm[,i]),][ceiling(sum(is.na(cm[,12+(i-7)*2]))+(length(cm[,13+(i-7)*2])*input$XrangeML))+1,i],cm[order(cm[,i]),][floor(length(cm[,13+(i-7)*2])*input$XrangeM),i])
    
  })
  
  output$normPlot2M<-renderPlot({
    cm<-dataInputCMAP1()
    i<-as.numeric(input$selectM)
    ul<-input$LimitM[2]
    ll<-input$LimitM[1]
    ggplot(cm)+
      geom_histogram(aes_string(x=names(cm)[i],y = "..density.."),fill="blue",alpha=0.5)+
      stat_function(
        fun = dnorm,
        args=with(cm, c(mean = mean(eval(parse(text=names(cm)[i]))), sd = sd(eval(parse(text=names(cm)[i]))))),
        colour="red")+
      geom_vline(xintercept = c(cm[which(cm[,13+(i-7)*2]==ll),i],cm[which(cm[,13+(i-7)*2]==ul),i]),colour="green", linetype = "longdash" )+
      xlim(cm[order(cm[,i]),][ceiling(sum(is.na(cm[,12+(i-7)*2]))+(length(cm[,13+(i-7)*2])*input$XrangeML))+1,i],cm[order(cm[,i]),][floor(length(cm[,13+(i-7)*2])*input$XrangeM),i])
  })
  
  ###### SNAP code follows ###
  dataInputSNAP<-reactive({
    #Set path for files and a term specific for SNAPs in the filename
    path<-input$path
    extS<-input$sensstr
    #path<-"/Users/adam/Dropbox/Research/Matthew Pitt/enorms";extS<-"SNAP"
    
    
    #Get a list of the files in the above directory which include the term in extS
    flsSNAP<-list.files(path=path,full.names = T)[grep(extS,list.files(path))]
    
    #Create and empty data frame ready to put the file data in, then load in the xls data
    snaps<-head(readWorksheet(loadWorkbook(flsSNAP[1],create=T),sheet=1),1)[-1,]
    for (i in 1:length(flsSNAP)){
      snaps<-rbind(snaps,readWorksheet(loadWorkbook(flsSNAP[i],create=T),sheet = 1))
    } ; snaps<-snaps[!duplicated(snaps),]
    rm(list=c("i","flsSNAP","path","extS"))
    
    #Clean up the data frame
    names(snaps)<-c("HospID","TestDate","Gender","Age","AgeYears","Nerve","Side","Onset","Amp","CV")
    snaps[,2]<-as.Date(snaps[,2]) 
    snaps[,3]<-as.factor(snaps[,3]); snaps[,6]<-as.factor(snaps[,6])
    snaps[,7]<-as.factor(snaps[,7])
    snaps[which(nchar(snaps[,9])>4),9]<-substr( snaps[which(nchar(snaps[,9])>4),9],1,4)
    snaps[,9]<-as.numeric(snaps[,9])
    snaps[which(nchar(snaps[,10])>4),10]<-substr( snaps[which(nchar(snaps[,10])>4),10],1,4)
    snaps[,10]<-as.numeric(snaps[,10])
    return(snaps)
  })
  
  dataInputSNAPlog<-reactive({
    snaps<-dataInputSNAP()
    snaps[,8:10]<-log(snaps[,8:10])
    for(i in 8:10){
      snaps[which(snaps[,i]==-Inf),i]<-0
    }
    return(snaps)
  })
  
  dataInputSNAP1<-reactive({
    if (input$logS) {snaps<-dataInputSNAPlog()} else {snaps<-dataInputSNAP()}
    sn<-snaps[(snaps$Age>(input$AgeS[1]*365)&snaps$Age<(input$AgeS[2]*365)&snaps$Nerve==input$NerveS),] #select subgroup of age and muscle
    for (i in 8:10){
      # i<-7
      sn<-sn[order(sn[,i]),]
      sn[,(length(sn[1,])+1)]<-c(0,diff(sn[,i]))
      sn[,(length(sn[1,])+1)]<-1:length(sn[,1])
      sn[sn[,i]==0,(length(sn[1,]))]<-NA
    }
    names(sn)[11:16]<-c("OnsetD","orderOn", "AmpD","orderAmp", "CVD","orderCV")
    return(sn)
  })
  
  output$sumTableS <- renderTable({
    snaps<-dataInputSNAP()
    nam<-names(snaps)
    return(ddply(snaps[(snaps$Age>input$AgeS[1]*365 & snaps$Age<input$AgeS[2]*365),c(6,8:10)],~Nerve,here(summarise),Total=length(get(nam[6])),Onset=length((which(get(nam[8])>0.001) )),Amp=length(which(get(nam[9])>0)),
                 CV=length(which(get(nam[10])>0))))
  })
  
  output$sensoryContr <- renderUI({
    sn<-dataInputSNAP1()
    i<-as.numeric(input$selectS)
    lo<-as.integer(sum(is.na(sn[,10+(i-7)*2])))
    up<-as.integer(floor(length(sn[,10+(i-7)*2])*0.99))
    
    helpText("Select upper and lower limits:")
    sliderInput ("LimitS",
                 "Order number:",
                 min = floor(lo/10)*10,
                 max = ceiling(up/10)*10,
                 value = c(lo+(up-lo)*0.25,up-(up-lo)*0.25))
  })
  
  output$cumPlotS<- renderPlot({
    # i<-9; ll<-200; ul<-300
    sn<-dataInputSNAP1()
    i<-as.numeric(input$selectS)
    ul<-input$LimitS[2]
    ll<-input$LimitS[1]
    nn<-(floor(length(sn[,10+(i-7)*2])*0.99)-sum(is.na(sn[,10+(i-7)*2])))
    con<-sn[order(sn[,i]),][ul,i]-((sn[order(sn[,i]),][ul,i]-sn[order(sn[,i]),][ll,i])/(ul-ll))*ul
    grad<-(sn[order(sn[,i]),][ul,i]-sn[order(sn[,i]),][ll,i])/(ul-ll)
    plot<-ggplot(sn)+
      geom_line(aes_string(x=names(sn)[10+(i-7)*2],y=names(sn)[i]))+
      #ggtitle(paste(input$NerveS,"; Age ",input$AgeS[1]," to ",input$AgeS[2],"; n ",nn,"; order ",ll," to ",ul,"; measure ",sn[which(sn[,13+(i-7)*2]==ll),i]," to ",sn[which(sn[,13+(i-6)*2]==ul),i],sep=""))+
      ggtitle(paste(input$NerveS,"; Age ",input$AgeS[1]," to ",input$AgeS[2],"; n ",nn,"; order ",ll," to ",ul,"; measure ",sn[which(sn[,10+(i-7)*2]==ll),i]," to ",sn[which(sn[,10+(i-7)*2]==ul),i],sep=""))+
      xlim(sum(is.na(sn[,10+(i-7)*2])),floor(length(sn[,10+(i-7)*2])*0.99))+
      #ylim(sn[order(sn[,i]),][sum(is.na(sn[,10+(i-7)*2]))+2,i],sn[order(sn[,i]),][floor(length(sn[,10+(i-7)*2])*input$XrangeS)-2,i])+
      ylim(sn[order(sn[,i]),][ceiling(sum(is.na(sn[,10+(i-7)*2]))+(length(sn[,10+(i-7)*2])*input$XrangeSL))+1,i],sn[order(sn[,i]),][floor(length(sn[,10+(i-7)*2])*input$XrangeS),i])+
      geom_abline(intercept=con, slope=grad,col="red")+
      geom_vline(xintercept = c(ll,ul), colour="blue", linetype = "longdash")+
      geom_hline(yintercept = c(sn[which(sn[,10+(i-7)*2]==ll),i],sn[which(sn[,10+(i-7)*2]==ul),i]),colour="green", linetype = "longdash" )
    return(plot) 
  })
  
  output$diffPlotS<- renderPlot({
    sn<-dataInputSNAP1()
    i<-as.numeric(input$selectS)
    ul<-input$LimitS[2]
    ll<-input$LimitS[1]
    nn<-(floor(length(sn[,10+(i-7)*2])*0.99)-sum(is.na(sn[,10+(i-7)*2])))
    return(ggplot(sn)+
             geom_point(aes_string(x=names(sn)[10+(i-7)*2],y=names(sn)[9+(i-7)*2]),col=2)+
             xlim(sum(is.na(sn[,10+(i-7)*2])),floor(length(sn[,10+(i-7)*2])*0.99))+
             ylim(min(sn[,9+(i-7)*2],na.rm=T),max(sn[order(sn[,9+(i-7)*2]),][1:floor(length(sn[,9+(i-7)*2])*input$YrangeS)-1,9+(i-7)*2]))+
             geom_vline(xintercept = c(ll,ul), colour="blue", linetype = "longdash"))
  })
  
  output$normPlotS<-renderPlot({
    sn<-dataInputSNAP1()
    i<-as.numeric(input$selectS)
    ul<-input$LimitS[2]
    ll<-input$LimitS[1]
    ggplot(sn)+
      stat_ecdf(aes_string(x=names(sn)[i]),col=2)+
      geom_vline(xintercept = c(sn[which(sn[,10+(i-7)*2]==ll),i],sn[which(sn[,10+(i-7)*2]==ul),i]),colour="green", linetype = "longdash" )+
      xlim(sn[order(sn[,i]),][ceiling(sum(is.na(sn[,10+(i-7)*2]))+(length(sn[,10+(i-7)*2])*input$XrangeSL))+1,i],sn[order(sn[,i]),][floor(length(sn[,10+(i-7)*2])*input$XrangeS),i])
    
  })
  
  output$normPlot2S<-renderPlot({
    sn<-dataInputSNAP1()
    i<-as.numeric(input$selectS)
    ul<-input$LimitS[2]
    ll<-input$LimitS[1]
    ggplot(sn)+
      geom_histogram(aes_string(x=names(sn)[i],y = "..density.."),fill="blue",alpha=0.5)+
      stat_function(
        fun = dnorm,
        args=with(sn, c(mean = mean(eval(parse(text=names(sn)[i]))), sd = sd(eval(parse(text=names(sn)[i]))))),
        colour="red")+
      geom_vline(xintercept = c(sn[which(sn[,10+(i-7)*2]==ll),i],sn[which(sn[,10+(i-7)*2]==ul),i]),colour="green", linetype = "longdash" )+
      xlim(sn[order(sn[,i]),][ceiling(sum(is.na(sn[,10+(i-7)*2]))+(length(sn[,10+(i-7)*2])*input$XrangeSL))+1,i],sn[order(sn[,i]),][floor(length(sn[,10+(i-7)*2])*input$XrangeS),i])
  })
  
  
  
  ##### EMG code follows ###
  dataInput<-reactive({
    path<-input$path
    ext<-input$emgstr
    #path<-"/Users/adam/Dropbox/Research/Matthew Pitt/enorms"
    #path<-"N:/EMG"
    #if(path=="/Users/adam/Dropbox/Research/Matthew Pitt/enorms") ext<-"EMG.xls"
    #if(path=="N:/EMG") ext<-"EMGMUP"
    flsEMG<-list.files(path=path,full.names = T)[grep(ext,list.files(path))]
    mups<-head(readWorksheet(loadWorkbook(flsEMG[1],create=T),sheet = "MUP"),1)[-1,]
    for (i in 1:length(flsEMG)){
      mups<-rbind(mups,readWorksheet(loadWorkbook(flsEMG[i],create=T),sheet = "MUP"))
    } ; mups<-mups[!duplicated(mups),]
    rm(list=c("i","flsEMG","path","ext"))
    mups[mups$Muscle=="Genioglossus",]$Muscle<-"Glossus"
    mups[,2]<-as.Date(mups[,2]); mups[,3]<-as.factor(mups[,3]); mups[,4]<-as.numeric(mups[,4]); mups[,5]<-as.factor(mups[,5])
    mups[,6]<-as.factor(mups[,6]);mups[,7]<-as.numeric(mups[,7]);mups[,8]<-as.numeric(mups[,8])
    mups[,9]<-as.numeric(mups[,9]);mups[,10]<-as.numeric(mups[,10]);mups[,11]<-as.numeric(mups[,11]);mups[,12]<-as.numeric(mups[,12])
    mups[,13]<-as.numeric(mups[,13])
    names(mups)[4]<-"Age"
    names(mups)[13]<-"Poly"
    return(mups)
  })
  
  dataInputlog<-reactive({
    mups<-dataInput()
    mups[,7:13]<-log(mups[,7:13])
    for(i in 7:13){
      mups[which(mups[,i]==-Inf),i]<-0
    }
    return(mups)
  })
 
  dataInput1<-reactive({
    mups<-dataInput()
    if (input$logE) {mups<-dataInputlog()} else {mups<-dataInput()}
    ta<-mups[(mups$Age>(input$Age[1]*365)&mups$Age<(input$Age[2]*365)&mups$Muscle==input$Muscle),] #select subgroup of age and muscle
    for (i in 7:13){
      # i<-7
      ta<-ta[order(ta[,i]),]
      ta[,(length(ta[1,])+1)]<-c(0,diff(ta[,i]))
      ta[,(length(ta[1,])+1)]<-1:length(ta[,1])
      ta[ta[,i]==0,(length(ta[1,]))]<-NA
    }
    names(ta)[14:27]<-c("simpleAmpD","orderSa", "simpleDurD","orderSd", "polyAmpD","orderPa",   "polyDurD" ,"orderPd",  "meanAmpD" ,"orderMa",  "meanDurD" ,"orderMd",  "PolyD","orderPx")
    return(ta)
  })
  
  output$sumTable <- renderTable({
    mups<-dataInput()
    nam<-names(mups)
    ddply(mups[(mups$Age>input$Age[1]*365 & mups$Age<input$Age[2]*365),c(5,7:13)],~Muscle,here(summarise),Total=length(get(nam[7])),SA=length((which(get(nam[7])>0.001) )),SD=length(which(get(nam[8])>0)),PA=length(which(get(nam[9])>0)),PD=length(which(get(nam[10])>0)),MA=length(which(get(nam[11])>0)),MD=length(which(get(nam[12])>0)),Pol=length(which(get(nam[13])>0)))
    #ddply(mups[(mups$Age>input$Age[1]*365 & mups$Age<input$Age[2]*365),c(5,7:13)],~Muscle,summarize,Total=length(get(paste(names(mups[7])))),SA=length(which(get(paste(names(mups[7])))>0)),SD=length(which(get(paste(names(mups[8])))>0)),PA=length(which(get(paste(names(mups[9])))>0)),PD=length(which(get(paste(names(mups[10])))>0)),MA=length(which(get(paste(names(mups[11])))>0)),MD=length(which(get(paste(names(mups[12])))>0)),Pol=length(which(get(paste(names(mups[13])))>0)))
  })
  
  output$emgControls <- renderUI({
    ta<-dataInput1()
    i<-as.numeric(input$select)
    lo<-as.integer(sum(is.na(ta[,13+(i-6)*2])))
    up<-as.integer(floor(length(ta[,13+(i-6)*2])*0.99))
    
    helpText("Select upper and lower limits:")
    sliderInput ("Limit",
                 "Order number:",
                 min = floor(lo/10)*10,
                 max = ceiling(up/10)*10,
                 value = c(lo+(up-lo)*0.25,up-(up-lo)*0.25))
  })
  
  output$cumPlot<- renderPlot({
    ta<-dataInput1()
    i<-as.numeric(input$select)
    ul<-input$Limit[2]
    ll<-input$Limit[1]
    nn<-(floor(length(ta[,13+(i-6)*2])*0.99)-sum(is.na(ta[,13+(i-6)*2])))
    con<-ta[order(ta[,i]),][ul,i]-((ta[order(ta[,i]),][ul,i]-ta[order(ta[,i]),][ll,i])/(ul-ll))*ul
    grad<-(ta[order(ta[,i]),][ul,i]-ta[order(ta[,i]),][ll,i])/(ul-ll)
    plot<-ggplot(ta)+
      geom_line(aes_string(x=names(ta)[13+(i-6)*2],y=names(ta)[i]))+
      ggtitle(paste(input$Muscle,"; Age ",input$Age[1]," to ",input$Age[2],"; n ",nn,"; order ",ll," to ",ul,"; measure ",ta[which(ta[,13+(i-6)*2]==ll),i]," to ",ta[which(ta[,13+(i-6)*2]==ul),i],sep=""))+
      xlim(sum(is.na(ta[,13+(i-6)*2])),floor(length(ta[,13+(i-6)*2])*0.99))+
      #ylim(ta[order(ta[,i]),][sum(is.na(ta[,13+(i-6)*2])),i],ta[order(ta[,i]),][floor(length(ta[,13+(i-6)*2])*input$Xrange),i])+
      ylim(ta[order(ta[,i]),][ceiling(sum(is.na(ta[,13+(i-6)*2]))+(length(ta[,13+(i-6)*2])*input$XrangeL))+1,i],ta[order(ta[,i]),][floor(length(ta[,13+(i-6)*2])*input$Xrange),i])+
      geom_abline(intercept=con, slope=grad,col="red")+
      geom_vline(xintercept = c(ll,ul), colour="blue", linetype = "longdash")+
      geom_hline(yintercept = c(ta[which(ta[,13+(i-6)*2]==ll),i],ta[which(ta[,13+(i-6)*2]==ul),i]),colour="green", linetype = "longdash" )
    return(plot) 
  })
  
  output$difPlot<- renderPlot({
    ta<-dataInput1()
    i<-as.numeric(input$select)
    ul<-input$Limit[2]
    ll<-input$Limit[1]
    nn<-(floor(length(ta[,13+(i-6)*2])*0.99)-sum(is.na(ta[,13+(i-6)*2])))
    return(ggplot(ta)+
             geom_point(aes_string(x=names(ta)[13+(i-6)*2],y=names(ta)[12+(i-6)*2]),col=2)+
             xlim(sum(is.na(ta[,13+(i-6)*2])),floor(length(ta[,13+(i-6)*2])*0.99))+
             ylim(min(ta[,12+(i-6)*2],na.rm=T),max(ta[order(ta[,12+(i-6)*2]),][1:floor(length(ta[,12+(i-6)*2])*input$Yrange)-1,12+(i-6)*2]))+
             geom_vline(xintercept = c(ll,ul), colour="blue", linetype = "longdash"))
  })
  
  output$normPlot<-renderPlot({
    ta<-dataInput1()
    i<-as.numeric(input$select)
    ul<-input$Limit[2]
    ll<-input$Limit[1]
    ggplot(ta)+
      stat_ecdf(aes_string(x=names(ta)[i]),col=2)+
      geom_vline(xintercept = c(ta[which(ta[,13+(i-6)*2]==ll),i],ta[which(ta[,13+(i-6)*2]==ul),i]),colour="green", linetype = "longdash" )+
      #xlim(ta[order(ta[,i]),][sum(is.na(ta[,13+(i-6)*2])),i],ta[order(ta[,i]),][floor(length(ta[,13+(i-6)*2])*0.99),i])
      xlim(ta[order(ta[,i]),][ceiling(sum(is.na(ta[,13+(i-6)*2]))+(length(ta[,13+(i-6)*2])*input$XrangeL)),i],ta[order(ta[,i]),][floor(length(ta[,13+(i-6)*2])*input$Xrange),i])
  })
  
  output$normPlot2<-renderPlot({
    ta<-dataInput1()
    i<-as.numeric(input$select)
    ul<-input$Limit[2]
    ll<-input$Limit[1]
    ggplot(ta)+
      geom_histogram(aes_string(x=names(ta)[i],y = "..density.."),fill="blue",alpha=0.5)+
      stat_function(
        fun = dnorm,
        args=with(ta, c(mean = mean(eval(parse(text=names(ta)[i]))), sd = sd(eval(parse(text=names(ta)[i]))))),
        colour="red")+
      geom_vline(xintercept = c(ta[which(ta[,13+(i-6)*2]==ll),i],ta[which(ta[,13+(i-6)*2]==ul),i]),colour="green", linetype = "longdash" )+
      #xlim(ta[order(ta[,i]),][sum(is.na(ta[,13+(i-6)*2])),i],ta[order(ta[,i]),][floor(length(ta[,13+(i-6)*2])*0.99),i])
      xlim(ta[order(ta[,i]),][ceiling(sum(is.na(ta[,13+(i-6)*2]))+(length(ta[,13+(i-6)*2])*input$XrangeL)),i],ta[order(ta[,i]),][floor(length(ta[,13+(i-6)*2])*input$Xrange),i])
       })
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste(Sys.time())
  })
})
