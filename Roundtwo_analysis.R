#Roundtwo_analysis

Roundtwo_analysis<-function(imported_data,type){
  #initializing
R2_metadata<-data.frame(R2_PA=0,T1=0,T2=0,GB=0,LA=0,ND=0,NE=0,UP=0,tp=0,PP=0,PUP=0)

  
  
  column_numbers_adobe_round2<-c(1,2,3,4) #FILLER
  column_numbers_webx_round2<-c(3,10,33,34,37,40,42,43)
  column_names_adobe_round2<-c("blah","blah","blah") #FILLER
  column_names_webx_round2<-c("Email","Status","Role","Role_other","Org_name","Region","Track","ID")
  
   if(type=="adobe"){
    cleaned_data<-imported_data[,column_numbers_adobe_round2]
    colnames(cleaned_data)<-column_names_adobe_round2
  }else if(type=="webx"){
    cleaned_data<-imported_data[,column_numbers_webx_round2]
    colnames(cleaned_data)<-column_names_webx_round2
  }
  
  ##Total Numbers
  
  attendance<-(sum(cleaned_data$Status=="Attended",na.rm = TRUE))
  attendance_rate<-(attendance)/length(cleaned_data$Status)
  
  
  #Adding practice participants-- from attended only
  attended_df<-cleaned_data[(cleaned_data$Status=="Attended"),]
  admin<-sum(attended_df$Role=="Administration",na.rm = TRUE)
  drs<-sum(attended_df$Role=="Clinicians providing healthcare services within context of model",na.rm = TRUE)
  cm<-sum(attended_df$Role=="Care Manager",na.rm = TRUE)
  da<-sum(attended_df$Role=="Data Analyst",na.rm = TRUE)
  pm<-sum(attended_df$Role=="Project Manager/Coordinator",na.rm = TRUE)
  pd<-sum(attended_df$Role=="Project Director/Principal Investigator",na.rm = TRUE)
  qi<-sum(attended_df$Role=="Quality Improvement Director",na.rm = TRUE)
  PA<-sum(admin,drs,cm,da,pm,pd,qi)

  #Adding non-practice attendees
  cms<-sum(attended_df$Role=="CMS",na.rm = TRUE)
  cnt<-sum(attended_df$Role=="Contractor",na.rm = TRUE)
  pay<-sum(attended_df$Role_other=="Payer",na.rm = TRUE)
  ven<-sum(attended_df$Role_other=="Vendor",na.rm = TRUE)

  #Practices
  up=length(unique(attended_df$ID))
  tp=sum(attended_df$ID!="Not Available",na.rm = TRUE)
  
  
  #Round 2 Numbers
  R1<-c("AR","CO","HI","MO","LA","MI","MT","NE","NJ","NY","ND","OH","OK","OR","PA","RI","TN")
  R2_D2<-c("GB","LA","NE","ND")
  round2<-attended_df[(attended_df$Region %in% R2_D2),]
  
  admin_R2<-sum(round2$Role=="Administration",na.rm = TRUE)
  drs_R2<-sum(round2$Role=="Clinicians providing healthcare services within context of model",na.rm = TRUE)
  cm_R2<-sum(round2$Role=="Care Manager",na.rm = TRUE)
  da_R2<-sum(round2$Role=="Data Analyst",na.rm = TRUE)
  pm_R2<-sum(round2$Role=="Project Manager/Coordinator",na.rm = TRUE)
  pd_R2<-sum(round2$Role=="Project Director/Principal Investigator",na.rm = TRUE)
  qi_R2<-sum(round2$Role=="Quality Improvement Director",na.rm = TRUE)
  PA_R2<-sum(admin_R2,drs_R2,cm_R2,da_R2,pm_R2,pd_R2,qi_R2)
  
  #Adding non-practice attendees--- there shouldn't be any in there... but just in case
  cms_R2<-sum(round2$Role=="CMS",na.rm = TRUE)
  cnt_R2<-sum(round2$Role=="Contractor",na.rm = TRUE)
  pay_R2<-sum(round2$Role_other=="Payer",na.rm = TRUE)
  ven_R2<-sum(round2$Role_other=="Vendor",na.rm = TRUE)
  
  #Practices
  up_R2=length(unique(round2$ID))
  tp_R2=sum(round2$ID!="Not Available",na.rm = TRUE)
  
  #Round2 Specifics
  
  T1_R2<-sum(round2$Track=="T1",na.rm = TRUE)
  T2_R2<-sum(round2$Track=="T2",na.rm = TRUE)
  
  #Round2 Regions
  
  GB_R2<-sum(round2$Region=="GB",na.rm = TRUE)
  LA_R2<-sum(round2$Region=="LA",na.rm = TRUE)
  ND_R2<-sum(round2$Region=="ND",na.rm = TRUE)
  NE_R2<-sum(round2$Region=="NE",na.rm = TRUE)
  
  
  #Counting R1 vs R2 Regions
Round1<-length(attended_df[(attended_df$Region %in% R1),])  
Round2<-length(attended_df[(attended_df$Region %in% R2_D2),])

percent_practices<-tp_R2/tp

percent_uniquepractices<-up_R2/up

R2_metadata$R2_PA<-PA_R2
R2_metadata$T1<-T1_R2
R2_metadata$T2<-T2_R2
R2_metadata$GB<-GB_R2
R2_metadata$LA<-LA_R2
R2_metadata$ND<-ND_R2
R2_metadata$NE<-NE_R2
R2_metadata$UP<-up_R2
R2_metadata$TP<-tp_R2
R2_metadata$PP<-percent_practices
R2_metadata$PUP<-percent_uniquepractices

return(R2_metadata)





}


