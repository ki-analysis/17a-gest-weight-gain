rootDir<-file.path("H:/GHAP/QuantSci/")
dataDir<-file.path(rootDir,"HBGD/")
libDir<-.libPaths()[1] 
library(tidyverse)
library(GGally)
library(haven)

sum_na <-function(x)sum(is.na(x))
sum_present <-function(x)sum(!is.na(x))

dat_rally17 <- 
  read_sas(file.path(dataDir,"rally-17/Sprint A/adam/rally_17.sas7bdat"))

dat_rally17 %>% filter(!is.na(M_WTKG)) %>% select(STUDYID,SUBJID,CTRYCD) %>% distinct() %>% select(-SUBJID) %>% table()

# STUDYID                    BGD   BRA   CHN   COD   GBR   GHA   GTM   IND   ITA   KEN   MWI   OMN   PAK   THA   USA   ZAF
#   INTERBIO-21                0   352     0     0   614     0     0     0     0   465     0     0   432   546     0   437
#   INTERGROWTH-21             0   430   616     0   660     0     0   647   538   646     0   646     0     0   317     0
#   ki1000153-HealthyStart     0     0     0     0     0     0     0     0     0     0     0     0     0     0  1364     0
#   ki1000307-St-Johns         0     0     0     0     0     0     0  2001     0     0     0     0     0     0     0     0
#   ki1033518-iLiNS-DYAD-G     0     0     0     0     0  1301     0     0     0     0     0     0     0     0     0     0
#   ki1055867-WomenFirst       0     0     0   634     0     0   666   641     0     0     0     0   727     0     0     0
#   ki1148112-iLiNS-DYAD-M     0     0     0     0     0     0     0     0     0     0  1245     0     0     0     0     0
#   kiGH5241-JiVitA-3      28493     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0


dat_selected <- dat_rally17 %>% mutate(EPOCH=replace(EPOCH,EPOCH=="Pre-pregnancy","Pre-Pregnancy")) %>%
  select(STUDYID,COUNTRY,SUBJID,GAGEDAYS,GAGEBRTH,M_WTKG,EPOCH) %>% 
  mutate(COMPWEEKS=trunc(GAGEDAYS/7)) %>%
  mutate(EPOCH=replace(EPOCH,EPOCH=="Pregnancy"&COMPWEEKS<13,"Pregnancy (T1)")) %>%
  mutate(EPOCH=replace(EPOCH,EPOCH=="Pregnancy"&COMPWEEKS%in%c(13:28),"Pregnancy (T2)")) %>%
  mutate(EPOCH=replace(EPOCH,EPOCH=="Pregnancy"&COMPWEEKS>28,"Pregnancy (T3)")) %>%
  na.omit() %>% droplevels()


dat_women1st <- dat_selected %>% 
  filter(STUDYID=="ki1055867-WomenFirst") %>% 
  select(SUBJID,EPOCH,COMPWEEKS,GAGEDAYS,GAGEBRTH,M_WTKG) %>%
  na.omit() %>% droplevels()

dat_women1st <- dat_women1st %>% filter(M_WTKG<100) 


dat_women1st %>%  
  group_by(SUBJID) %>% 
  mutate(NEW=abs(c(0,diff(M_WTKG)/diff(GAGEDAYS)))) %>% 
  summarize(max=max(NEW)) %>% #select(max) %>% filter(max<0.75) %>% ungroup() %>% unlist() %>% hist() #arrange(-max)
  filter(max>0.75) %>% 
  select(SUBJID) %>% unlist() %>% as.numeric() -> ID_SPIKY

dat_women1st %>% # mutate(NEW=abs(c(0,diff(M_WTKG)))) %>% 
  filter(SUBJID %in% ID_SPIKY) %>% ggplot(aes(x=GAGEDAYS,y=M_WTKG,group=SUBJID,col=as.factor(SUBJID))) +geom_line() +
  geom_vline(xintercept=40*7)

dat_women1st <- dat_women1st %>% filter(!SUBJID%in%ID_SPIKY) %>% filter(GAGEDAYS<=GAGEBRTH,GAGEBRTH<45*7,GAGEDAYS>(-26*7))

dat_women1st %>% ggplot(aes(x=GAGEDAYS,y=M_WTKG,group=SUBJID)) + geom_line(alpha=0.5) + theme_bw()


dat_women1st %>% group_by(SUBJID) %>% summarize(n=n()) %>% ungroup() %>% top_n(wt=-n,n=4)  %>% select(SUBJID) %>% unlist() %>% as.numeric()
# 4381  5171  6991  8641 13171 13821 19481 25421 26041 27601 30751 32261 32641 32891 33491 41711 46081 46521 47931 48481 49821 57931 59691 61801 61811 64141 64261 72231

dat_women2nd <- dat_women1st %>% 
  mutate(T1=pmin(13*7,pmax(0,GAGEDAYS)),
         T2=pmin(29*7,pmax(13*7,GAGEDAYS))-13*7,
         T3=pmin(GAGEBRTH,pmax(29*7,GAGEDAYS))-29*7,
         SUBJID=factor(SUBJID))

dat_test <- dat_women2nd %>% filter(SUBJID==4381)

dat_test %>% ggplot(aes(x=GAGEDAYS,y=M_WTKG))+geom_point()+geom_line()
lm_test <- lm(M_WTKG~1+T1+T2+T3,data=dat_test)
dat_new<- data.frame(GAGEDAYS=seq(-200,320,1),GAGEBRTH=320) %>%
  mutate(T1=pmin(13*7,pmax(0,GAGEDAYS)),
         T2=pmin(29*7,pmax(13*7,GAGEDAYS))-13*7,
         T3=pmin(GAGEBRTH,pmax(29*7,GAGEDAYS))-29*7,
         COMPWEEKS=trunc(GAGEDAYS/7),
         EPOCH="Pre-Pregnancy") %>%
  mutate(EPOCH=replace(EPOCH,COMPWEEKS>=0&COMPWEEKS<13,"Pregnancy (T1)")) %>%
  mutate(EPOCH=replace(EPOCH,COMPWEEKS>=13&COMPWEEKS<29,"Pregnancy (T2)")) %>%
  mutate(EPOCH=replace(EPOCH,COMPWEEKS>=29,"Pregnancy (T3)")) 



ggplot(dat_test,aes(x=GAGEDAYS)) + theme_bw() + geom_point(aes(y=M_WTKG)) + geom_line(data=dat_new %>% mutate(FITTED=predict(lm_test,newdata=dat_new)) ,
                                                                                      aes(y=FITTED,col=EPOCH)) 


require(nlme)

lme_full <- lme(fixed = M_WTKG ~ 1+T1+T2+T3, 
                data= dat_women2nd, 
                random= ~ 1+T1+T2+T3|SUBJID)
i<-6
dat_new2<-dat_new %>% mutate(FITTED=apply(matrix(rep(as.numeric(fixef(lme_full)+ranef(lme_full)[i,]),each=nrow(dat_new)),ncol=4) * 
                                            cbind(1,dat_new%>%select(T1,T2,T3)),1,sum))
ggplot()+geom_point(data=dat_women2nd %>% filter(SUBJID==levels(unique(lme_full$groups[1,]))[i]),aes(x=GAGEDAYS,y=M_WTKG))+theme_bw()+
  geom_line(data=dat_new2,aes(x=GAGEDAYS,y=FITTED,col=EPOCH))
############ now try with another study added 



dat_women1st <- dat_selected %>% 
  filter(STUDYID%in%c("ki1055867-WomenFirst","INTERBIO-21")) %>% 
  select(STUDYID,SUBJID,EPOCH,COMPWEEKS,GAGEDAYS,GAGEBRTH,M_WTKG) %>%
  na.omit() %>% droplevels()

dat_women1st <- dat_women1st %>% filter(M_WTKG<100) %>% mutate(USUBJID=factor(paste(STUDYID,SUBJID)))


dat_women1st %>%  
  group_by(USUBJID) %>% 
  mutate(NEW=abs(c(0,diff(M_WTKG)/diff(GAGEDAYS)))) %>% 
  summarize(max=max(NEW)) %>% #select(max) %>% filter(max<0.75) %>% ungroup() %>% unlist() %>% hist() #arrange(-max)
  filter(max>0.75) %>% 
  select(USUBJID) %>% unlist() %>% as.numeric() -> ID_SPIKY


dat_women1st <- dat_women1st %>% filter(!USUBJID%in%ID_SPIKY) %>% filter(GAGEDAYS<=GAGEBRTH,GAGEBRTH<45*7,GAGEDAYS>(-26*7))



dat_women1st %>% group_by(USUBJID) %>% summarize(n=n()) %>% ungroup() %>% top_n(wt=n,n=4)  %>% select(USUBJID) %>% unlist()

dat_women2nd <- dat_women1st %>% 
  mutate(T1=pmin(13*7,pmax(0,GAGEDAYS)),
         T2=pmin(29*7,pmax(13*7,GAGEDAYS))-13*7,
         T3=pmin(GAGEBRTH,pmax(29*7,GAGEDAYS))-29*7)

dat_test <- dat_women2nd %>% filter(USUBJID=="ki1055867-WomenFirst 13171")

dat_test %>% ggplot(aes(x=GAGEDAYS,y=M_WTKG))+geom_point()+geom_line()

dat_test<-dat_test %>% mutate(FITTED=lm(M_WTKG~1+T1+T2+T3,data=dat_test) %>% predict())
ggplot(dat_test,aes(x=GAGEDAYS)) + theme_bw() + geom_point(aes(y=M_WTKG)) + geom_line(aes(y=FITTED,col=EPOCH)) 


lme_full2 <- lme(fixed = M_WTKG ~ 1+STUDYID+T1+T2+T3, 
                 data= dat_women2nd, 
                 random= ~ 1+T1+T2+T3|USUBJID)



