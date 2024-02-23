rootDir<-file.path("H:/GHAP/QuantSci/")
dataDir<-file.path(rootDir,"HBGD/")
teamDir<-file.path(rootDir,"HBGD-Teams/fetal/")
scriptDir<-file.path(teamDir,"jayson.wilbur")
figDir<-file.path("U:/Sprint17A_TFL")
libDir<-.libPaths()[1] 
setwd(scriptDir)

library(tidyverse)
library(GGally)
library(haven)

sum_na <-function(x)sum(is.na(x))
sum_present <-function(x)sum(!is.na(x))

dat_rally17 <- 
  read_sas(file.path(dataDir,"rally-17/Sprint A/adam/rally_17.sas7bdat"))

# dat_rally17 %>% filter(!is.na(M_WTKG)) %>% select(STUDYID,UID,CTRYCD) %>% distinct() %>% select(-UID) %>% table()

# STUDYID                  BGD   BRA   CHN   COD   GBR   GHA   GTM   IND   ITA   KEN   MWI   OMN   PAK   THA   USA   ZAF
# INTERBIO-21                0   352     0     0   614     0     0     0     0   465     0     0   432   546     0   437
# INTERGROWTH-21             0   430   616     0   660     0     0   647   538   646     0   646     0     0   317     0
# ki1000153-HealthyStart     0     0     0     0     0     0     0     0     0     0     0     0     0     0  1364     0
# ki1000307-St-Johns         0     0     0     0     0     0     0  2001     0     0     0     0     0     0     0     0
# ki1033518-iLiNS-DYAD-G     0     0     0     0     0  1301     0     0     0     0     0     0     0     0     0     0
# ki1055867-WomenFirst       0     0     0   634     0     0   666   641     0     0     0     0   727     0     0     0
# ki1148112-iLiNS-DYAD-M     0     0     0     0     0     0     0     0     0     0  1245     0     0     0     0     0
# kiGH5241-JiVitA-3      28493     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0


dat_selected <- dat_rally17 %>% mutate(EPOCH=replace(EPOCH,EPOCH=="Pre-pregnancy","Pre-Pregnancy")) %>%
  select(STUDYID,CTRYCD,SITEID,UID,GAGEDAYS,GAGEBRTH,M_WTKG,EPOCH) %>% 
  mutate(COMPWEEKS=trunc(GAGEDAYS/7)) %>%
  mutate(EPOCH=replace(EPOCH,EPOCH=="Pregnancy"&COMPWEEKS<13,"Pregnancy (T1)")) %>%
  mutate(EPOCH=replace(EPOCH,EPOCH=="Pregnancy"&COMPWEEKS%in%c(13:28),"Pregnancy (T2)")) %>%
  mutate(EPOCH=replace(EPOCH,EPOCH=="Pregnancy"&COMPWEEKS>28,"Pregnancy (T3)")) %>%
  mutate(GROUP=factor(paste(STUDYID,CTRYCD,SITEID,sep="-"))) %>%
  na.omit() %>% droplevels()

# ggplot(dat_selected,aes(x=GAGEDAYS,y=M_WTKG,group=UID,col=STUDYID,facet=STUDYID)) +
#   geom_line() + facet_wrap(~STUDYID,ncol=1) + theme_bw() + 
#   theme(legend.position="none")


dat_selected <- dat_selected %>%
  # Filter out weights far in time from pregnancy
  filter(GAGEDAYS<=GAGEBRTH,GAGEBRTH<45*7,GAGEDAYS>(-26*7)) %>%
  # Filter out spikes > 100 kg in Woment First study
  filter(!(STUDYID=="ki1055867-WomenFirst" & M_WTKG >100)) %>%
  droplevels()

dat_selected %>% select(UID) %>% distinct() %>% dim()

# ggplot(dat_selected,aes(x=GAGEDAYS,y=M_WTKG,group=UID,col=STUDYID,facet=STUDYID)) +
#   geom_line() + facet_wrap(~STUDYID,ncol=1) + theme_bw() + 
#   theme(legend.position="none")


dat_selected %>%  
  group_by(UID) %>% 
  mutate(NEW=abs(c(0,diff(M_WTKG)/diff(GAGEDAYS)))) %>% 
  summarize(max=max(NEW)) %>%
  filter(max>0.75) %>% 
  select(UID) %>% unlist() %>% as.numeric() -> ID_SPIKY

length(ID_SPIKY)  # 511 subjects

dat_selected <- dat_selected %>% filter(!UID%in%ID_SPIKY) 


# ggplot(dat_selected,aes(x=GAGEDAYS,y=M_WTKG,group=UID,col=STUDYID,facet=STUDYID)) +
#   geom_line() + facet_wrap(~STUDYID,ncol=1) + theme_bw() + 
#   theme(legend.position="none")


dat_selected <- dat_selected %>% 
  mutate(T1=pmin(13*7,pmax(0,GAGEDAYS)),
         T2=pmin(29*7,pmax(13*7,GAGEDAYS))-13*7,
         T3=pmin(GAGEBRTH,pmax(29*7,GAGEDAYS))-29*7,
         UID=factor(UID))



require(nlme)
memory.limit(10000)


# lme_full <- lme(fixed = M_WTKG ~ 1+T1+T2+T3, 
#                  data= dat_selected, 
#                  random= ~ 1+T1+T2+T3|UID)

# save(lme_full,file="U:lme_full.RData")
# save(lme_full,file="H:lme_full.RData")

load("H:lme_full.RData")


plot_subject <- function(i){
  COEF <- fixef(lme_full)+ranef(lme_full)[i,]
  dat_test <- data.frame(GAGEDAYS=seq(min(dat_selected$GAGEDAYS),max(dat_selected$GAGEDAYS),1)) %>%
    mutate(T1=pmin(13*7,pmax(0,GAGEDAYS)),
           T2=pmin(29*7,pmax(13*7,GAGEDAYS))-13*7,
           T3=pmax(29*7,GAGEDAYS)-29*7)
  dat_test <- dat_test %>% mutate(FITTED = as.matrix(cbind(1,dat_test[,2:4]))%*%t(as.matrix(COEF)))
  
  p1 <- ggplot(dat_test,aes(x=GAGEDAYS/7,y=FITTED))+
    geom_line()+theme_bw()+labs(x="Gestational age (weeks)",y="Maternal weight (kg)",
                                title=dat_selected %>% filter(UID==unique(dat_selected$UID)[i]) %>% select(STUDYID,CTRYCD,SITEID,UID) %>% distinct() %>% paste(collapse=" "))+
    geom_point(data=dat_selected %>% filter(UID==unique(dat_selected$UID)[i]),
               aes(x=GAGEDAYS/7,y=M_WTKG))
  print(p1)
}

# lme_full_group <- lme(fixed = M_WTKG ~ 1+GROUP+T1+T2+T3, 
#                 data= dat_selected, 
#                 random= ~ 1+T1+T2+T3|UID)

# save(lme_full_group,file="U:lme_full_group.RData")
# save(lme_full_group,file="H:lme_full_group.RData")

load("H:lme_full_group.RData")

plot_subject_group <- function(i){
  COEF <- fixef(lme_full_group)[c(1,28,29,30)]+ranef(lme_full_group)[i,]
  GROUPI <- dat_selected %>% filter(UID==i) %>% select(GROUP) %>% distinct() %>% unlist() %>% as.numeric()
  if(GROUPI>1) COEF[1]<-COEF[1]+fixef(lme_full_group)[GROUPI]
  dat_test <- data.frame(GAGEDAYS=seq(min(dat_selected$GAGEDAYS),max(dat_selected$GAGEDAYS),1)) %>%
    mutate(T1=pmin(13*7,pmax(0,GAGEDAYS)),
           T2=pmin(29*7,pmax(13*7,GAGEDAYS))-13*7,
           T3=pmax(29*7,GAGEDAYS)-29*7)
  dat_test <- dat_test %>% mutate(FITTED = as.matrix(cbind(1,dat_test[,2:4]))%*%t(as.matrix(COEF)))
  
  p1 <- ggplot(dat_test,aes(x=GAGEDAYS/7,y=FITTED))+
    geom_line()+theme_bw()+labs(x="Gestational age (weeks)",y="Maternal weight (kg)",
                                title=dat_selected %>% filter(UID==unique(dat_selected$UID)[i]) %>% select(STUDYID,CTRYCD,SITEID,UID) %>% distinct() %>% paste(collapse=" "))+
    geom_point(data=dat_selected %>% filter(UID==unique(dat_selected$UID)[i]),
               aes(x=GAGEDAYS/7,y=M_WTKG))
  print(p1)
}





# ggplot(dat_coef2,aes(x=GROUP,y=MW_BIRTH,fill=CTRYCD)) + 
#   geom_boxplot(outlier.shape=NA) + theme_bw() + 
#   coord_flip() + theme(legend.position="none")   +
#   ylim(25,125)
# 
# ggplot(dat_coef2,aes(x=GROUP,y=GWG_HAT,fill=CTRYCD)) + 
#   geom_boxplot(outlier.shape=NA) + theme_bw() + 
#   coord_flip() + theme(legend.position="none")  +
#   ylim(-5,25)
# 
# ggplot(dat_coef2,aes(x=PPBMI,y=GWG_HAT,col=CTRYCD)) + geom_point()
# 
# ggplot(dat_coef,aes(x=GROUP,y=Intercept,fill=CTRYCD)) + 
#   geom_boxplot(outlier.shape=NA) + theme_bw() + 
#   coord_flip() + theme(legend.position="none")  +
#   ylim(25,110)
# 
# 
# ggplot(dat_coef,aes(x=GROUP,y=T1,fill=CTRYCD)) + 
#   geom_boxplot(outlier.shape=NA) + theme_bw() + 
#   coord_flip() + theme(legend.position="none") +
#   ylim(-0.06,0.04)
# 
# 
# ggplot(dat_coef,aes(x=GROUP,y=T3,fill=CTRYCD)) + 
#   geom_boxplot(outlier.shape=NA) + theme_bw() + 
#   coord_flip() + theme(legend.position="none") 
# 
# 
# ggplot(dat_coef,aes(x=GROUP,y=T3,fill=CTRYCD)) + 
#   geom_boxplot(outlier.shape=NA) + theme_bw() + 
#   coord_flip() + theme(legend.position="none") 
# 
# ggplot(dat_coef,aes(x=GROUP,y=T3,fill=CTRYCD)) + 
#   geom_boxplot(outlier.shape=NA) + theme_bw() + 
#   coord_flip() + theme(legend.position="none") + 
#   ylim(0,0.2)
# 
# ggplot(dat_coef,aes(x=GROUP,y=T2/T3,fill=CTRYCD)) + 
#   geom_boxplot(outlier.shape=NA) + theme_bw() + 
#   coord_flip() + theme(legend.position="none") + 
#   ylim(0,2)
# 
# ggplot(dat_coef,aes(x=GROUP,y=PPBMI,fill=CTRYCD)) + 
#   geom_boxplot(outlier.shape=NA) + theme_bw() + 
#   coord_flip() + theme(legend.position="none")+ 
#   ylim(10,40)
# 
# 
# ggplot(dat_coef,aes(x=PPBMI,y=T3,col=GROUP)) + 
#   geom_point() + theme_bw() +  theme(legend.position="none")
# 
# ggplot(dat_coef,aes(x=PPBMI,y=T1,col=GROUP)) + 
#   geom_point() + theme_bw() +  theme(legend.position="none")

dat_selected <- dat_selected %>% mutate(T4=T2+T3)

 
# lme_group_t2t3 <- lme(fixed = M_WTKG ~ 1+GROUP+T1+T4,
#                       data= dat_selected,
#                       random= ~ 1+T1+T4|UID)
# 
# save(lme_group_t2t3,file="U:lme_group_t2t3.RData")
# save(lme_group_t2t3,file="H:lme_group_t2t3.RData")
load("H:lme_group_t2t3.RData")

# lme_group_onechange <- lme(fixed = M_WTKG ~ 1+GROUP+T4,
#                       data= dat_selected,
#                       random= ~ 1+T4|UID,
#                       control=lmeControl(opt="optim"))
# 
# 
# save(lme_group_onechange,file="U:lme_group_onechange.RData")
# save(lme_group_onechange,file="H:lme_group_onechange.RData")

load("H:lme_group_onechange.RData")

# lme_onechange <- lme(fixed = M_WTKG ~ 1+T4,
#                      data= dat_selected,
#                      random= ~ 1+T4|UID)
# 
# save(lme_onechange,file="U:lme_onechange.RData")
# save(lme_onechange,file="H:lme_onechange.RData")

load("H:lme_onechange.RData")


# lme_ctry_onechange <- lme(fixed = M_WTKG ~ 1+CTRYCD+T4,
#                            data= dat_selected,
#                            random= ~ 1+T4|UID,
#                            control=lmeControl(opt="optim"))


# save(lme_ctry_onechange,file="U:lme_ctry_onechange.RData")
# save(lme_ctry_onechange,file="H:lme_ctry_onechange.RData")

load("H:lme_ctry_onechange.RData")

AIC(lme_full,lme_full_group,lme_group_t2t3,lme_group_onechange,lme_onechange,lme_ctry_onechange)
BIC(lme_full,lme_full_group,lme_group_t2t3,lme_group_onechange,lme_onechange,lme_ctry_onechange)

# set.seed(123456)
# test_set <- dat_selected %>% select(UID,GROUP) %>% distinct() %>% 
#   group_by(GROUP) %>% sample_frac(0.5) %>% 
#   ungroup() %>% select(UID) %>% unlist() %>% as.character() %>% as.numeric() 
# 
# dat_train <- dat_selected %>% filter(!UID %in% test_set)
# dat_test <- dat_selected %>% filter(UID %in% test_set)
# 
# dat_train %>% select(GROUP) %>% table()
# dat_test %>% select(GROUP) %>% table()

# USE GROUP2 = JUST STUDY*COUNTRY




COEF <- ranef(lme_full_group) %>% rename(Intercept='(Intercept)') %>%
  mutate(Intercept=Intercept+fixef(lme_full_group)[1],
         T1=T1+fixef(lme_full_group)[28],
         T2=T2+fixef(lme_full_group)[29],
         T3=T3+fixef(lme_full_group)[30]
  )


FIXED <- fixef(lme_full_group) %>% as.data.frame() 
FIXED <- FIXED %>% mutate(GROUP=row.names(FIXED)) 
names(FIXED)[1] <- "EFFECT"

COUNTS <- dat_selected %>% select(UID,EPOCH) %>% table() %>% as.matrix() 


dat_coef <- dat_selected %>% select(UID,STUDYID,CTRYCD,SITEID,GAGEBRTH,GROUP) %>% distinct() %>% 
  mutate(GROUP=paste("GROUP",GROUP,sep="")) %>% 
  left_join(FIXED,by="GROUP") %>%
  mutate(EFFECT=replace(EFFECT,is.na(EFFECT),0)) %>% cbind(COEF) %>%
  mutate(n0=COUNTS[,2] %>% as.numeric(),
         n1=COUNTS[,3] %>% as.numeric(),
         n2=COUNTS[,4] %>% as.numeric(),
         n3=COUNTS[,5] %>% as.numeric()) %>%
  mutate(Intercept=Intercept+EFFECT) %>% select(-EFFECT)

dat_height <- dat_rally17 %>% select(UID,M_HTCM) %>% na.omit() %>% group_by(UID) %>% top_n(n=1) %>% ungroup()

dat_coef <- dat_coef %>% mutate(UID=as.integer(as.character(UID))) %>% left_join(dat_height,by="UID") 
dat_coef <- dat_coef %>% mutate(PPBMI = Intercept/(M_HTCM/100)^2)

dat_coef <- dat_coef %>% 
  mutate(T1B=pmin(13*7,pmax(0,GAGEBRTH)),
         T2B=pmin(29*7,pmax(13*7,GAGEBRTH))-13*7,
         T3B=pmax(29*7,GAGEBRTH)-29*7) %>%
  mutate(MW_BIRTH=Intercept+T1*T1B+T2*T2B+T3*T3B,
         GWG_HAT = T1*T1B+T2*T2B+T3*T3B,
         GWG_37_HAT = 13*7*T1+(29-13)*7*T2+(37-29)*7*T3)


dat_coef <- dat_coef %>% 
  left_join((dat_rally17 %>% select(UID,BIRTHWT,BIRTHLEN,
                                    BIRTHHC,GAGECM,PREGOUT,MSCR,
                                    STLBRTH,LVBRTH) %>% distinct()),by="UID")

ggpairs(dat_coef %>% select(Intercept,T1,T2,T3))+theme_bw()




