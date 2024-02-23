setwd("H:/GHAP/QuantSci/HBGD-Teams/fetal/jayson.wilbur")

library(tidyverse)
library(readr)
library(sn)
library(MASS)
library(rstan)
library(GGally)
library(gridExtra)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

memory.limit(100000)

dat <- readRDS("SINASC/snsc_2011-2015.rds")

sum_na <- function(x)sum(is.na(x))
sum_not_na <- function(x)sum(!is.na(x))

dat_2015 <- dat %>% filter(birth_year==2015)

temp <- dat_2015 %>% 
  filter(!is.na(gest_weeks2),
         !is.na(gest_weeks),
         !is.na(brthwt_g)) %>% 
  dplyr::select(gest_weeks,gest_weeks2,brthwt_g,birth_state_code,birth_micro_code) %>% 
  group_by(birth_micro_code) %>% 
  summarize(n=n(),
            PTBRATE=mean(gest_weeks<37),
            LMPRATE=mean(gest_weeks2<37),
            LBWRATE=mean(brthwt_g<2500)) %>% 
  filter(PTBRATE>0,n>1000)

ggplot(temp,aes(x=PTBRATE,y=LMPRATE)) + 
  geom_point() + theme_bw() + 
  geom_abline(intercept=c(-0.01,0,0.01),slope=1)

temp2 <- dat_2015 %>% 
  filter(!is.na(gest_weeks),
         n_prenat_visit>4) %>%
  dplyr::select(gest_weeks,gest_weeks2,brthwt_g,birth_state_code,birth_micro_code) %>% 
  mutate(HAVELMP = factor(is.na(gest_weeks2),labels=c("LMP","NOLMP"))) %>%
  group_by(birth_micro_code,HAVELMP) %>% 
  summarize(n=n(),
            PTBRATE=mean(gest_weeks<37))

temp3 <- temp2 %>% filter(birth_micro_code %in% 
                  (temp2 %>% 
                     group_by(birth_micro_code) %>% 
                     summarize(nmin=min(n)) %>% 
                     filter(nmin>1000) %>% 
                     dplyr::select(birth_micro_code) %>% 
                     unlist() %>% as.vector())) %>% 
  dplyr::select(-n) %>% 
  spread(key=HAVELMP,value=PTBRATE) 

ggplot(temp3,aes(x=NOLMP,y=LMP)) + 
  geom_point() + theme_bw() + 
  geom_abline(intercept=c(-0.01,0,0.01),slope=1)


temp5 <- dat_2015 %>% 
  filter(!is.na(gest_weeks),is.na(gest_weeks2)) %>%
  dplyr::select(gest_weeks,gest_weeks2,brthwt_g,birth_state_code,birth_micro_code,gest_method) %>% 
  group_by(birth_micro_code,gest_method) %>% 
  summarize(n=n(),
            PTBRATE=mean(gest_weeks<37))

temp6 <- temp5 %>% filter(birth_micro_code %in% 
                            (temp5 %>% 
                               group_by(birth_micro_code) %>% 
                               summarize(nmin=min(n)) %>% 
                               filter(nmin>1000) %>% 
                               dplyr::select(birth_micro_code) %>% 
                               unlist() %>% as.vector()))   %>%
  dplyr::select(-n) %>% 
  spread(key=gest_method,value=PTBRATE) 

names(temp6) <- c("birth_micro_code","PE","AM","UK")


dat_2015_usd <- dat_2015 %>% filter(gest_method=="Another method",
                                    !is.na(gest_weeks),
                                    is.na(gest_weeks2),
                                    !is.na(brthwt_g),
                                    gest_month_precare<=4, # reliable USD
                                    n_prenat_visit_cat=="7 and over") 
                                  

dat_2015_usd %>% group_by(birth_state_code) %>% 
  summarize(n=n(),PTBRATE=mean(gest_weeks<37),LBWRATE=mean(brthwt_g<2500)) %>% 
  filter(n>=1000) %>% arrange(PTBRATE) %>% print(n=100) %>% ggplot(aes(x=PTBRATE,y=LBWRATE)) + geom_point() + theme_bw() + geom_smooth(method=rlm) + geom_abline(intercept=c(0,-0.01,0.01),slope=1,col=2)

states_to_use <- dat_2015_usd %>% group_by(birth_state_code) %>% 
  summarize(n=n(),PTBRATE=mean(gest_weeks<37),LBWRATE=mean(brthwt_g<2500)) %>% 
  filter(n>=1000) %>% dplyr::select(birth_state_code) %>% unlist() %>% as.character()

dat_2015_usd_subset <- dat_2015_usd %>% 
  filter(birth_state_code %in% states_to_use)  %>% 
  rename(state_code=birth_state_code)

stfit <- function (x, alphastart=-1,nustart=5, ...) 
{
  xbar = mean(x)
  s = sd(x)
  phat0<-fitdistr(x,densfun=dst,
           start=list(xi=mean(x),omega=sd(x),alpha=alphastart,nu=nustart))$estimate
  tabx <- table(x)
  value = as.numeric(labels(tabx)[[1]])
  freq = as.numeric(tabx)
  p = phat0
  loglik = function(x,value=value, freq=freq) {
    f = -sum(freq*log(pst(value+1, x[1], x[2], x[3], x[4])-
                        pst(value, x[1], x[2], x[3], x[4])))
    f
  }
  fit = nlm(f = loglik, p = p, value=value,freq=freq,hessian=TRUE)
  Names = c("xi", "omega", "alpha", "nu")
  names(fit$estimate) = Names
  names(fit$gradient) = Names
  fit
}




state_split <- split(dat_2015_usd_subset$gest_weeks, 
                     dat_2015_usd_subset$state_code)

state_fits <- lapply(state_split,stfit)

#save(state_fits,file="20190104.Rdata")

load("20190104.Rdata")
parameters<-do.call(rbind,lapply(state_fits,function(x)return(x$estimate)))

dat_2015_usd_subset %>% 
  group_by(birth_state_code) %>% 
  summarize(PTBRATE=mean(gest_weeks<37)) %>% 
  cbind(PTBEST=apply(parameters,1,function(x)pst(37,x[1],x[2],x[3],x[4]))) %>%
  ggplot(aes(x=PTBRATE,y=PTBEST)) + geom_point()+theme_bw()+
  geom_abline(slope=1,intercept=c(-0.01,0,0.01),col=2) +
  geom_smooth(method=rlm)

load("br_state_codes.rda")

br_state_codes

dat_2015_usd_subset <- dat_2015_usd_subset %>%
  left_join(br_state_codes,by="state_code") 

subset_rates <- dat_2015_usd_subset %>% 
  group_by(state_code) %>% 
  summarize(PTBRATE=mean(gest_weeks<37)) %>% 
  cbind(PTBEST=apply(parameters,1,function(x)pst(37,x[1],x[2],x[3],x[4]))) 

subset_results <- cbind(subset_rates,parameters) %>% left_join(br_state_codes, by="state_code")




df_density <- lapply(1:nrow(subset_results),function(i)
  data.frame(state_name=subset_results$state_name[i],
             gest_weeks=seq(18.5,45.5,0.1),
             density=dst(seq(18.5,45.5,0.1),
                         subset_results$xi[i],
                         subset_results$omega[i],
                         subset_results$alpha[i],
                         subset_results$nu[i]))
)

df_density <- do.call(rbind,df_density)


ggplot(dat_2015_usd_subset,aes(x=gest_weeks,y=..density..,facet=state_name))+
  geom_histogram(breaks=seq(18.99,45,1),col="black",fill="white")+
  theme_bw()+facet_wrap(~state_name,ncol=4) +
  geom_line(data=df_density, 
            aes(x=gest_weeks,y=density),col="red") + xlim(28,43) +
  labs(x="Gestational Age (weeks)")

partab<-do.call(rbind,lapply(1:length(state_fits), function(i){
  temp<-data.frame(se=sqrt(abs(diag(solve(state_fits[[i]]$hessian)))),
                   estimate=state_fits[[i]]$estimate) 
  temp %>% mutate(parameter=factor(row.names(temp),levels=c("xi","omega","alpha","nu")), 
                  state_code=names(state_fits)[i])
}) ) %>%  
  left_join(br_state_codes,by="state_code") 



ggplot(partab,
       aes(y=state_name,
           x=estimate,
           xmin=estimate-qnorm(0.975)*se,col=region_code,
           xmax=estimate+qnorm(0.975)*se,facet=parameter))+
  geom_errorbarh()+
  facet_wrap(~parameter,ncol=2,scale="free_x")+
  geom_point()+theme_bw()

stfit_bw <- function (x, ...) 
{
  inits<-fitdistr(x,densfun="t")$estimate
  inits<- as.numeric(unlist(inits))
  fit<-fitdistr(x,densfun=dst,
                  start=list(xi=inits[1],omega=inits[2],alpha=0,nu=inits[3]))
}


state_split_bw <- split(dat_2015_usd_subset$brthwt_g, 
                     dat_2015_usd_subset$state_code)

state_fits_bw <- lapply(state_split_bw,stfit_bw)


# save.image("20190107.Rdata")

partab_bw<-do.call(rbind,lapply(1:length(state_fits), function(i){
  temp<-data.frame(estimate=state_fits_bw[[i]]$estimate,
                   confint(state_fits_bw[[i]]))
  temp %>% mutate(parameter=factor(row.names(temp),levels=c("xi","omega","alpha","nu")), 
                  state_code=names(state_fits_bw)[i]) %>%
    rename(LCL=X2.5.., UCL=X97.5..)
}) ) %>%  
  left_join(br_state_codes,by="state_code") 


ggplot(partab_bw,
       aes(y=state_name,
           x=estimate,
           xmin=LCL,
           xmax=UCL))+
  geom_errorbarh()+
  facet_wrap(~parameter,ncol=2,scale="free_x")+
  geom_point()+theme_bw()


left_join(partab %>% dplyr::select(state_name,parameter,estimate)  %>% spread(parameter,estimate),
          partab_bw %>% dplyr::select(state_name,parameter,estimate) %>% spread(parameter,estimate) %>%
            rename(xi_bw=xi, omega_bw=omega, alpha_bw=alpha, nu_bw=nu),
          by="state_name") -> bigpartab

xi.bs<-regsubsets(xi~xi_bw+omega_bw+alpha_bw+nu_bw,data=bigpartab)  # xi_bw alpha_bw
omega.bs<-regsubsets(omega~xi_bw+omega_bw+alpha_bw+nu_bw,data=bigpartab)  # xi_bw nu_bw
alpha.bs<-regsubsets(alpha~xi_bw+omega_bw+alpha_bw+nu_bw,data=bigpartab) # xi_bw alpha_bw
nu.bs<-regsubsets(nu~xi_bw+omega_bw+alpha_bw+nu_bw,data=bigpartab)  # xi_bw nu_bw

lm.xi<-lm(xi~xi_bw+alpha_bw,data=bigpartab)  # xi_bw alpha_bw
lm.omega<-lm(omega~xi_bw+nu_bw,data=bigpartab)  # xi_bw nu_bw
lm.alpha<-lm(alpha~xi_bw+alpha_bw,data=bigpartab) # xi_bw alpha_bw
lm.nu<-lm(nu~xi_bw+nu_bw,data=bigpartab)  # xi_bw nu_bw


dat_2015_usd_subset %>% 
  group_by(birth_state_code) %>% 
  summarize(PTBRATE=mean(gest_weeks<37)) %>% 
  cbind(PTBEST=apply(parameters,1,function(x)pst(37,x[1],x[2],x[3],x[4]))) %>%
  ggplot(aes(x=PTBRATE,y=PTBEST)) + geom_point()+theme_bw()+
  geom_abline(slope=1,intercept=c(-0.01,0,0.01),col=2) +
  geom_smooth(method=rlm)


           
dat_2015_usd_subset %>% 
  group_by(state_code) %>% 
  summarize(PTBRATE=mean(gest_weeks<37)) %>% 
  cbind(PTBEST=data.frame(xi=lm.xi$fitted.values,
                          omega=lm.omega$fitted.values,
                          alpha=lm.alpha$fitted.values,
                          nu=lm.nu$fitted.values) %>% 
          apply(1,function(x)pst(37,x[1],x[2],x[3],x[4]))) %>%
  ggplot(aes(x=PTBRATE,y=PTBEST)) + geom_point()+theme_bw()+
  geom_abline(slope=1,intercept=c(-0.01,0,0.01),col=2) +
  geom_smooth(method=rlm)


mvlm1 <- manova(cbind(xi,omega,alpha,nu)~xi_bw+omega_bw+alpha_bw+nu_bw,data=bigpartab)

xdata<-data.frame(Intercept=1,bigpartab %>% dplyr::select(xi_bw,omega_bw,alpha_bw,nu_bw))
temp<-lapply(1:1000,function(i)as.matrix(xdata) %*% matrix(mvrnorm(n=1,mu=coef(mvlm1) %>% as.vector(),Sigma=vcov(mvlm1)),ncol=4) %>% 
         apply(1,function(x)pst(37,x[1],x[2],x[3],x[4])))

#save.image("20190108.Rdata")
df.sim <- do.call(rbind,temp) %>% apply(2,function(x)quantile(x,c(0.025,0.975))) %>% t() %>% as.data.frame()
names(df.sim) <- c("LCL","UCL")
subset_rates <- dat_2015_usd_subset %>% 
  group_by(state_code) %>% 
  summarize(PTBRATE=mean(gest_weeks<37)) %>% cbind(df.sim) %>%
  mutate(PAREST = partab %>% dplyr::select(-se) %>% spread(parameter,estimate) %>% dplyr::select(xi,omega,alpha,nu) %>% apply(1,function(x)pst(37,x[1],x[2],x[3],x[4])),
         LBWRATE = dat_2015_usd_subset %>% dplyr::select(state_code,brthwt_g) %>% group_by(state_code) %>% summarize(LBWRATE=mean(brthwt_g<2500,na.rm=TRUE)) %>% dplyr::select(LBWRATE) %>% unlist() %>% as.numeric()
         )


ggplot(subset_rates,aes(y=PTBRATE,ymin=LCL,ymax=UCL,x=state_code))+geom_errorbar()+geom_point()+geom_point(aes(y=PAREST),col=2)+geom_point(aes(y=LBWRATE),col=4)

df.sims2 <- data.frame(do.call(rbind,temp))
names(df.sims2) <- bigpartab$state_name

