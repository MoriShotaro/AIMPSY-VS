
###########################################################################
# setting
###########################################################################

# library -----------------------------------------------------------------

# print R version
version

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(gdxrrw)
library(ggsci)
library(viridis)
library(patchwork)
library(foreach)
library(sf)
library(Cairo)
library(lemon)
library(imputeTS)

# setting -----------------------------------------------------------------

cdir <- getwd()
ddir <- paste0(cdir,'/data/2402011936')  # output path
pdir <- paste0(cdir,'/prog/inc_prog')
odir <- paste0(cdir,'/output/Chapter_3')
basedir <- 'AIMPSY-master'
reg_in <- 'NEA'
plot_year <- '2050'
time_resol <- '3HOUR'
gams_sys_dir <- 'C:/GAMS/38'

# get gams system directory
igdx(gams_sys_dir)

Sys.setlocale("LC_TIME", "en_US.UTF-8")

# define geometry
geometry_NEA <- st_read(paste0(ddir,'/define/NEA_wo.geojson')) %>% 
  rename(Sr=N_PSY)
geometry_NEA_w <- st_read(paste0(ddir,'/define/NEA_w.geojson')) %>% 
  transmute(Sr=N_PSY,geometry)


# import data -------------------------------------------------------------

# import scenario matrix
scen_mat_load <- read_csv(paste0(ddir,'/main/scenario_table.csv')) %>% 
  filter(Scenario%in%c('NoPOL','500C','500C_Elc','500C_dmd+_CCS','500C_dmd+_CCS_Elc'))

# extract infeasible scenario
scen_inf <- scen_mat_load %>% filter(Status=='Infeasible')
scen_mat <- scen_mat_load %>% filter(Status!='Infeasible') %>% 
  mutate(Scenario=recode(Scenario,
                         '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                         '500C_dmd+_CCS'='demand+',
                         '500C_Elc'='default w/ grid',
                         '500C'='default'),
         scen_dpol=recode(scen_dpol,
                          '500C_DEC_CCS_Dom'='demand+',
                          '500C'='default'),
         scen_tpol=recode(scen_tpol,
                          'NEAG'='w/ grid',
                          'default'='w/o grid'))

# import AIM/PSY output
df_all_hr <- rgdx.param(paste0(ddir,'/main/hr/',plot_year,'.gdx'),'data_out_hr') %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('default','default w/ grid','demand+','demand+ w/ grid')) %>% 
  filter(Sc%in%scen_mat$Scenario) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>%
  rename(value=data_out_hr)

df_all_yr <- rgdx.param(paste0(ddir,'/main/merged_output.gdx'),'data_all_yr') %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('default','default w/ grid','demand+','demand+ w/ grid','NoPOL')) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>%
  mutate(Sy=as.numeric(Sy)) %>% 
  rename(value=data_all_yr) %>% 
  mutate(Y5=ceiling(Sy-2001)%/%5*5+2005) %>% 
  group_by(Sc,Sv,Sr,Y5) %>% 
  reframe(value=mean(value)) %>%
  replace_na(list(value=0)) %>% 
  pivot_wider(names_from=Y5,values_from=value,values_fill=0) %>%
  pivot_longer(cols=-c(Sc,Sv,Sr),names_to='Y5',values_to='value',names_transform=as.numeric)

# scenario category
scen_cat <- scen_mat %>% 
  transmute(Sc=Scenario,cat1=scen_dpol,cat2=scen_tpol)

# timeslices
T <- rgdx.set(paste0(ddir,'/main/hr/2050.gdx'),'St')


# load modules ------------------------------------------------------------

# import variable definition
source(paste0(ddir,'/define/var.R'))

# import plot function
source(paste0(pdir,'/func_analysis.R'))

# input summary -----------------------------------------------------------

dmda_t <- foreach (i=c('500C','500C_Elc','500C_dmd+_CCS','500C_dmd+_CCS_Elc','NoPOL')) %do% {
  dmda_t <- rgdx.param(paste0(ddir,'/input/',i,'.gdx'),'dmda_t') %>% 
    mutate(Sc=i)
} %>% bind_rows() %>% 
  mutate(Sr=case_when(str_detect(N,'JP-')~'JPN',
                      str_detect(N,'CN-')~'CHN',
                      str_detect(N,'KR')~'KOR',
                      str_detect(N,'MN')~'MNG')) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Y=as.numeric(Y))

g_3_1 <- dmda_t %>% 
  filter(I=='AC2') %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  group_by(Sr,I,Y,Sc) %>%
  reframe(value=sum(dmda_t)) %>% 
  filter(Sc%in%c('NoPOL','default','demand+')) %>% 
  mutate(Sr=factor(Sr,levels=c('JPN','CHN','KOR','MNG'))) %>% 
  ggplot() +
  geom_line(aes(x=Y,y=value/10^6,color=Sc)) +
  geom_point(aes(x=Y,y=value/10^6,color=Sc)) +
  scale_color_d3() +
  scale_y_continuous(limits=c(0,NA)) +
  facet_wrap(vars(Sr),scales='free',nrow=1) +
  labs(x='',y='electricity demand (TWh/yr)') +
  plot_theme +
  theme(legend.position='right')
ggsave(filename=paste0(odir,'/Fig_3_1.png'),g_3_1,width=7,height=3)

g_3_2 <- dmda_t %>% 
  filter(I=='H2') %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  group_by(Sr,I,Sc) %>% 
  complete(Y=seq(2020,2050,5),fill=list(dmda_t=0)) %>% 
  group_by(Sr,I,Y,Sc) %>%
  reframe(value=sum(dmda_t)) %>% 
  filter(Sc%in%c('NoPOL','default','demand+')) %>% 
  mutate(Sr=factor(Sr,levels=c('JPN','CHN','KOR','MNG'))) %>% 
  ggplot() +
  geom_line(aes(x=Y,y=value/10^6,color=Sc)) +
  geom_point(aes(x=Y,y=value/10^6,color=Sc)) +
  scale_color_d3() +
  scale_y_continuous(limits=c(0,NA)) +
  facet_wrap(vars(Sr),scales='free',nrow=1) +
  labs(x='',y='domestic green hydrogen production\n(TWh/yr)') +
  plot_theme +
  theme(legend.position='right')
ggsave(filename=paste0(odir,'/Fig_3_2.png'),g_3_2,width=7,height=3)

qmax_t <- foreach (i=c('500C','500C_Elc','500C_dmd+_CCS','500C_dmd+_CCS_Elc','NoPOL')) %do% {
  qmax_t <- rgdx.param(paste0(ddir,'/input/',i,'.gdx'),'qmax_t') %>% 
    mutate(Sc=i)
} %>% bind_rows() %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Y=as.numeric(Y))

g_3_3 <- qmax_t %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('default')) %>% 
  mutate(MQ=recode(MQ,'MNG'='MNG','KOR'='KOR')) %>% 
  mutate(MQ=factor(MQ,levels=c('JPN','CHN','KOR','MNG'))) %>% 
  ggplot() +
  geom_line(aes(x=Y,y=qmax_t/10^6)) +
  scale_color_d3() +
  scale_y_continuous(limits=c(0,NA)) +
  facet_wrap(vars(MQ),scales='free',nrow=1) +
  labs(x='',y=expression(paste('CO'[2],' emission (MtCO'[2],'/yr)'))) +
  plot_theme +
  theme(legend.position='right')
ggsave(filename=paste0(odir,'/Fig_3_3.png'),g_3_3,width=7,height=2.5)

g_s8_1 <- dmda_t %>% 
  filter(I=='AC2',Sr=='JPN') %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  group_by(I,Y,Sc) %>%
  reframe(value=sum(dmda_t)) %>% 
  filter(Sc%in%c('NoPOL','default','demand+')) %>% 
  ggplot() +
  geom_line(aes(x=Y,y=value/10^6,color=Sc)) +
  geom_point(aes(x=Y,y=value/10^6,color=Sc)) +
  scale_color_d3() +
  scale_y_continuous(limits=c(0,NA)) +
  labs(x='',y='electricity demand (TWh/yr)') +
  plot_theme +
  theme(legend.position=c(0.7,0.2))

g_s8_2 <- dmda_t %>% 
  filter(I=='H2',Sr=='JPN') %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  group_by(Sr,I,Sc) %>% 
  complete(Y=seq(2020,2050,5),fill=list(dmda_t=0)) %>% 
  group_by(I,Y,Sc) %>%
  reframe(value=sum(dmda_t)) %>% 
  filter(Sc%in%c('NoPOL','default','demand+')) %>% 
  ggplot() +
  geom_line(aes(x=Y,y=value/10^6,color=Sc)) +
  geom_point(aes(x=Y,y=value/10^6,color=Sc)) +
  scale_color_d3() +
  scale_y_continuous(limits=c(0,NA)) +
  labs(x='',y='domestic green hydrogen production\n(TWh/yr)') +
  plot_theme +
  theme(legend.position=c(0.3,0.8))

g_s8_3 <- qmax_t %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('default'),MQ=='JPN') %>% 
  mutate(MQ=recode(MQ,'MNG'='MNG','KOR'='KOR')) %>% 
  group_by(Y,Sc) %>% 
  reframe(qmax_t=sum(qmax_t)) %>% 
  ggplot() +
  geom_line(aes(x=Y,y=qmax_t/10^9)) +
  scale_color_d3() +
  scale_y_continuous(limits=c(0,NA)) +
  labs(x='',y=expression(paste('CO'[2],' emission (GtCO'[2],'/yr)'))) +
  plot_theme +
  theme(legend.position='right')

g_s8 <- g_s8_1 + g_s8_2 + g_s8_3

ggsave(filename=paste0(odir,'/../Presentation/Fig_S8.png'),g_s8,
       width=8,height=3.5)

