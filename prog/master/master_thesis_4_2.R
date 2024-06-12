
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

date <- '2403191936'
cdir <- getwd()
ddir <- paste0(cdir,'/data/',date)  # output path
pdir <- paste0(cdir,'/prog/inc_prog')
odir <- paste0(cdir,'/output/',date,'/Chapter_4_2')
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


###########################################################################
# plot
###########################################################################

# generation mix ----------------------------------------------------------

plt <- set_plot(eleyr)

df_yr_ele <- df_all_yr %>% 
  filter(Sv%in%eleyr$Variable) %>%
  mutate(Sv=recode(Sv,'Sec_Ene_Ele_Sto_Dis_PHS'='Sec_Ene_Ele_Hyd')) %>% 
  group_by(Sc,Sv,Sr,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=scen_mat$Scenario)) %>%
  mutate(cat1=scen_mat$scen_dpol[Sc], cat2=scen_mat$scen_tpol[Sc]) %>%
  mutate(cat1=factor(cat1, levels=unique(scen_mat$scen_dpol))) %>%
  mutate(cat2=factor(cat2, levels=unique(scen_mat$scen_tpol)))

g_4_8 <- df_yr_ele %>%
  filter(Sr%in%c('JPN','CHN','KR','MN','NEA'),Sc%in%c('NoPOL','default','default w/ grid','demand+','demand+ w/ grid')) %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value),
         Sr=recode(Sr,'KR'='KOR','MN'='MNG')) %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         Sc=factor(Sc,levels=c('NoPOL','default','default w/ grid','demand+','demand+ w/ grid')),
         Sr=factor(Sr,levels=c('JPN','CHN','KOR','MNG','NEA'))) %>%
  ggplot() +
  geom_area(aes(x=Y5,y=value/10^6,fill=Sv)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  labs(x='',y='power generation (TWh/yr)') +
  facet_grid(cols=vars(Sc),rows=vars(Sr),scales='free_y') +
  plot_theme +
  theme(legend.position='bottom')

ggsave(filename=paste0(odir,'/Fig_4_8.png'),g_4_8,
       width=8,height=11)

g_4_9 <- df_yr_ele %>%
  filter(Sr%in%c('JPN','CHN','KR','MN','NEA'),
         Sc%in%c('default','default w/ grid','demand+','demand+ w/ grid'),
         Y5==2050) %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value),
         Sr=recode(Sr,'KR'='KOR','MN'='MNG')) %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         Sc=factor(Sc,levels=c('default','default w/ grid','demand+','demand+ w/ grid')),
         Sr=factor(Sr,levels=c('JPN','CHN','KOR','MNG','NEA'))) %>%
  ggplot() +
  geom_bar(aes(x=Sc,y=value/10^6,fill=Sv),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  labs(x='',y='power generation (TWh/yr)') +
  facet_wrap(vars(Sr),nrow=1,scales='free_y') +
  plot_theme +
  theme(legend.position='bottom')

ggsave(filename=paste0(odir,'/Fig_4_9.png'),g_4_9,
       width=7.5,height=5.5)

g_4_13 <- df_yr_ele %>%
  filter(str_detect(Sr,'JP-'),
         Sc%in%c('default','default w/ grid','demand+','demand+ w/ grid'),
         Y5==2050) %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value)) %>%
  select(-Sc) %>% 
  pivot_wider(names_from=cat2,values_from=value,values_fill=0) %>% 
  transmute(cat1,Sv,Sr,Y5,value=`w/ grid`-`w/o grid`) %>% 
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         cat1=factor(cat1,levels=c('default','demand+')),
         Sr=factor(Sr,levels=c('JP-HD','JP-TH','JP-KT','JP-CB','JP-HR','JP-KS','JP-CG','JP-SK','JP-KY'))) %>%
  ggplot() +
  geom_bar(aes(x=cat1,y=value/10^6,fill=Sv),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  labs(x='',y='power generation change due to Asia grid (TWh/yr)') +
  facet_wrap(vars(Sr),nrow=1) +
  plot_theme +
  theme(legend.position='right')
plot(g_4_13)

ggsave(filename=paste0(odir,'/Fig_4_13.png'),g_4_13,
       width=8,height=5)


# Interconnection capacity ------------------------------------------------

df_lnkcap <- foreach (i=scen_mat_load$Scenario) %do% {
  tmp <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_lnk_cap') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>%
    mutate(YEAR=as.numeric(YEAR)) %>% 
    filter(str_detect(PW_LNK,'.*-AC_.*-AC$')) %>% 
    mutate(Sc=i) %>% 
    mutate(Y5=ceiling(YEAR-2001)%/%5*5+2005) %>% 
    group_by(Sc,PW_LNK,Y5) %>% 
    reframe(value=mean(prm2sec_lnk_cap))
} %>% bind_rows()

df_lnkcap_agg <- df_lnkcap %>%
  separate(PW_LNK,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
  mutate(Sv=case_when(str_detect(N0,'CN-')&str_detect(N1,'CN-')~'China domestic',
                      str_detect(N0,'JP-')&str_detect(N1,'JP-')~'Japan domestic',
                      str_detect(N0,'CN-')&str_detect(N1,'MN-')~'China/Mongolia',
                      str_detect(N0,'CN-')&str_detect(N1,'KR-')~'China/Korea',
                      str_detect(N0,'KR-')&str_detect(N1,'JP-')~'Korea/Japan')) %>% 
  group_by(Sc,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sv=factor(Sv,levels=c('China/Mongolia','China/Korea','Korea/Japan','China domestic','Japan domestic'))) %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('default','default w/ grid','demand+','demand+ w/ grid')) %>% 
  left_join(scen_cat)

g_4_11 <- df_lnkcap_agg %>%
  ggplot() +
  geom_bar(aes(x=Y5,y=value/10^6,fill=Sv),stat='identity') +
  facet_wrap(vars(Sc),nrow=1) +
  scale_fill_manual(values=c('China domestic'='#5ea6f2',
                             'Japan domestic'='#1a66b8',
                             'China/Mongolia'='#fcca3f',
                             'China/Korea'='#e6a73c',
                             'Korea/Japan'='#b85e1a')) +
  labs(x='',y='interconnection capacity (TW)') +
  plot_theme
plot(g_4_11)

ggsave(filename=paste0(odir,'/Fig_4_11.png'),g_4_11,
       width=8,height=3.5)


# capacity ----------------------------------------------------------------

gen_map <- tribble(~SE0,~PW_GEN,~Sv,
                   'ALL','PVC','Cap_Ele_Solar_Cen',
                   'ALL','PVD','Cap_Ele_Solar_Dec',
                   'ALL','WON','Cap_Ele_Win_Ons',
                   'ALL','WOF','Cap_Ele_Win_Off',
                   'ALL','HYD','Cap_Ele_Hyd',
                   'ALL','GEO','Cap_Ele_Geo',
                   'ALL','BMS','Cap_Ele_Bio_wo_CCS',
                   'ALL','BMS_CCS','Cap_Ele_Bio_w_CCS',
                   'ALL','COL','Cap_Ele_Coa_wo_CCS',
                   'ALL','COL_CCS','Cap_Ele_Coa_w_CCS',
                   'ALL','GAS','Cap_Ele_Gas_wo_CCS',
                   'ALL','GAS_CCS','Cap_Ele_Gas_w_CCS',
                   'ALL','OIL','Cap_Ele_Oil',
                   'ALL','NUC','Cap_Ele_Nuc')

df_gencap <- foreach (i=scen_mat_load$Scenario) %do% {
  tmp <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_gen_cap') %>% 
    mutate(Sc=i)
} %>% bind_rows() %>% 
  full_join(gen_map) %>% 
  drop_na(Sv,YEAR) %>% 
  transmute(Sc,Sr=N,Sv,Sy=YEAR,value=prm2sec_gen_cap)

df_lnkcap2 <- foreach (i=scen_mat_load$Scenario) %do% {
  tmp <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_lnk_cap') %>% 
    mutate(Sc=i)
} %>% bind_rows() %>% 
  filter(!str_detect(PW_LNK,'.*-AC_.*-AC$')) %>% 
  separate(PW_LNK,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
  filter(N0==N1) %>% 
  mutate(Sr=case_when(str_detect(N0,'JP-HD')&str_detect(N1,'JP-HD')~'JP-HD',
                      str_detect(N0,'JP-TH')&str_detect(N1,'JP-TH')~'JP-TH',
                      str_detect(N0,'JP-KT')&str_detect(N1,'JP-KT')~'JP-KT',
                      str_detect(N0,'JP-CB')&str_detect(N1,'JP-CB')~'JP-CB',
                      str_detect(N0,'JP-HR')&str_detect(N1,'JP-HR')~'JP-HR',
                      str_detect(N0,'JP-KS')&str_detect(N1,'JP-KS')~'JP-KS',
                      str_detect(N0,'JP-CG')&str_detect(N1,'JP-CG')~'JP-CG',
                      str_detect(N0,'JP-SK')&str_detect(N1,'JP-SK')~'JP-SK',
                      str_detect(N0,'JP-KY')&str_detect(N1,'JP-KY')~'JP-KY'),
         Sv=case_when(str_detect(N0,'-AC$')&str_detect(N1,'-DC$')~'Cap_Ele_Sto_LiB_Pow',
                      str_detect(N0,'-AC$')&str_detect(N1,'-H2$')~'Cap_Hyd_Ele',
                      str_detect(N0,'-AC$')&str_detect(N1,'-PHS$')~'Cap_Ele_Sto_PHS_Pow',
                      str_detect(N0,'-H2-GT$')&str_detect(N1,'-AC$')~'Cap_Ele_H2_GT',
                      str_detect(N0,'-H2-FC$')&str_detect(N1,'-AC$')~'Cap_Ele_H2_FC',
                      str_detect(N0,'-H2$')&str_detect(N1,'-H2S$')~'Cap_Hyd_Sto_H2S_Pow',
                      str_detect(N0,'-AC$')&str_detect(N1,'-AC2$')~'Cap_Ele_Dis')) %>% 
  transmute(Sc,Sr,Sv,Sy=YEAR,value=prm2sec_lnk_cap)

sto_map <- tribble(~SE0,~PW_STO,~Variable,
                   'ALL','LIBE','Cap_Ele_Sto_LiB_Ene',
                   'ALL','PHSE','Cap_Ele_Sto_PHS_Ene',
                   'ALL','H2SE','Cap_Ele_Sto_H2S_Ene')

df_stocap <- foreach (i=scen_mat_load$Scenario) %do% {
  tmp <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_sto_cap') %>% 
    mutate(Sc=i)
} %>% bind_rows() %>% 
  full_join(sto_map) %>% 
  drop_na(Variable) %>% 
  transmute(Sc,Sr=N,Sv=Variable,Sy=YEAR,value=prm2sec_sto_cap)

df_techcap <- bind_rows(df_gencap,df_lnkcap2,df_stocap) %>% 
  mutate(Sy=as.numeric(as.character(Sy))) %>% 
  arrange(Sy) %>% 
  mutate(Sr2=case_when(str_detect(Sr,'JP-')~'JPN',
                       str_detect(Sr,'CN-')~'CHN',
                       str_detect(Sr,'KR')~'KOR',
                       str_detect(Sr,'MN')~'MNG')) %>% 
  mutate(Y5=ceiling(Sy-2001)%/%5*5+2005) %>% 
  group_by(Sc,Sr,Sr2,Sv,Y5) %>% 
  reframe(value=mean(value)) %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('default','default w/ grid','demand+','demand+ w/ grid'))

g_4_12 <- df_techcap %>% 
  filter(Sv%in%c('Cap_Ele_Sto_LiB_Ene','Cap_Ele_Sto_PHS_Ene'),Sc%in%c('default','default w/ grid','demand+','demand+ w/ grid'),
         Y5==2050) %>% 
  group_by(Sc,Sr2,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=c('default','default w/ grid','demand+','demand+ w/ grid')),
         Sr2=factor(Sr2,levels=c('JPN','CHN','KOR','MNG'))) %>% 
  mutate(Sv=recode(Sv,'Cap_Ele_Sto_LiB_Ene'='Li-ion battery','Cap_Ele_Sto_PHS_Ene'='pumped hydro')) %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value/10^6,fill=Sv),stat='identity') +
  labs(x='',y='energy storage energy capacity (TWh)') +
  scale_fill_manual(values=c('#32AC9AFF','#7fffd4')) +
  facet_wrap(vars(Sr2),nrow=1,scales='free') +
  plot_theme +
  theme(legend.position = 'right')

ggsave(filename=paste0(odir,'/Fig_4_12.png'),g_4_12,
       width=8,height=3.5)


# import and export -------------------------------------------------------

Sys.setlocale("LC_TIME", "en_GB.UTF-8")

df_flw_exp <- foreach (i=c('500C_Elc','500C_dmd+_CCS_Elc')) %do% {
  tmp0 <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_flw_snd') %>%
    filter(YEAR==plot_year) %>% 
    mutate(across(where(is.factor),~as.character(.))) %>%
    select(-I0,-I1) %>% 
    rename(St=T,Sy=YEAR) %>% 
    filter(Sy==plot_year,
           L%in%c('CN-NE-AC_MN-AC','CN-NO-AC_MN-AC','CN-NO-AC_KR-AC',
                  'CN-NW-AC_MN-AC','KR-AC_JP-KY-AC','KR-AC_JP-CG-AC')) %>% 
    filter(N0==str_match(L,'(.*?)-AC_(.*?)-AC')[,2])
  tmp1 <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_flw_snd') %>% 
    filter(YEAR==plot_year) %>% 
    mutate(across(where(is.factor),~as.character(.))) %>%
    select(-I0,-I1) %>% 
    rename(St=T,Sy=YEAR) %>% 
    filter(Sy==plot_year,
           L%in%c('CN-NE-AC_MN-AC','CN-NO-AC_MN-AC','CN-NO-AC_KR-AC',
                  'CN-NW-AC_MN-AC','KR-AC_JP-KY-AC','KR-AC_JP-CG-AC')) %>% 
    filter(N1==str_match(L,'(.*?)-AC_(.*?)-AC')[,2]) %>% 
    mutate(prm2sec_flw_snd=-prm2sec_flw_snd)
  tmp <- bind_rows(tmp0,tmp1) %>% 
    group_by(N0,N1,Sy,St) %>% 
    reframe(value=sum(prm2sec_flw_snd)) %>%
    transmute(Sc=i,N0,N1,Sy,St,value)
  summary <- bind_rows(tmp %>% filter(value>0),
                       tmp %>% filter(value<0) %>% transmute(Sc,tmp=N0,N0=N1,N1=tmp,Sy,St,value) %>% select(-tmp))
} %>% bind_rows() %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_Elc'='default w/ grid')) %>% 
  filter(Sc%in%c('default w/ grid','demand+ w/ grid')) 

df_flw_imp <- foreach (i=c('500C_Elc','500C_dmd+_CCS_Elc')) %do% {
  tmp0 <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_flw_rec') %>%
    filter(YEAR==plot_year) %>% 
    mutate(across(where(is.factor),~as.character(.))) %>%
    select(-I0,-I1) %>% 
    rename(St=T,Sy=YEAR) %>% 
    filter(Sy==plot_year,
           L%in%c('CN-NE-AC_MN-AC','CN-NO-AC_MN-AC','CN-NO-AC_KR-AC',
                  'CN-NW-AC_MN-AC','KR-AC_JP-KY-AC','KR-AC_JP-CG-AC')) %>% 
    filter(N0==str_match(L,'(.*?)-AC_(.*?)-AC')[,2])
  tmp1 <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_flw_rec') %>% 
    filter(YEAR==plot_year) %>% 
    mutate(across(where(is.factor),~as.character(.))) %>%
    select(-I0,-I1) %>% 
    rename(St=T,Sy=YEAR) %>% 
    filter(Sy==plot_year,
           L%in%c('CN-NE-AC_MN-AC','CN-NO-AC_MN-AC','CN-NO-AC_KR-AC',
                  'CN-NW-AC_MN-AC','KR-AC_JP-KY-AC','KR-AC_JP-CG-AC')) %>% 
    filter(N1==str_match(L,'(.*?)-AC_(.*?)-AC')[,2]) %>% 
    mutate(prm2sec_flw_rec=-prm2sec_flw_rec)
  tmp <- bind_rows(tmp0,tmp1) %>% 
    group_by(N0,N1,Sy,St) %>% 
    reframe(value=sum(prm2sec_flw_rec)) %>%
    transmute(Sc=i,N0,N1,Sy,St,value)
  summary <- bind_rows(tmp %>% filter(value>0),
                       tmp %>% filter(value<0) %>% transmute(Sc,tmp=N0,N0=N1,N1=tmp,Sy,St,value) %>% select(-tmp))
} %>% bind_rows() %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_Elc'='default w/ grid')) %>% 
  filter(Sc%in%c('default w/ grid','demand+ w/ grid')) 

df_flw_exp_annual <- bind_rows(df_flw_exp %>% filter(value>0),
                               df_flw_exp %>% filter(value<0) %>% transmute(Sc,tmp=N0,N0=N1,N1=tmp,Sy,St,value=-value) %>% select(-tmp))
df_flw_imp_annual <- bind_rows(df_flw_exp %>% filter(value>0),
                               df_flw_exp %>% filter(value<0) %>% transmute(Sc,tmp=N0,N0=N1,N1=tmp,Sy,St,value=-value) %>% select(-tmp))

df_exp_annual <- df_flw_exp_annual %>% 
  group_by(Sc,N0,Sy,St) %>% 
  reframe(value=sum(value)) %>% 
  transmute(Sc,N=N0,Sv='export',Sy,St,value)

df_imp_annual <- df_flw_imp_annual %>% 
  group_by(Sc,N1,Sy,St) %>% 
  reframe(value=sum(value)) %>% 
  transmute(Sc,N=N1,Sv='import',Sy,St,value)

df_flw_expimp_annual <- bind_rows(df_exp_annual,df_imp_annual) %>% 
  mutate(Sr=case_when(str_detect(N,'CN-')~'CHN',
                      str_detect(N,'JP-')~'JPN',
                      str_detect(N,'KR')~'KOR',
                      str_detect(N,'MN')~'MNG')) %>% 
  group_by(Sc,Sr,Sv,Sy,St) %>%
  reframe(value=sum(value)) %>% 
  pivot_wider(names_from=Sv,values_from=value,values_fill=0) %>% 
  mutate(value=import-export)

df_flw_annual <- bind_rows(df_flw_expimp_annual %>% filter(value>0) %>% transmute(Sc,Sr,Sv='import',Sy,St,value=value),
                           df_flw_expimp_annual %>% filter(value<0) %>% transmute(Sc,Sr,Sv='export',Sy,St,value=value)) %>% 
  group_by(Sc,Sr,Sv) %>% 
  reframe(value=sum(value))

df_flw_annual_net <- df_flw_annual %>% 
  pivot_wider(names_from=Sv,values_from=value) %>% 
  transmute(Sc,Sr,value=export+import)

g_4_10 <- df_flw_annual %>%
  mutate(Sr=factor(Sr,levels=c('JPN','CHN','KOR','MNG'))) %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value/10^6,fill=Sv),stat='identity') +
  geom_point(data=df_flw_annual_net %>% mutate(Sr=factor(Sr,levels=c('JPN','CHN','KOR','MNG'))),
             aes(x=Sc,y=value/10^6),fill='white',color='black',shape=23,size=1.5) +
  scale_fill_manual(values=c('#a3e0ffFF','#4fa1c9FF'))+
  labs(x='',y='electricity import and export (TWh/yr)') +
  facet_wrap(vars(Sr),nrow=1) +
  theme(legend.position='none',
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank()) +
  plot_theme +
  theme(legend.position = 'right')

ggsave(filename=paste0(odir,'/Fig_4_10.png'),g_4_10,
       width=7,height=4)

dmd <- foreach (i=c('500C_Elc','500C_dmd+_CCS_Elc')) %do% {
  tmp <- rgdx.param(paste0(ddir,'/gams_output/gdx_primary/',i,'/2050.gdx'),'dmd') %>% 
    filter(I%in%c('AC2','H2')) %>% 
    mutate(Sr=case_when(str_detect(N,'JP-')~'JPN',
                        str_detect(N,'CN-')~'CHN',
                        str_detect(N,'KR')~'KOR',
                        str_detect(N,'MN')~'MNG')) %>% 
    mutate(dmd=case_when(I=='H2'~dmd/0.68,
                         TRUE~dmd)) %>% 
    group_by(Sr) %>% reframe(dmd=sum(dmd)) %>% 
    mutate(Sc=i)
} %>% bind_rows() %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_Elc'='default w/ grid'))

g_s5 <- df_flw_annual %>% 
  pivot_wider(names_from=Sv,values_from=value) %>% 
  mutate(export=-export) %>% 
  mutate(Sr=recode(Sr,'CHN'='China','JPN'='Japan','KOR'='Korea','MNG'='Mongolia')) %>% 
  mutate(Sr=factor(Sr,levels=c('Japan','China','Korea','Mongolia'))) %>%
  ggplot() +
  geom_point(aes(x=import/10^6,y=export/10^6,color=Sr,shape=Sc),size=2) +
  geom_abline(slope=1,intercept=0,linetype='dotted',color='grey40',linewidth=0.3) +
  scale_color_d3() +
  scale_shape_manual(values=c(20,24)) +
  coord_cartesian(xlim = c(0, 1100), ylim = c(0, 1100)) +
  labs(x='electricity import (TWh/yr)',y='electricity export (TWh/yr)') +
  plot_theme
plot(g_s5)
ggsave(filename=paste0(odir,'/../Presentation/Fig_S5.png'),g_s5,
       width=5,height=3.5)

df_flw_heat <- df_flw_exp %>% 
  mutate(Sv=case_when(str_detect(N0,'CN-')&str_detect(N1,'MN')~'China/Mongolia',
                      str_detect(N0,'CN-')&str_detect(N1,'KR')~'China/Korea',
                      str_detect(N0,'KR')&str_detect(N1,'JP-')~'Korea/Japan')) %>% 
  group_by(Sc,Sv,St) %>% 
  reframe(value=sum(value)) %>% 
  complete(Sc,Sv,St=St,fill=list(value=0)) %>% 
  mutate(T=(as.numeric(str_sub(St,start=2,end=4))-1)*24+as.numeric(str_sub(St,start=6,end=7))+1,
         T=as.POSIXct(paste0(plot_year,'-01-01'))+(T+9-1)*3600,
         D=as.Date(T+9*3600),
         H=as.numeric(format(T, "%H")))

g_4_16 <- df_flw_heat %>% 
  filter(Sv=='Korea/Japan') %>% 
  ggplot() +
  geom_tile(aes(x=D,y=H,fill=value/1000)) +
  facet_wrap(vars(Sc)) +
  scale_fill_distiller(palette='Spectral',
                       limits=c(-max(abs(max(df_flw_heat %>% filter(Sv=='Korea/Japan') %>% select(value), na.rm = TRUE))/1000,
                                     abs(min(df_flw_heat %>% filter(Sv=='Korea/Japan') %>% select(value), na.rm = TRUE)))/1000,
                                max(abs(max(df_flw_heat %>% filter(Sv=='Korea/Japan') %>% select(value), na.rm = TRUE))/1000,
                                    abs(min(df_flw_heat %>% filter(Sv=='Korea/Japan') %>% select(value), na.rm = TRUE))/1000))) +
  labs(x='day of the year',y='hour of the day (UTC+9)',
       fill='power flow from Korea to Japan (GW)') +
  scale_x_date(labels=scales::date_format("%b %d")) +
  plot_theme +
  theme(axis.title.x = element_blank(),
        legend.title = element_text(size = 8.5)) +
  guides(fill=guide_colorbar(title.position='top',
                             barwidth = 15))
plot(g_4_16)

ggsave(filename=paste0(odir,'/Fig_4_16.png'),g_4_16 + theme(legend.position = 'bottom'),width=8,height=4)


# daily electricity balance -----------------------------------------------

plt <- set_plot(elehr)

# hourly electricity balance
df_hr_ele <- df_all_hr %>%
  filter(Sv%in%elehr$Variable) %>% 
  complete(Sc,Sv,Sr,St,fill=list(value=0)) %>%
  mutate(T=(as.numeric(str_sub(St,start=2,end=4))-1)*24+as.numeric(str_sub(St,start=6,end=7))+1+9) %>% 
  mutate(Sc=factor(Sc,levels=scen_mat$Scenario)) %>%
  mutate(cat1=scen_mat$scen_dpol[Sc], cat2=scen_mat$scen_tpol[Sc])

# daily electricity balance
df_da_ele <- df_hr_ele %>% 
  mutate(D=(T-1)%/%24+1) %>% 
  group_by(Sc,Sv,Sr,D,cat1,cat2) %>%
  reframe(value=sum(value)) %>% 
  filter(cat1!='NoPOL')

# plot all scenario
g_4_14 <- df_da_ele %>% 
  filter(Sr=='JPN',Sc%in%c('default','default w/ grid'),value!=0) %>%
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value),
         D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('default','default w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
  facet_wrap(vars(Sc),ncol=1) +
  labs(x='',y='electricity balance (GWh/day)') +
  plot_theme +
  theme(legend.position = 'bottom')

ggsave(filename=paste0(odir,'/Fig_4_14.png'),g_4_14,width=7,height=8)

g_4_15 <- df_da_ele %>% 
  filter(Sr=='JPN',Sc%in%c('demand+','demand+ w/ grid'),value!=0) %>%
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value),
         D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('demand+','demand+ w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
  facet_wrap(vars(Sc),ncol=1) +
  labs(x='',y='electricity balance (GWh/day)') +
  plot_theme +
  theme(legend.position = 'bottom')
ggsave(filename=paste0(odir,'/Fig_4_15.png'),g_4_15,width=7,height=8)

g_4_17 <- df_hr_ele %>% 
  filter(Sr=='JPN',Sc%in%c('default','default w/ grid'),
         T%in%seq(calc_d2h('2050-07-25','2050-08-07')[[1]], calc_d2h('2050-07-25','2050-08-07')[[2]]),
         value!=0) %>%
  mutate(T=as.POSIXct(paste0(plot_year,'-01-01'))+(T+24-1)*3600) %>% 
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value)) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('default','default w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=T,y=value/1000,fill=Sv),stat='identity',width=3600) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_datetime(date_breaks='1 day',labels=scales::date_format("%b %d")) +
  facet_wrap(vars(Sc),ncol=1) +
  labs(x='',y='electricity balance (GWh/h)') +
  plot_theme +
  theme(legend.position = 'bottom')
ggsave(filename=paste0(odir,'/Fig_4_17.png'),g_4_17,width=7,height=7)

g_4_18 <- df_hr_ele %>% 
  filter(Sr=='JPN',Sc%in%c('default','default w/ grid'),
         T%in%seq(calc_d2h('2050-03-08','2050-03-21')[[1]], calc_d2h('2050-03-08','2050-03-21')[[2]]),
         value!=0) %>%
  mutate(T=as.POSIXct(paste0(plot_year,'-01-01'))+(T+24-1)*3600) %>% 
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value)) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('default','default w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=T,y=value/1000,fill=Sv),stat='identity',width=3600) +
  scale_x_datetime(date_breaks='1 day',labels=scales::date_format("%b %d")) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  facet_wrap(vars(Sc),ncol=1) +
  labs(x='',y='electricity balance (GWh/h)') +
  plot_theme +
  theme(legend.position = 'bottom')
ggsave(filename=paste0(odir,'/Fig_4_18.png'),g_4_18,width=7,height=7)

# for slide
plt <- set_plot(elehr3)

df_hr_ele3 <- df_all_hr %>%
  filter(Sr=='JPN') %>% 
  mutate(Sv=recode(Sv,                   
                   'Sec_Ene_Ele_Sto_Cha_Gro_LiB'='Charge',
                   'Sec_Ene_Ele_Sto_Cha_Gro_PHS'='Charge',
                   'Sec_Ene_Ele_Nuc'='Other',
                   'Sec_Ene_Ele_Hyd'='Other',
                   'Sec_Ene_Ele_Geo'='Other',
                   'Sec_Ene_Ele_Coa'='Other',
                   'Sec_Ene_Ele_Bio'='Other',
                   'Sec_Ene_Ele_Gas'='Other',
                   'Sec_Ene_Ele_Oil'='Other',
                   'Sec_Ene_Ele_Sto_Dis_LiB'='Discharge',
                   'Sec_Ene_Ele_Sto_Dis_PHS'='Discharge',
                   'Sec_Ene_Ele_Own_Use_Ene_Sup_Hyd'='Charge',
                   'Sec_Ene_Ele_Cur'='Curtailment',
                   'Sec_Ene_Ele_inc_Cur_Win'='Wind',
                   'Sec_Ene_Ele_inc_Cur_Solar'='Solar',
                   'Sec_Ene_Ele_H2_FC'='Discharge',
                   'Sec_Ene_Ele_H2_GT'='Discharge',
                   'Trd_Sec_Ene_Ele'='Transmission')) %>% 
  filter(Sv%in%elehr3$Variable) %>%
  group_by(Sc,Sv,Sr,St) %>% 
  reframe(value=sum(value)) %>% 
  complete(Sc,Sv,Sr,St,fill=list(value=0)) %>%
  mutate(T=(as.numeric(str_sub(St,start=2,end=4))-1)*24+as.numeric(str_sub(St,start=6,end=7))+1+9) %>% 
  mutate(Sc=factor(Sc,levels=scen_mat$Scenario)) %>%
  mutate(cat1=scen_mat$scen_dpol[Sc], cat2=scen_mat$scen_tpol[Sc])

g_s0 <- df_hr_ele3 %>% 
  filter(Sr=='JPN',Sc%in%c('default','default w/ grid'),
         T%in%seq(calc_d2h('2050-03-19','2050-03-20')[[1]], calc_d2h('2050-03-19','2050-03-20')[[2]]),
         value!=0) %>%
  mutate(T=as.POSIXct(paste0(plot_year,'-01-01'))+(T+24-1)*3600) %>% 
  mutate(value=case_when(Sv%in%flag_elehr3~-value,
                         TRUE~value)) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr3$Variable)),
         Sc=factor(Sc,levels=c('default','default w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=T,y=value/1000,fill=Sv),stat='identity',width=3600) +
  geom_line(data=df_hr_ele %>% 
              filter(Sv=='Fin_Ene_Ele') %>% 
              mutate(T=(as.numeric(str_sub(St,start=2,end=4))-1)*24+as.numeric(str_sub(St,start=6,end=7))+1+9) %>% 
              filter(Sr=='JPN',Sc%in%c('default','default w/ grid'),
                                      T%in%seq(calc_d2h('2050-03-19','2050-03-20')[[1]], calc_d2h('2050-03-19','2050-03-20')[[2]])) %>% 
              mutate(T=as.POSIXct(paste0(plot_year,'-01-01'))+(T+24-1)*3600),
            aes(x=T,y=value/1000))+
  scale_x_datetime(date_breaks='1 day',labels=scales::date_format("%b %d")) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  facet_wrap(vars(Sc),ncol=1) +
  labs(x='',y='electricity balance (GWh/h)') +
  plot_theme +
  theme(legend.position = 'right')
ggsave(filename=paste0(odir,'/../Presentation/Fig_S0.png'),width=7,height=7)

