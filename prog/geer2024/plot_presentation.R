
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

aimpsy <- '//SY_3600_01/atfs02/Individual/smori/AIMPSY/output'
scenario <- 'globalNEAG'
date <- '2406152336'
cdir <- getwd()
ddir <- paste0(aimpsy,'/',scenario,'/',date)  # output path
pdir <- paste0(cdir,'/prog/inc_prog')
odir <- paste0(cdir,'/output/',date,'/presentation/main')
basedir <- 'AIMPSY-master'
reg_in <- 'NEA'
plot_year <- '2050'
time_resol <- '3HOUR'
gams_sys_dir <- 'C:/GAMS/38'

# get gams system directory
igdx(gams_sys_dir)

Sys.setlocale("LC_TIME", "en_US.UTF-8")

# define geometry
geometry_NEA <- st_read(paste0(cdir,'/define/NEA_w.geojson')) %>% 
  rename(Sr=N_PSY) %>% 
  mutate(centroid_x=case_when(Sr=='JP-KT'~centroid_x+0.55,TRUE~centroid_x),
         centroid_y=case_when(Sr=='JP-KT'~centroid_y-0.35,TRUE~centroid_y))


# import data -------------------------------------------------------------

# import scenario matrix
scen_mat_load <- read_csv(paste0(ddir,'/main/scenario_table.csv')) %>% 
  filter(Scenario%in%c('NoPOL','500C','500C_Elc30','500C_Elc35','500C_Elc40'))

# extract infeasible scenario
scen_inf <- scen_mat_load %>% filter(Status=='Infeasible')
scen_mat <- scen_mat_load %>% filter(Status!='Infeasible') %>% 
  mutate(Scenario=recode(Scenario,
                         '500C_Elc30'='w/ (2030)',
                         '500C_Elc35'='w/ (2035)',
                         '500C_Elc40'='w/ grid',
                         '500C'='w/o grid'),
         scen_tpol=recode(scen_tpol,
                          'NEAG'='w/ grid',
                          'default'='w/o grid'))

# import AIM/PSY output
df_all_hr <- rgdx.param(paste0(ddir,'/main/hr/',plot_year,'.gdx'),'data_out_hr') %>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc30'='w/ (2030)',
                   '500C_Elc35'='w/ (2035)',
                   '500C_Elc40'='w/ grid',
                   '500C'='w/o grid')) %>% 
  filter(Sc%in%c('w/o grid','w/ grid')) %>% 
  filter(Sc%in%scen_mat$Scenario) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>%
  rename(value=data_out_hr)

df_all_yr <- rgdx.param(paste0(ddir,'/main/merged_output.gdx'),'data_all_yr') %>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc30'='w/ (2030)',
                   '500C_Elc35'='w/ (2035)',
                   '500C_Elc40'='w/ grid',
                   '500C'='w/o grid')) %>% 
  filter(Sc%in%c('w/o grid','w/ grid','demand+','demand+ w/','NoPOL')) %>% 
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
source(paste0(cdir,'/define/var.R'))

# import plot function
source(paste0(pdir,'/func_analysis.R'))

eleyr3 <- tribble(~Variable,~Legend,~Color,
                  'Sec_Ene_Ele_Fos_wo_CCS','fossil w/o CCS','#4A4F54FF',
                  'Sec_Ene_Ele_Fos_w_CCS','fossil w/ CCS','#8A9197FF',
                  'Sec_Ene_Ele_Nuc','nuclear','#E68639FF',
                  'Sec_Ene_Ele_Hyd','hydro','#1C9EA0FF',
                  'Sec_Ene_Ele_Geo','geothermal','#A94A4AFF',
                  'Sec_Ene_Ele_Bio_wo_CCS','biomass w/o CCS','#617f00FF',
                  'Sec_Ene_Ele_Bio_w_CCS','biomass w/ CCS','#AFDD66FF',
                  'Sec_Ene_Ele_Win','wind','#145db8',
                  'Sec_Ene_Ele_Solar','solar','#FEC906FF',
                  'Sec_Ene_Ele_H2_FC','fuel cell','orchid',
                  'Sec_Ene_Ele_H2_GT','hydrogen gas turbine','orchid')
eleyr4 <- tribble(~Variable,~Legend,~Color,
                  'Sec_Ene_Ele_Fos','fossil','#62696FFF',
                  'Sec_Ene_Ele_Nuc','nuclear','#E68639FF',
                  'Sec_Ene_Ele_Hyd','hydro','#1C9EA0FF',
                  'Sec_Ene_Ele_Geo','geothermal','#A94A4AFF',
                  'Sec_Ene_Ele_Bio','biomass','#A9D16AFF',
                  'Sec_Ene_Ele_Win','wind','#145db8',
                  'Sec_Ene_Ele_Solar','solar','#FEC906FF',
                  'Sec_Ene_Ele_H2_FC','fuel cell','orchid',
                  'Sec_Ene_Ele_H2_GT','hydrogen gas turbine','orchid')

###########################################################################
# plot
###########################################################################

# generation mix ----------------------------------------------------------

plt <- set_plot(eleyr3)

df_yr_ele <- df_all_yr %>% 
  filter(Sv%in%eleyr$Variable,Sv!='Sec_Ene_Ele_Trd_Los',Sv!='Sec_Ene_Ele_Cur',Sv!='Sec_Ene_Ele_Sto_Los') %>%
  mutate(Sv=recode(Sv,'Sec_Ene_Ele_Sto_Dis_PHS'='Sec_Ene_Ele_Hyd'),
         Sv=case_when(Sv%in%c('Sec_Ene_Ele_Coa_wo_CCS','Sec_Ene_Ele_Oil','Sec_Ene_Ele_Gas_wo_CCS')~'Sec_Ene_Ele_Fos_wo_CCS',
                      Sv%in%c('Sec_Ene_Ele_Coa_w_CCS','Sec_Ene_Ele_Gas_w_CCS')~'Sec_Ene_Ele_Fos_w_CCS',
                      str_detect(Sv,'Sec_Ene_Ele_Win')~'Sec_Ene_Ele_Win',
                      str_detect(Sv,'Sec_Ene_Ele_Solar')~'Sec_Ene_Ele_Solar',
                      TRUE~Sv)) %>% 
  group_by(Sc,Sv,Sr,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=scen_mat$Scenario)) %>%
  mutate(cat1=scen_mat$scen_dpol[Sc], cat2=scen_mat$scen_tpol[Sc]) %>%
  mutate(cat1=factor(cat1, levels=unique(scen_mat$scen_dpol))) %>%
  mutate(cat2=factor(cat2, levels=unique(scen_mat$scen_tpol)))

g_Fig_1 <- df_yr_ele %>%
  filter(Sr%in%c('JPN','CHN','KR','MN','NEA'),
         Sc%in%c('w/o grid','w/ grid'),
         Sv!='Sec_Ene_Ele_Cur',Sv!='Sec_Ene_Ele_Sto_Los',Sv!='Sec_Ene_Ele_Trd_Los',
         Y5==2050) %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value),
         Sr=recode(Sr,
                   'JPN'='Japan',
                   'CHN'='China',
                   'KR'='Korea',
                   'MN'='Mongolia',
                   'NEA'='Total')) %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr3$Variable)),
         Sc=factor(Sc,levels=c('w/o grid','w/ grid')),
         Sr=factor(Sr,levels=c('Japan','China','Korea','Mongolia','Total'))) %>%
  ggplot() +
  geom_bar(aes(x=Sc,y=value/10^9,fill=Sv),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  labs(x='',y='power generation (PWh/yr)') +
  facet_wrap(vars(Sr),nrow=1,scales='free') +
  guides(fill=guide_legend(nrow=2)) +
  plot_theme +
  theme(legend.position='bottom')
plot(g_Fig_1)

ggsave(filename=paste0(odir,'/Fig_1_1.png'),g_Fig_1,
       width=6,height=4.5)


# import & export ---------------------------------------------------------

df_flw_exp <- foreach (i=c('500C_Elc40')) %do% {
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
  mutate(Sc=recode(Sc,'500C_Elc40'='default w/ grid')) %>% 
  filter(Sc%in%c('default w/ grid')) 

df_flw_imp <- foreach (i=c('500C_Elc40')) %do% {
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
  mutate(Sc=recode(Sc,'500C_Elc40'='default w/ grid')) %>% 
  filter(Sc%in%c('default w/ grid')) 

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
                   '500C_Elc30'='w/ grid (2030)',
                   '500C_Elc35'='w/ grid (2035)',
                   '500C_Elc40'='w/ grid',
                   '500C'='w/o grid'),
         Flag=case_when(str_detect(Sv,'domestic')~'domestic',
                        !str_detect(Sv,'domestic')~'international')) %>% 
  filter(Sc%in%c('w/o grid','w/ grid'))

# capacity

g_2_1_0 <- df_lnkcap_agg %>%
  mutate(Sv=factor(Sv,levels=c('China domestic',
                               'Japan domestic',
                               'China/Mongolia',
                               'China/Korea',
                               'Korea/Japan'))) %>% 
  # pivot_wider(names_from=Y5,values_from=value,values_fill=0) %>%
  # pivot_longer(cols=-c(Sc,Sv),names_to='Y5',values_to='value',names_transform=as.numeric) %>% 
  ggplot() +
  geom_line(aes(x=Y5,y=value/10^3,color=Sv,linetype=Sc),stat='identity',linewidth=0.3) +
  geom_point(aes(x=Y5,y=value/10^3,color=Sv,shape=Sc)) +
  geom_hline(yintercept=0,linewidth=0.2,color='grey60') +
  # scale_color_manual(values=c('China domestic'='#5ea6f2',
  #                            'Japan domestic'='#1a66b8',
  #                            'China/Mongolia'='#fcca3f',
  #                            'China/Korea'='#e6a73c',
  #                            'Korea/Japan'='#b85e1a')) +
  scale_color_manual(values=c('China domestic'='#FF713F',
                              'Japan domestic'='#B42F00',
                              'China/Mongolia'='#20B2AA',
                              'China/Korea'='#167873',
                              'Korea/Japan'='grey20')) +
  scale_shape_manual(values=c(1,4)) +
  labs(x='',y='interconnection capacity (GW)',tag='a)') +
  plot_theme +
  theme(legend.position='bottom')
plot(g_2_1_0)

g_2_1_1 <- df_lnkcap_agg %>%
  filter(Flag=='domestic',Y5>=2015) %>% 
  # pivot_wider(names_from=Y5,values_from=value,values_fill=0) %>%
  # pivot_longer(cols=-c(Sc,Sv),names_to='Y5',values_to='value',names_transform=as.numeric) %>% 
  ggplot() +
  geom_line(aes(x=Y5,y=value/10^3,color=Sv,linetype=Sc),stat='identity',linewidth=0.3) +
  geom_point(aes(x=Y5,y=value/10^3,color=Sv,shape=Sc)) +
  geom_hline(yintercept=0,linewidth=0.1,color='grey60') +
  # scale_color_manual(values=c('China domestic'='#5ea6f2',
  #                            'Japan domestic'='#1a66b8',
  #                            'China/Mongolia'='#fcca3f',
  #                            'China/Korea'='#e6a73c',
  #                            'Korea/Japan'='#b85e1a')) +
  scale_color_manual(values=c('China domestic'='#FF713F',
                              'Japan domestic'='#B42F00',
                              'China/Mongolia'='#20B2AA',
                              'China/Korea'='#167873',
                              'Korea/Japan'='grey20')) +
  scale_shape_manual(values=c(1,4)) +
  labs(x='',y='interconnection capacity (GW)') +
  guides(color='none') +
  plot_theme +
  theme(legend.position=c(0.7,0.5))
plot(g_2_1_1)

g_2_1_2 <- df_lnkcap_agg %>%
  filter(Flag=='international') %>% 
  group_by(Sv,Y5,Flag) %>% 
  complete(Sc=c('w/ grid','w/o grid'),fill=list(value=0)) %>% 
  ungroup() %>% 
  # pivot_wider(names_from=Y5,values_from=value,values_fill=0) %>%
  # pivot_longer(cols=-c(Sc,Sv),names_to='Y5',values_to='value',names_transform=as.numeric) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey60') +
  geom_line(aes(x=Y5,y=value/10^3,color=Sv,linetype=Sc),stat='identity',linewidth=0.3) +
  geom_point(aes(x=Y5,y=value/10^3,color=Sv,shape=Sc)) +
  # scale_color_manual(values=c('China domestic'='#5ea6f2',
  #                            'Japan domestic'='#1a66b8',
  #                            'China/Mongolia'='#fcca3f',
  #                            'China/Korea'='#e6a73c',
  #                            'Korea/Japan'='#b85e1a')) +
  scale_color_manual(values=c('China domestic'='#FF713F',
                              'Japan domestic'='#B42F00',
                              'China/Mongolia'='#20B2AA',
                              'China/Korea'='#167873',
                              'Korea/Japan'='grey20')) +
  scale_shape_manual(values=c(1,4)) +
  scale_x_continuous(n.breaks=3,limits=c(2039,2050)) +
  labs(x='',y='',tag='') +
  plot_theme +
  theme(legend.position='none')
plot(g_2_1_2)

g_2_1 <- ((g_2_1_1 + g_2_1_2 + plot_layout(width=c(2.5,1.5))) /
                g_legend(g_2_1_0+guides(color=guide_legend(nrow=2),linetype='none',shape='none'))) +
  plot_layout(heights = c(8,1))
plot(g_2_1)

ggsave(filename=paste0(odir,'/Fig2_1.png'),g_2_1,
       width=4,height=4)

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
  transmute(Sc,Sr=N,Sv=Variable,Sy=YEAR,value=prm2sec_sto_cap) %>% 
  mutate(Sy=as.numeric(as.character(Sy))) %>% 
  arrange(Sy) %>% 
  mutate(Sr2=case_when(str_detect(Sr,'JP-')~'Japan',
                       str_detect(Sr,'CN-')~'China',
                       str_detect(Sr,'KR')~'Korea',
                       str_detect(Sr,'MN')~'Mongolia')) %>% 
  mutate(Y5=ceiling(Sy-2001)%/%5*5+2005) %>% 
  group_by(Sc,Sr,Sr2,Sv,Y5) %>% 
  reframe(value=mean(value)) %>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc30'='w/ grid (2030)',
                   '500C_Elc35'='w/ grid (2035)',
                   '500C_Elc40'='w/ grid',
                   '500C'='w/o grid')) %>% 
  filter(Sc%in%c('w/ grid','w/o grid'))

df_stocap_total <- df_stocap %>%
  group_by(Sc,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sr='Total',Sr2='Total')

g_2_2 <- bind_rows(df_stocap,df_stocap_total) %>%
  filter(Sv%in%c('Cap_Ele_Sto_LiB_Ene','Cap_Ele_Sto_PHS_Ene'),Sc%in%c('w/o grid','w/ grid'),
         Y5==2050) %>%
  group_by(Sc,Sr2,Sv,Y5) %>%
  reframe(value=sum(value)) %>%
  mutate(Sc=factor(Sc,levels=c('w/ grid','w/o grid')),
         Sr2=factor(Sr2,levels=c('Japan','China','Korea','Mongolia','Total'))) %>%
  mutate(Sv=recode(Sv,'Cap_Ele_Sto_LiB_Ene'='Li-ion battery','Cap_Ele_Sto_PHS_Ene'='pumped hydro')) %>%
  filter(Sv=='Li-ion battery') %>%
  ggplot() +
  geom_point(aes(x=Sr2,y=value/10^6,shape=Sc),stat='identity') +
  labs(x='',y='energy capacity of battery (TWh)') +
  scale_shape_manual(values=c(1,4)) +
  plot_theme +
  theme(legend.position = 'bottom')
plot(g_2_2)

ggsave(filename=paste0(odir,'/Fig_2_2.png'),g_2_2,
       width=2.5,height=4)

# # heat map
# df_flw_heat <- df_flw_exp %>% 
#   mutate(Sv=case_when(str_detect(N0,'CN-')&str_detect(N1,'MN')~'China/Mongolia',
#                       str_detect(N0,'CN-')&str_detect(N1,'KR')~'China/Korea',
#                       str_detect(N0,'KR')&str_detect(N1,'JP-')~'Korea/Japan')) %>% 
#   group_by(Sc,Sv,St) %>% 
#   reframe(value=sum(value)) %>% 
#   complete(Sc,Sv,St=St,fill=list(value=0)) %>% 
#   mutate(T=(as.numeric(str_sub(St,start=2,end=4))-1)*24+as.numeric(str_sub(St,start=6,end=7))+1,
#          T=as.POSIXct(paste0(plot_year,'-01-01'))+(T+9-1)*3600,
#          D=as.Date(T+9*3600),
#          H=as.numeric(format(T, "%H"))) %>% 
#   left_join(df_lnkcap_agg %>% filter(!str_detect(Sv,'domestic'),Y5==2050) %>% transmute(Sv,vfs=value)) %>% 
#   mutate(value=value/vfs*100)
# 
# g_2_2 <- df_flw_heat %>% 
#   mutate(Sv=str_replace_all(Sv,'/',' to ')) %>% 
#   ggplot() +
#   geom_tile(aes(x=D,y=H,fill=value)) +
#   facet_wrap(vars(Sv),ncol=1) +
#   scale_fill_distiller(palette='Spectral') +
#   labs(x='day of the year',y='hour of the day (UTC+9)',
#        fill='power flow (% of transmission capacity)') +
#   scale_x_date(labels=scales::date_format("%b %d")) + 
#   scale_y_continuous(breaks=seq(0,24,6)) +
#   plot_theme +
#   theme(axis.title.x = element_blank(),
#         legend.title = element_text(size = 10),
#         axis.text.x=element_text(vjust=1),
#         legend.position='bottom') +
#   guides(fill=guide_colorbar(title.position='top',
#                              barwidth = 15,
#                              barheight =0.5))
# plot(g_2_2)
# ggsave(filename=paste0(odir,'/Fig_2_2.png'),g_15,width=4.5,height=5)



# Power flow in Japan -----------------------------------------------------

plt <- set_plot(eleyr4)

create_pie_chart <- function(df,i,j) {
  g_pie <- df %>% 
    filter(Sr==i,Sc==j) %>% 
    ggplot() +
    geom_bar(aes(x='',y=value,fill=Sv),stat='identity',width=1,position=position_stack(reverse=TRUE)) +
    coord_polar('y') +
    scale_fill_manual(values=plt$Color,labels=plt$Legend)+
    theme_void() +
    theme(legend.position='none')
}

df_genmix <- df_yr_ele %>%
  filter(Y5==2050,str_detect(Sr,'JP-'),Sc%in%c('w/o grid','w/ grid'),
         Sv!='Sec_Ene_Ele_Cur',Sv!='Sec_Ene_Ele_Sto_Los',Sv!='Sec_Ene_Ele_Trd_Los') %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value),
         Sv=case_when(str_detect(Sv,'Sec_Ene_Ele_Fos')~'Sec_Ene_Ele_Fos',
                      str_detect(Sv,'Sec_Ene_Ele_Bio')~'Sec_Ene_Ele_Bio',
                      TRUE~Sv)) %>% 
  group_by(Sc,Sv,Sr,Y5,cat1,cat2) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sv=factor(Sv,levels=rev(eleyr4$Variable)),
         Sc=factor(Sc,levels=c('w/o grid','w/ grid')),
         Sr=factor(Sr,levels=c('JP-HD','JP-TH','JP-KT','JP-CB','JP-HR','JP-KS','JP-CG','JP-SK','JP-KY'))) %>% 
  left_join(geometry_NEA %>% st_drop_geometry())

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
  separate(PW_LNK,sep='_',into=c('N0','N1'),remove=TRUE) %>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc30'='w/ grid (2030)',
                   '500C_Elc35'='w/ grid (2035)',
                   '500C_Elc40'='w/ grid',
                   '500C'='w/o grid')) %>% 
  filter(Sc%in%c('w/ grid','w/o grid'),
         str_detect(N1,'JP-'),Y5==2050) %>%
  mutate(N0=str_remove_all(N0,'-AC'),
         N1=str_remove_all(N1,'-AC'),
         value=value/1000) %>% 
  left_join(geometry_NEA %>% st_drop_geometry(),by=c('N0'='Sr')) %>% 
  rename(N0_x=centroid_x,N0_y=centroid_y) %>% 
  left_join(geometry_NEA %>% st_drop_geometry(),by=c('N1'='Sr')) %>% 
  rename(N1_x=centroid_x,N1_y=centroid_y)

linewidth_scale <- scale_linewidth_continuous(limits=c(min(df_lnkcap_agg$value),max(df_lnkcap_agg$value)),
                                              range=c(0.2,2.3),
                                              breaks=c(10,30),
                                              labels=paste0(c(10,30),' GW'),
                                              name='')

df_mcoe <- foreach (i=c('500C','500C_Elc40')) %do% {
  eq_npb_m_AC <- rgdx.param(paste0(ddir,'/gams_output/gdx_primary/',i,'/2050.gdx'),'eq_npb_m') %>% 
    filter(MT!='ANN',I=='AC') %>% rename(T=MT) %>% select(-I)
  eq_npb_m_AC2 <- rgdx.param(paste0(ddir,'/gams_output/gdx_primary/',i,'/2050.gdx'),'eq_npb_m') %>% 
    filter(MT!='ANN',I=='AC2') %>% rename(T=MT) %>% select(-I)
  dmd <- rgdx.param(paste0(ddir,'/gams_output/gdx_primary/',i,'/2050.gdx'),'dmd') %>% 
    filter(I=='AC2') %>% rename(T=MT)
  tmp <- bind_rows(eq_npb_m_AC %>% left_join(dmd) %>% mutate(I='AC'),
                   eq_npb_m_AC2 %>% left_join(dmd) %>% mutate(I='AC2')) %>% 
    mutate(Sc=i)
} %>% bind_rows()

df_mcoe_JPN <- df_mcoe %>% 
  filter(str_detect(N,'JP-'),I=='AC') %>% 
  group_by(Sc,N) %>% 
  reframe(value=sum(dmd*eq_npb_m)/sum(dmd)) %>% 
  transmute(Sc=recode(Sc,'500C_Elc40'='w/ grid','500C'='w/o grid'),
            Sr=N,value)

df_gentotal <- df_genmix %>% 
  group_by(Sc,Sr) %>% 
  reframe(value=sum(value)) %>% 
  left_join(geometry_NEA)

g_3_1_wo <- geometry_NEA %>% 
  left_join(df_mcoe_JPN) %>% 
  ggplot() +
  geom_sf(fill='white',color='black',linewidth=0.3) +
  # geom_sf(data=. %>% filter(str_detect(Sr,'JP-'),Sc=='w/o grid'),aes(fill=value),color='black',linewidth=0.3) +
  geom_segment(data=df_lnkcap_agg %>% filter(Sc=='w/o grid'),
               aes(x=N0_x,y=N0_y,xend=N1_x,yend=N1_y,linewidth=value),color='#e68ad6') +
  # scale_fill_viridis(option='mako',direction=-1,limits=c(75,115)) +
  # scale_fill_distiller(direction=1,limits=c(75,110)) +
  linewidth_scale +
  coord_sf(xlim = c(125, 148),ylim=c(30,45)) +
  theme_void() +
  theme(legend.position='none')
plot(g_3_1_wo)

g_3_1_w <- geometry_NEA %>% 
  left_join(df_mcoe_JPN) %>% 
  ggplot() +
  geom_sf(fill='white',color='black',linewidth=0.3) +
  geom_sf(data=. %>% filter(str_detect(Sr,'JP-'),Sc=='w/o grid'),aes(fill=value),color='black',linewidth=0.3) +
  geom_segment(data=df_lnkcap_agg %>% filter(Sc=='w/o grid'),
               aes(x=N0_x,y=N0_y,xend=N1_x,yend=N1_y,linewidth=value),color='#e68ad6') +
  scale_fill_viridis(option='mako',direction=-1,limits=c(75,115)) +
  # scale_fill_distiller(direction=1,limits=c(75,110)) +
  linewidth_scale +
  coord_sf(xlim = c(125, 148),ylim=c(30,45)) +
  theme_void() +
  theme(legend.position='none')
plot(g_3_1_w)

for(i in c('JP-HD','JP-TH','JP-KT','JP-CB','JP-HR','JP-KS','JP-CG','JP-SK','JP-KY')) {
  
  j <- 'w/o grid'
  g_pie <- create_pie_chart(df_genmix,i,j)
  
  size_factor <- df_genmix %>%
    filter(Sr==i,Sc==j) %>% 
    reframe(value=sum(value)/160000000-0.1) %>% 
    pull(value)
  
  g_3_1_wo <- g_3_1_wo + annotation_custom(
    ggplotGrob(g_pie), 
    xmin = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_x) - size_factor,
    xmax = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_x) + size_factor, 
    ymin = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_y) - size_factor,
    ymax = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_y) + size_factor
    
  )
}

for(i in c('JP-HD','JP-TH','JP-KT','JP-CB','JP-HR','JP-KS','JP-CG','JP-SK','JP-KY')) {
  
  j <- 'w/o grid'
  g_pie <- create_pie_chart(df_genmix,i,j)
  
  size_factor <- df_genmix %>%
    filter(Sr==i,Sc==j) %>% 
    reframe(value=sum(value)/160000000-0.1) %>% 
    pull(value)
  
  g_3_1_w <- g_3_1_w + annotation_custom(
    ggplotGrob(g_pie), 
    xmin = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_x) - size_factor,
    xmax = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_x) + size_factor, 
    ymin = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_y) - size_factor,
    ymax = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_y) + size_factor
    
  )
}

plot(g_3_1)

g_3_2_wo <- geometry_NEA %>% 
  left_join(df_mcoe_JPN) %>% 
  ggplot() +
  geom_sf(fill='white',color='black',linewidth=0.3) +
  # geom_sf(data=. %>% filter(str_detect(Sr,'JP-'),Sc=='w/ grid'),aes(fill=value),color='black',linewidth=0.3) +
  geom_segment(data=df_lnkcap_agg %>% filter(Sc=='w/ grid'),
               aes(x=N0_x,y=N0_y,xend=N1_x,yend=N1_y,linewidth=value),color='#e68ad6') +
  # scale_fill_viridis(option='mako',direction=-1,limits=c(75,115)) +
  # scale_fill_distiller(direction=1,limits=c(75,110)) +
  linewidth_scale +
  coord_sf(xlim = c(125, 148),ylim=c(30,45)) +
  guides(fill='none') +
  theme_void() +
  theme(legend.position=c(0.85,0.15))
plot(g_3_2_wo)

g_3_2_w <- geometry_NEA %>% 
  left_join(df_mcoe_JPN) %>% 
  ggplot() +
  geom_sf(fill='white',color='black',linewidth=0.3) +
  geom_sf(data=. %>% filter(str_detect(Sr,'JP-'),Sc=='w/ grid'),aes(fill=value),color='black',linewidth=0.3) +
  geom_segment(data=df_lnkcap_agg %>% filter(Sc=='w/ grid'),
               aes(x=N0_x,y=N0_y,xend=N1_x,yend=N1_y,linewidth=value),color='#e68ad6') +
  scale_fill_viridis(option='mako',direction=-1,limits=c(75,115)) +
  # scale_fill_distiller(direction=1,limits=c(75,110)) +
  linewidth_scale +
  coord_sf(xlim = c(125, 148),ylim=c(30,45)) +
  guides(fill='none') +
  theme_void() +
  theme(legend.position=c(0.85,0.15))
plot(g_3_2)

for(i in c('JP-HD','JP-TH','JP-KT','JP-CB','JP-HR','JP-KS','JP-CG','JP-SK','JP-KY')) {

  j <- 'w/ grid'
  g_pie <- create_pie_chart(df_genmix,i,j)
  
  size_factor <- df_genmix %>%
    filter(Sr==i,Sc==j) %>% 
    reframe(value=sum(value)/160000000-0.1) %>% 
    pull(value)
  
  g_3_2_wo <- g_3_2_wo + annotation_custom(
    ggplotGrob(g_pie), 
    xmin = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_x) - size_factor,
    xmax = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_x) + size_factor, 
    ymin = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_y) - size_factor,
    ymax = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_y) + size_factor

  )
}

for(i in c('JP-HD','JP-TH','JP-KT','JP-CB','JP-HR','JP-KS','JP-CG','JP-SK','JP-KY')) {

  j <- 'w/ grid'
  g_pie <- create_pie_chart(df_genmix,i,j)
  
  size_factor <- df_genmix %>%
    filter(Sr==i,Sc==j) %>% 
    reframe(value=sum(value)/160000000-0.1) %>% 
    pull(value)
  
  g_3_2_w <- g_3_2_w + annotation_custom(
    ggplotGrob(g_pie), 
    xmin = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_x) - size_factor,
    xmax = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_x) + size_factor, 
    ymin = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_y) - size_factor,
    ymax = geometry_NEA %>% filter(Sr==i) %>% pull(centroid_y) + size_factor

  )
}

plot(g_3_2)

g_3_gen_l <- g_legend(g_pie+guides(fill=guide_legend(nrow=1,title=''))+theme(legend.position='bottom')) 
plot(g_3_gen_l)

g_3_prc_l <- g_legend(g_3_1+guides(linewidth='none',
                                   fill=guide_colourbar(title='annual mean MCOE (USD/kWh)  ',
                                                        barwidth=12,
                                                        barheight=0.5,
                                                        title.vjust=1))
                      +theme(legend.position='bottom',legend.title=element_text(size=9))) 
plot(g_3_prc_l)

g_3_wo_prc <- ((g_3_1_wo + g_3_2_wo) / g_3_gen_l)  + plot_layout(heights=c(6,1))
plot(g_3_wo_prc)

g_3_w_prc <- ((g_3_1_w + g_3_2_w) / g_3_gen_l / g_3_prc_l)  + plot_layout(heights=c(6,1,1))
plot(g_3_w_prc)

# df_legend <- data_frame(
#   x=c(140,140,140),
#   y=c(30,40,50),
#   Sr=c('sample1','sample2','sample3'),
#   Sc=rep('sample'),
#   value=c(30000,100000,300000)
# )
# 
# create_pie_legend <- function(df,i,j) {
#   g_pie <- df %>% 
#     filter(Sr==i,Sc==j) %>% 
#     ggplot() +
#     geom_bar(aes(x='',y=value),stat='identity',width=1,position=position_stack(reverse=TRUE)) +
#     coord_polar('y') +
#     theme_void() +
#     theme(legend.position='none')
# }
# 
# g_3_l <- ggplot() + theme_void()
# 
# for(i in c('sample1','sample2','sample3')) {
#   
#   j <- 'sample'
#   g_pie <- create_pie_legend(df_legend,i,j)
#   
#   size_factor <- df_legend %>%
#     filter(Sr==i,Sc==j) %>% 
#     reframe(value=sum(value)/160000000-0.1) %>% 
#     pull(value)
#   
#   g_3_l <- g_3 + annotation_custom(
#     ggplotGrob(g_pie), 
#     xmin = df_legend %>% filter(Sr==i) %>% pull(x) - size_factor,
#     xmax = df_legend %>% filter(Sr==i) %>% pull(x) + size_factor, 
#     ymin = df_legend %>% filter(Sr==i) %>% pull(y) - size_factor,
#     ymax = df_legend %>% filter(Sr==i) %>% pull(y) + size_factor
#     
#   )
# }
# 
# plot(g_3_l)

ggsave(filename=paste0(odir,'/Fig_3_wo_prc.png'),g_3_wo_prc,
       width=8,height=4.5,dpi=500)
ggsave(filename=paste0(odir,'/Fig_3_w_prc.png'),g_3_w_prc,
       width=8,height=4.5,dpi=500)


# cost --------------------------------------------------------------------

df_mcoe <- foreach (i=c('500C','500C_Elc40')) %do% {
  eq_npb_m_AC <- rgdx.param(paste0(ddir,'/gams_output/gdx_primary/',i,'/2050.gdx'),'eq_npb_m') %>% 
    filter(MT!='ANN',I=='AC') %>% rename(T=MT) %>% select(-I)
  eq_npb_m_AC2 <- rgdx.param(paste0(ddir,'/gams_output/gdx_primary/',i,'/2050.gdx'),'eq_npb_m') %>% 
    filter(MT!='ANN',I=='AC2') %>% rename(T=MT) %>% select(-I)
  dmd <- rgdx.param(paste0(ddir,'/gams_output/gdx_primary/',i,'/2050.gdx'),'dmd') %>% 
    filter(I=='AC2') %>% rename(T=MT)
  tmp <- bind_rows(eq_npb_m_AC %>% left_join(dmd) %>% mutate(I='AC'),
                   eq_npb_m_AC2 %>% left_join(dmd) %>% mutate(I='AC2')) %>% 
    mutate(Sc=i)
} %>% bind_rows()

df_mcoe_JPN <- df_mcoe %>% 
  filter(str_detect(N,'JP-')) %>% 
  group_by(Sc,I,T) %>% 
  reframe(value=sum(dmd*eq_npb_m)/sum(dmd)) %>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc30'='w/ grid (2030)',
                   '500C_Elc35'='w/ grid (2035)',
                   '500C_Elc40'='w/ grid',
                   '500C'='w/o grid'))

df_mcoe_ann <- df_mcoe %>% 
  mutate(Sr=case_when(str_detect(N,'JP-')~'JPN',
                      str_detect(N,'CN-')~'CHN',
                      N=='KR'~'KOR',
                      N=='MN'~'MNG')) %>% 
  group_by(Sr,Sc,I) %>% 
  reframe(value=sum(dmd*eq_npb_m)/sum(dmd))%>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc30'='w/ grid (2030)',
                   '500C_Elc35'='w/ grid (2035)',
                   '500C_Elc40'='w/ grid',
                   '500C'='w/o grid'))

df_mcoe_ann_total <- df_mcoe %>% 
  group_by(Sc,I) %>% 
  reframe(value=sum(dmd*eq_npb_m)/sum(dmd))%>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc30'='w/ grid (2030)',
                   '500C_Elc35'='w/ grid (2035)',
                   '500C_Elc40'='w/ grid',
                   '500C'='w/o grid')) %>% 
  mutate(Sr='Total')

scen_cat <- scen_mat %>% 
  transmute(Sc=Scenario,cat1=scen_dpol,cat2=scen_tpol)

g_4_1 <- bind_rows(df_mcoe_ann,df_mcoe_ann_total) %>% 
  filter(I=='AC') %>% 
  ggplot() +
  geom_point(aes(x=Sr,y=value,shape=Sc)) +
  scale_shape_manual(values=c(1,4)) +
  scale_y_continuous(limits=c(0,NA)) +
  labs(x='',y='Annual mean MCOE\n(USD/kWh)') +
  plot_theme +
  theme(legend.position=c(0.75,0.25),
        legend.text=element_text(size=10))

# g_4_2 <- df_mcoe_JPN %>%
#   group_by(Sc,I) %>% 
#   filter(I=='AC') %>% 
#   arrange(-value) %>% mutate(T=row_number()) %>% 
#   ggplot() +
#   geom_step(aes(x=T,y=value,linetype=Sc),linewidth=0.3) +
#   scale_color_d3() +
#   labs(y='Marginal cost of electricity (USD/kWh)') +
#   scale_x_continuous(breaks=c(1,seq(1000,8000,1000),8760)) +
#   scale_y_continuous(limits=c(0,500))  +
#   scale_linetype_manual(values=c('w/o grid'='solid','w/ grid'='dashed')) +
#   labs(x='',y='MCOE (USD/kWh)') +
#   plot_theme +
#   theme(legend.position=c(0.7,0.8),
#         legend.text=element_text(size=10))

g_4_2 <- df_mcoe_JPN %>%
  group_by(Sc,I) %>% 
  filter(I=='AC') %>% 
  arrange(-value) %>% mutate(T=row_number()) %>% 
  ggplot() +
  geom_point(aes(x=T,y=value,shape=Sc,color=Sc)) +
  scale_color_manual(values=c('#FF713F','#20B2AA')) +
  labs(y='Marginal cost of electricity (USD/kWh)') +
  scale_x_continuous(breaks=c(1,seq(1000,8000,1000),8760)) +
  scale_y_continuous(limits=c(0,500))  +
  scale_shape_manual(values=c(1,4)) +
  labs(x='',y='MCOE (USD/kWh)') +
  plot_theme +
  theme(legend.position=c(0.7,0.8),
        legend.text=element_text(size=10))
plot(g_4_2)

g_4 <- g_4_1 + g_4_2 + plot_layout(nrow=1,widths=c(4,7))

ggsave(filename=paste0(odir,'/Fig_4.png'),g_4,width=8,height=4)

# gen_inv_map <- tribble(~R,~Sv,
#                        'PVC','CAPEX (solar)',
#                        'PVDC','CAPEX (solar)',
#                        'PVDR','CAPEX (solar)',
#                        'WNO','CAPEX (wind)',
#                        'WNB','CAPEX (wind)',
#                        'WNF','CAPEX (wind)',
#                        'GEO','CAPEX (other)',
#                        'BMS','CAPEX (other)',
#                        'BMS_CCS','CAPEX (other)',
#                        'PC','CAPEX (fossil w/o CCS)',
#                        'SC','CAPEX (fossil w/o CCS)',
#                        'IGCC','CAPEX (fossil w/o CCS)',
#                        'ST','CAPEX (fossil w/o CCS)',
#                        'OCGT','CAPEX (fossil w/o CCS)',
#                        'CCGT','CAPEX (fossil w/o CCS)',
#                        'CCGT_CCS','CAPEX (fossil w/ CCS)',
#                        'OIL','CAPEX (fossil w/o CCS)')
# 
# df_gensto_sys <- foreach (i=c('500C','500C_Elc30','500C_Elc35','500C_Elc40','NoPOL')) %do% {
#   gen_inv <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_gen_inv2') %>% 
#     filter(SE0=='ALL') %>% select(-SE0) %>% 
#     left_join(gen_inv_map) %>% 
#     group_by(N,Sv,YEAR) %>% 
#     reframe(value=sum(prm2sec_gen_inv2))
#   
#   sto_inv <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_sto_inv2') %>% 
#     filter(SE0=='ALL') %>% select(-SE0) %>% 
#     mutate(Sv='CAPEX (battery)') %>% 
#     group_by(N,Sv,YEAR) %>% 
#     reframe(value=sum(prm2sec_sto_inv2))
#   
#   gen_fom <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_gen_fom2') %>% 
#     filter(SE0=='ALL') %>% select(-SE0) %>% 
#     mutate(Sv='OPEX') %>% 
#     group_by(N,Sv,YEAR) %>% 
#     reframe(value=sum(prm2sec_gen_fom2))
#   
#   sto_fom <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_sto_fom2') %>% 
#     filter(SE0=='ALL') %>% select(-SE0) %>% 
#     mutate(Sv='OPEX') %>% 
#     group_by(N,Sv,YEAR) %>% 
#     reframe(value=sum(prm2sec_sto_fom2))
#   
#   gen_vom <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_gen_vom2') %>% 
#     filter(SE0=='ALL') %>% select(-SE0) %>% 
#     mutate(Sv='Fuel') %>% 
#     group_by(N,Sv,YEAR) %>% 
#     reframe(value=sum(prm2sec_gen_vom2))
#   
#   tmp <- bind_rows(gen_inv,sto_inv,gen_fom,sto_fom,gen_vom) %>% 
#     mutate(Sc=i)
#   
# } %>% bind_rows()
# 
# 
# df_lnk_sys <- foreach (i=c('500C','500C_Elc30','500C_Elc35','500C_Elc40','NoPOL')) %do% {
#   lnk_inv <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_lnk_inv2') %>% 
#     separate(L,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
#     mutate(N0=str_remove_all(N0,'-AC$|-DC$|-H2$|-PHS$|-AC2$'),
#            N1=str_remove_all(N1,'-AC$|-DC$|-H2$|-PHS$|-AC2$'),
#            Sv=case_when(str_detect(L,'.*-AC_.*-AC$')&str_sub(N0,end=2)==str_sub(N1,end=2)~'CAPEX (domestic interconnection)',
#                         str_detect(L,'.*-AC_.*-AC$')&str_sub(N0,end=2)!=str_sub(N1,end=2)~'CAPEX (international interconnection)',
#                         str_detect(L,'.*-AC_.*-DC$')~'CAPEX (battery)',
#                         str_detect(L,'.*-AC_.*-H2$')~'CAPEX (hydrogen)',
#                         str_detect(L,'.*-AC_.*-AC2$')~'CAPEX (other)',
#                         str_detect(L,'.*-AC_.*-PHS$')~'CAPEX (other)'))
#   
#   lnk_fom <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_lnk_fom2') %>% 
#     separate(L,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
#     mutate(N0=str_remove_all(N0,'-AC$|-DC$|-H2$|-PHS$|-AC2$'),
#            N1=str_remove_all(N1,'-AC$|-DC$|-H2$|-PHS$|-AC2$')) %>% 
#     mutate(Sv='OPEX')
#   
#   lnk_vom <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_lnk_vom2') %>% 
#     separate(L,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
#     mutate(N0=str_remove_all(N0,'-AC$|-DC$|-H2$|-PHS$|-AC2$'),
#            N1=str_remove_all(N1,'-AC$|-DC$|-H2$|-PHS$|-AC2$')) %>% 
#     mutate(Sv='Fuel')
#   
#   tmp <- bind_rows(lnk_inv %>% filter(str_sub(N0,end=2)==str_sub(N1,end=2)) %>% group_by(N0,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_inv2)) %>% rename(N=N0),
#                    lnk_fom %>% filter(str_sub(N0,end=2)==str_sub(N1,end=2)) %>% group_by(N0,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_fom2)) %>% rename(N=N0),
#                    lnk_vom %>% filter(str_sub(N0,end=2)==str_sub(N1,end=2)) %>% group_by(N0,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_vom2)) %>% rename(N=N0),
#                    lnk_inv %>% filter(str_sub(N0,end=2)!=str_sub(N1,end=2)) %>% group_by(N0,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_inv2)/2) %>% rename(N=N0),
#                    lnk_fom %>% filter(str_sub(N0,end=2)!=str_sub(N1,end=2)) %>% group_by(N0,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_fom2)/2) %>% rename(N=N0),
#                    lnk_vom %>% filter(str_sub(N0,end=2)!=str_sub(N1,end=2)) %>% group_by(N0,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_vom2)/2) %>% rename(N=N0),
#                    lnk_inv %>% filter(str_sub(N0,end=2)!=str_sub(N1,end=2)) %>% group_by(N1,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_inv2)/2) %>% rename(N=N1),
#                    lnk_fom %>% filter(str_sub(N0,end=2)!=str_sub(N1,end=2)) %>% group_by(N1,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_fom2)/2) %>% rename(N=N1),
#                    lnk_vom %>% filter(str_sub(N0,end=2)!=str_sub(N1,end=2)) %>% group_by(N1,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_vom2)/2) %>% rename(N=N1)) %>% 
#     mutate(Sc=i)
# } %>% bind_rows()
# 
# df_sys <- bind_rows(df_gensto_sys,df_lnk_sys) %>% 
#   mutate(across(where(is.factor),~as.character(.))) %>% mutate(YEAR=as.numeric(YEAR)) %>% 
#   mutate(Sr2=case_when(str_detect(N,'JP-')~'JPN',
#                        str_detect(N,'CN-')~'CHN',
#                        str_detect(N,'KR')~'KOR',
#                        str_detect(N,'MN')~'MNG')) %>% 
#   filter(YEAR>=2020) %>% 
#   group_by(Sr2,Sc,Sv,YEAR) %>%
#   reframe(value=sum(value)) %>% 
#   group_by(Sr2,Sc,Sv) %>% 
#   complete(YEAR=seq(2020,2050,5)) %>% 
#   replace_na(list(value=0)) %>% 
#   complete(YEAR=2020:2050) %>% 
#   mutate(value=na_locf(value,option='nocb')) %>% 
#   pivot_wider(names_from=Sc,values_from=value,values_fill=0) %>% 
#   mutate(across(c(`500C`,`500C_Elc40`),~.-`NoPOL`)) %>%
#   pivot_longer(cols=-c(Sv,Sr2,YEAR),names_to='Sc',values_to='value') %>% 
#   mutate(discount=(1-0.05)^(YEAR-2020)) %>% 
#   filter(Sc!='NoPOL') %>%
#   group_by(Sc,YEAR,Sr2) %>% 
#   reframe(value=sum(value*discount)) %>% 
#   mutate(Sc=recode(Sc,
#                    '500C_Elc40'='w/ grid',
#                    '500C'='w/o grid')) %>% 
#   filter(Sc%in%c('w/o grid','w/ grid'))
# 
# df_sys_nea <- df_sys %>%
#   group_by(Sc,YEAR) %>% 
#   reframe(value=sum(value)) %>% 
#   mutate(Sr2='Total')
# 
# df_GDP <- read_csv(paste0(cdir,'/define/GDP_SSP2_v3.csv')) %>% 
#   mutate(Sr2=recode(Region,'China'='CHN','Japan'='JPN','South Korea'='KOR','Mongolia'='MNG')) %>% 
#   select(-Model,-Scenario,-Region,-Variable,-Unit) %>% 
#   pivot_longer(cols=-c(Sr2),names_to='YEAR',values_to='GDP',names_transform=as.numeric) %>% 
#   group_by(Sr2) %>% 
#   complete(YEAR=2020:2050) %>% 
#   mutate(GDP=na_interpolation(GDP)) %>% 
#   mutate(discount=(1-0.05)^(YEAR-2020)) %>% 
#   filter(YEAR%in%c(2021:2050)) %>% 
#   mutate(Y10=case_when(YEAR%in%c(2021:2030)~'2021-30',
#                        YEAR%in%c(2031:2040)~'2031-40',
#                        YEAR%in%c(2041:2050)~'2041-50')) %>% 
#   group_by(Y10) %>% 
#   reframe(GDP=sum(GDP*discount)*10^9*91.9/102.9)
# 
# df_sys_total <- bind_rows(df_sys,df_sys_nea) %>%
#   filter(Sr2=='Total') %>% 
#   filter(YEAR%in%c(2021:2050)) %>% 
#   mutate(Y10=case_when(YEAR%in%c(2021:2030)~'2021-30',
#                        YEAR%in%c(2031:2040)~'2031-40',
#                        YEAR%in%c(2041:2050)~'2041-50')) %>% 
#   group_by(Sc,Y10) %>% 
#   reframe(value=sum(value)) %>% 
#   left_join(df_GDP2) %>%
#   mutate(Sc=factor(Sc,levels=c('w/o grid','w/ grid')))