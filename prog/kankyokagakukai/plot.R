
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
odir <- paste0(cdir,'/output/',date,'/kankyokagakukai/main')
basedir <- 'AIMPSY-master'
reg_in <- 'NEA'
plot_year <- '2050'
time_resol <- '3HOUR'
gams_sys_dir <- 'C:/GAMS/38'

# get gams system directory
igdx(gams_sys_dir)

Sys.setlocale("LC_TIME", "en_US.UTF-8")

# define geometry
geometry_NEA <- st_read(paste0(cdir,'/define/NEA_wo.geojson')) %>% 
  rename(Sr=N_PSY)
geometry_NEA_w <- st_read(paste0(cdir,'/define/NEA_w.geojson')) %>% 
  transmute(Sr=N_PSY,geometry)


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
                         '500C_Elc40'='w/',
                         '500C'='w/o'),
         scen_tpol=recode(scen_tpol,
                          'NEAG'='w/',
                          'default'='w/o'))

# import AIM/PSY output
df_all_hr <- rgdx.param(paste0(ddir,'/main/hr/',plot_year,'.gdx'),'data_out_hr') %>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc30'='w/ (2030)',
                   '500C_Elc35'='w/ (2035)',
                   '500C_Elc40'='w/',
                   '500C'='w/o')) %>% 
  filter(Sc%in%c('w/o','w/')) %>% 
  filter(Sc%in%scen_mat$Scenario) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>%
  rename(value=data_out_hr)

df_all_yr <- rgdx.param(paste0(ddir,'/main/merged_output.gdx'),'data_all_yr') %>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc30'='w/ (2030)',
                   '500C_Elc35'='w/ (2035)',
                   '500C_Elc40'='w/',
                   '500C'='w/o')) %>% 
  filter(Sc%in%c('w/o','w/','demand+','demand+ w/','NoPOL')) %>% 
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

eleyr2 <- tribble(~Variable,~Legend,~Color,
                  'Sec_Ene_Ele_Coa','coal','#4A4F54FF',
                  'Sec_Ene_Ele_Oil','oil','#8C655DFF',
                  'Sec_Ene_Ele_Gas','gas','#A99F6DFF',
                  'Sec_Ene_Ele_Nuc','nuclear','#FF9626FF',
                  'Sec_Ene_Ele_Hyd','hydro','#29dde3FF',
                  'Sec_Ene_Ele_Geo','geothermal','#B74F3AFF',
                  'Sec_Ene_Ele_Bio','biomass','#AFDD66FF',
                  'Sec_Ene_Ele_Win','wind','#145db8',
                  'Sec_Ene_Ele_Solar','solar','#FEC906FF',
                  'Sec_Ene_Ele_H2_FC','fuel cell','orchid',
                  'Sec_Ene_Ele_H2_GT','hydrogen gas turbine','orchid')

###########################################################################
# plot
###########################################################################

# generation mix ----------------------------------------------------------

plt <- set_plot(eleyr2)

df_yr_ele <- df_all_yr %>% 
  filter(Sv%in%eleyr2$Variable) %>%
  mutate(Sv=recode(Sv,'Sec_Ene_Ele_Sto_Dis_PHS'='Sec_Ene_Ele_Hyd')) %>% 
  group_by(Sc,Sv,Sr,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=scen_mat$Scenario)) %>%
  mutate(cat1=scen_mat$scen_dpol[Sc], cat2=scen_mat$scen_tpol[Sc]) %>%
  mutate(cat1=factor(cat1, levels=unique(scen_mat$scen_dpol))) %>%
  mutate(cat2=factor(cat2, levels=unique(scen_mat$scen_tpol)))

g_Fig_1 <- df_yr_ele %>%
  filter(Sr%in%c('JPN','CHN','KR','MN'),
         Sc%in%c('w/o','w/'),
         Sv!='Sec_Ene_Ele_Cur',Sv!='Sec_Ene_Ele_Sto_Los',Sv!='Sec_Ene_Ele_Trd_Los',
         Y5==2050) %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value),
         Sr=recode(Sr,'KR'='KOR','MN'='MNG')) %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr2$Variable)),
         Sc=factor(Sc,levels=c('w/o','w/')),
         Sr=factor(Sr,levels=c('JPN','CHN','KOR','MNG'))) %>%
  ggplot() +
  geom_bar(aes(x=Sc,y=value/10^9,fill=Sv),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  labs(x='',y='power generation in 2050\n(PWh/yr)') +
  facet_wrap(vars(Sr),nrow=1,scales='free') +
  plot_theme +
  theme(legend.position='bottom')
plot(g_Fig_1)

ggsave(filename=paste0(odir,'/Fig1.png'),g_Fig_1,
       width=5,height=3.25)


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
  mutate(Sv=case_when(str_detect(N0,'CN-')&str_detect(N1,'CN-')~'CHN',
                      str_detect(N0,'JP-')&str_detect(N1,'JP-')~'JPN',
                      str_detect(N0,'CN-')&str_detect(N1,'MN-')~'CHN-MNG',
                      str_detect(N0,'CN-')&str_detect(N1,'KR-')~'CHN-KOR',
                      str_detect(N0,'KR-')&str_detect(N1,'JP-')~'KOR-JPN')) %>% 
  group_by(Sc,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sv=factor(Sv,levels=c('CHN-MNG','CHN-KOR','KOR-JPN','CHN','JPN'))) %>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc30'='w/ (2030)',
                   '500C_Elc35'='w/ (2035)',
                   '500C_Elc40'='w/',
                   '500C'='w/o'),
         Flag=case_when(!str_detect(Sv,'-')~'domestic',
                        str_detect(Sv,'-')~'international')) %>% 
  filter(Sc%in%c('w/o','w/'))

g_Fig_2_1_0 <- df_lnkcap_agg %>%
  filter(Y5>=2015) %>% 
  # pivot_wider(names_from=Y5,values_from=value,values_fill=0) %>%
  # pivot_longer(cols=-c(Sc,Sv),names_to='Y5',values_to='value',names_transform=as.numeric) %>% 
  ggplot() +
  geom_line(aes(x=Y5,y=value/10^3,color=Sv,linetype=Sc),stat='identity',linewidth=0.3) +
  geom_point(aes(x=Y5,y=value/10^3,color=Sv,shape=Sc)) +
  geom_hline(yintercept=0,linewidth=0.2,color='grey60') +
  # scale_color_manual(values=c('CHN'='#5ea6f2',
  #                            'JPN'='#1a66b8',
  #                            'CHN-MNG'='#fcca3f',
  #                            'CHN-KOR'='#e6a73c',
  #                            'KOR-JPN'='#b85e1a')) +
  scale_color_manual(values=c('CHN'='#d6982f',
                              'JPN'='#a14d0e',
                              'CHN-MNG'='#5ea6f2',
                              'CHN-KOR'='#1a66b8',
                              'KOR-JPN'='grey20')) +
  scale_shape_manual(values=c(1,4)) +
  labs(x='',y='interconnection capacity (GW)',tag='') +
  guides(color='none') +
  plot_theme +
  theme(legend.position=c(0.7,0.5))
plot(g_Fig_2_1_0)

g_Fig_2_1_1 <- df_lnkcap_agg %>%
  filter(Flag=='domestic',Y5>=2015) %>% 
  # pivot_wider(names_from=Y5,values_from=value,values_fill=0) %>%
  # pivot_longer(cols=-c(Sc,Sv),names_to='Y5',values_to='value',names_transform=as.numeric) %>% 
  ggplot() +
  geom_line(aes(x=Y5,y=value/10^3,color=Sv,linetype=Sc),stat='identity',linewidth=0.3) +
  geom_point(aes(x=Y5,y=value/10^3,color=Sv,shape=Sc)) +
  geom_hline(yintercept=0,linewidth=0.2,color='grey60') +
  # scale_color_manual(values=c('CHN'='#5ea6f2',
  #                            'JPN'='#1a66b8',
  #                            'CHN-MNG'='#fcca3f',
  #                            'CHN-KOR'='#e6a73c',
  #                            'KOR-JPN'='#b85e1a')) +
  scale_color_manual(values=c('CHN'='#d6982f',
                              'JPN'='#a14d0e',
                              'CHN-MNG'='#5ea6f2',
                              'CHN-KOR'='#1a66b8',
                              'KOR-JPN'='grey20')) +
  scale_shape_manual(values=c(1,4)) +
  labs(x='',y='interconnection capacity (GW)',tag='') +
  guides(color='none') +
  plot_theme +
  theme(legend.position=c(0.7,0.5))
plot(g_Fig_2_1_1)

g_Fig_2_1_2 <- df_lnkcap_agg %>%
  filter(Flag=='international') %>% 
  group_by(Sv,Y5,Flag) %>% 
  complete(Sc=c('w/','w/o'),fill=list(value=0)) %>% 
  ungroup() %>% 
  # pivot_wider(names_from=Y5,values_from=value,values_fill=0) %>%
  # pivot_longer(cols=-c(Sc,Sv),names_to='Y5',values_to='value',names_transform=as.numeric) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.2,color='grey60') +
  geom_line(aes(x=Y5,y=value/10^3,color=Sv,linetype=Sc),stat='identity',linewidth=0.3) +
  geom_point(aes(x=Y5,y=value/10^3,color=Sv,shape=Sc)) +
  # scale_color_manual(values=c('CHN'='#5ea6f2',
  #                            'JPN'='#1a66b8',
  #                            'CHN-MNG'='#fcca3f',
  #                            'CHN-KOR'='#e6a73c',
  #                            'KOR-JPN'='#b85e1a')) +
  scale_color_manual(values=c('CHN'='#d6982f',
                              'JPN'='#a14d0e',
                              'CHN-MNG'='#5ea6f2',
                              'CHN-KOR'='#1a66b8',
                              'KOR-JPN'='grey20')) +
  scale_shape_manual(values=c(1,4)) +
  scale_x_continuous(n.breaks=3) +
  labs(x='',y='',tag='') +
  plot_theme +
  theme(legend.position='none')
plot(g_Fig_2_1_2)

g_Fig_2 <- ((g_Fig_2_1_1 + g_Fig_2_1_2 + plot_layout(width=c(2,1))) /
                g_legend(g_Fig_2_1_0+guides(color=guide_legend(nrow=2),linetype='none',shape='none'))) +
  plot_layout(heights = c(8,1))
ggsave(filename=paste0(odir,'/Fig2.png'),g_Fig_2,
       width=4.5,height=3.5)

g_Fig_2 <- g_Fig_2_1_1 + g_Fig_2_1_2 + g_legend(g_Fig_2_1_0+guides(color=guide_legend(ncol=1),linetype='none',shape='none')) + plot_layout(width=c(2,1,1.5))
ggsave(filename=paste0(odir,'/Fig2.png'),g_Fig_2,
       width=5,height=2.75)
plot(g_Fig_2)

