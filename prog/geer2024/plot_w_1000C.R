
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

date <- '2403271409'
cdir <- getwd()
ddir <- paste0(cdir,'/data/',date)  # output path
pdir <- paste0(cdir,'/prog/inc_prog')
odir <- paste0(cdir,'/output/',date,'/geer2024/main')
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
  filter(Scenario%in%c('NoPOL','1000C','1000C_Elc','500C','500C_Elc'))

# extract infeasible scenario
scen_inf <- scen_mat_load %>% filter(Status=='Infeasible')
scen_mat <- scen_mat_load %>% filter(Status!='Infeasible') %>% 
  mutate(Scenario=recode(Scenario,
                         '500C_Elc'='500C w/ grid',
                         '500C'='500C w/o grid',
                         '1000C_Elc'='1000C w/ grid',
                         '1000C'='1000C w/o grid'),
         scen_tpol=recode(scen_tpol,
                          'NEAG'='w/ grid',
                          'default'='w/o grid'))

# import AIM/PSY output
df_all_hr <- rgdx.param(paste0(ddir,'/main/hr/',plot_year,'.gdx'),'data_out_hr') %>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc'='500C w/ grid',
                   '500C'='500C w/o grid',
                   '1000C_Elc'='1000C w/ grid',
                   '1000C'='1000C w/o grid')) %>% 
  filter(Sc%in%c('NoPOL','1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid')) %>% 
  filter(Sc%in%scen_mat$Scenario) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>%
  rename(value=data_out_hr)

df_all_yr <- rgdx.param(paste0(ddir,'/main/merged_output.gdx'),'data_all_yr') %>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc'='500C w/ grid',
                   '500C'='500C w/o grid',
                   '1000C_Elc'='1000C w/ grid',
                   '1000C'='1000C w/o grid')) %>% 
  filter(Sc%in%c('NoPOL','1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid')) %>% 
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

g_Fig_1_1 <- df_yr_ele %>%
  filter(Sr%in%c('NEA'),Sc%in%c('NoPOL','1000C w/o grid','500C w/o grid'),
         Sv!='Sec_Ene_Ele_Cur',Sv!='Sec_Ene_Ele_Sto_Los',Sv!='Sec_Ene_Ele_Trd_Los') %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value),
         Sr=recode(Sr,'KR'='KOR','MN'='MNG')) %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         Sc=factor(Sc,levels=c('NoPOL','1000C w/o grid','500C w/o grid')),
         Sr=factor(Sr,levels=c('JPN','CHN','KOR','MNG','NEA'))) %>%
  ggplot() +
  geom_bar(aes(x=Y5,y=value/10^9,fill=Sv),stat='identity',width=0.95*5) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  labs(x='',y='power generation (PWh/yr)',tag='a)') +
  facet_wrap(vars(Sc)) +
  plot_theme +
  theme(legend.position='none',
        plot.tag=element_text(size=10))
plot(g_Fig_1_1)

g_Fig_1_3 <- df_yr_ele %>%
  filter(Sr%in%c('JPN','CHN','KR','MN','NEA'),
         Sc%in%c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid'),
         Sv!='Sec_Ene_Ele_Cur',Sv!='Sec_Ene_Ele_Sto_Los',Sv!='Sec_Ene_Ele_Trd_Los',
         Y5==2050) %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value),
         Sr=recode(Sr,'KR'='KOR','MN'='MNG','NEA'='Total')) %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         Sc=factor(Sc,levels=c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid')),
         Sr=factor(Sr,levels=c('Total','JPN','CHN','KOR','MNG'))) %>%
  ggplot() +
  geom_bar(aes(x=Sc,y=value/10^9,fill=Sv),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  labs(x='',y='power generation in 2050\n(PWh/yr)',tag='b)') +
  facet_wrap(vars(Sr),nrow=1,scales='free') +
  plot_theme +
  theme(legend.position='none')
plot(g_Fig_1_3)

g_Fig_1_4 <- g_legend(g_Fig_1_3+theme(legend.position='right')+
                        guides(fill=guide_legend(nrow=2)))
plot(g_Fig_1_4)

g_Fig_1 <- (((g_Fig_1_1 + g_Fig_1_3 + plot_layout(widths=c(1,1)))) / g_Fig_1_4 + plot_layout(heights = c(4,1)))
plot(g_Fig_1)

# ggsave(filename=paste0(odir,'/Fig1.png'),g_Fig_1,
#        width=10,height=4)
g_Fig_1_4 <- g_legend(g_Fig_1_3+theme(legend.position='right')+
                        guides(fill=guide_legend(ncol=3)))
plot(g_Fig_1_4)

g_Fig_1 <- g_Fig_1_1 + g_Fig_1_3 + g_Fig_1_4 + plot_layout(heights = c(4,4,3))
plot(g_Fig_1)

ggsave(filename=paste0(odir,'/Fig1.png'),g_Fig_1,
       width=7,height=7)

t_1_1 <- df_yr_ele %>%
  filter(Sr%in%c('JPN','CHN','KR','MN','NEA'),
         Sc%in%c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid'),
         Sv!='Sec_Ene_Ele_Cur',Sv!='Sec_Ene_Ele_Sto_Los',Sv!='Sec_Ene_Ele_Trd_Los',
         Y5==2050) %>% 
  group_by(Sc,Sr) %>% 
  mutate(share=value/sum(value)*100)

t_1_2 <- df_yr_ele %>%
  filter(Sr%in%c('JPN','CHN','KR','MN','NEA'),
         Sc%in%c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid'),
         Sv!='Sec_Ene_Ele_Cur',Sv!='Sec_Ene_Ele_Sto_Los',Sv!='Sec_Ene_Ele_Trd_Los',
         Y5==2050) %>% 
  group_by(Sc,Sr) %>% 
  reframe(value=sum(value))


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
                   '500C_Elc'='500C w/ grid',
                   '500C'='500C w/o grid',
                   '1000C_Elc'='1000C w/ grid',
                   '1000C'='1000C w/o grid')) %>% 
  filter(Sc%in%c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid'))

g_Fig_2_1 <- df_lnkcap_agg %>%
  # pivot_wider(names_from=Y5,values_from=value,values_fill=0) %>%
  # pivot_longer(cols=-c(Sc,Sv),names_to='Y5',values_to='value',names_transform=as.numeric) %>% 
  mutate(cpol=case_when(str_detect(Sc,'500C')~'500C',
                        str_detect(Sc,'1000C')~'1000C'),
         tpol=case_when(str_detect(Sc,'w/ grid')~'w/ grid',
                        str_detect(Sc,'w/o grid')~'w/o grid')) %>% 
  ggplot() +
  geom_line(aes(x=Y5,y=value/10^6,color=Sv,linetype=tpol),stat='identity',linewidth=0.3) +
  geom_point(aes(x=Y5,y=value/10^6,color=Sv,shape=tpol)) +
  geom_hline(yintercept=0,linewidth=0.2,color='grey60') +
  # scale_color_manual(values=c('China domestic'='#5ea6f2',
  #                            'Japan domestic'='#1a66b8',
  #                            'China/Mongolia'='#fcca3f',
  #                            'China/Korea'='#e6a73c',
  #                            'Korea/Japan'='#b85e1a')) +
  scale_color_manual(values=c('China domestic'='#d6982f',
                              'Japan domestic'='#a14d0e',
                              'China/Mongolia'='#5ea6f2',
                              'China/Korea'='#1a66b8',
                              'Korea/Japan'='grey20')) +
  scale_shape_manual(values=c(1,4)) +
  facet_wrap(vars(cpol)) +
  labs(x='',y='interconnection capacity (TW)',tag='a)') +
  plot_theme +
  theme(legend.position='right')
plot(g_Fig_2_1)

ggsave(filename=paste0(odir,'/Fig2_1.png'),g_Fig_2_1,
       width=7,height=4)


# storage capacity --------------------------------------------------------

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
  mutate(Sr2=case_when(str_detect(Sr,'JP-')~'JPN',
                       str_detect(Sr,'CN-')~'CHN',
                       str_detect(Sr,'KR')~'KOR',
                       str_detect(Sr,'MN')~'MNG')) %>% 
  mutate(Y5=ceiling(Sy-2001)%/%5*5+2005) %>% 
  group_by(Sc,Sr,Sr2,Sv,Y5) %>% 
  reframe(value=mean(value)) %>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc'='500C w/ grid',
                   '500C'='500C w/o grid',
                   '1000C_Elc'='1000C w/ grid',
                   '1000C'='1000C w/o grid')) %>% 
  filter(Sc%in%c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid'))

df_stocap_total <- df_stocap %>%
  group_by(Sc,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sr='Total',Sr2='Total')

g_Fig_2_2 <- bind_rows(df_stocap,df_stocap_total) %>% 
  filter(Sv%in%c('Cap_Ele_Sto_LiB_Ene','Cap_Ele_Sto_PHS_Ene'),Sc%in%c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid'),
         Y5==2050,Sr2!='Total') %>%
  group_by(Sc,Sr2,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid')),
         Sr2=factor(Sr2,levels=c('JPN','CHN','KOR','MNG','Total'))) %>% 
  mutate(Sv=recode(Sv,'Cap_Ele_Sto_LiB_Ene'='Li-ion battery','Cap_Ele_Sto_PHS_Ene'='pumped hydro')) %>% 
  filter(Sv=='Li-ion battery') %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value/10^6,fill=Sv),fill='#32AC9AFF',stat='identity') +
  labs(x='',y='energy capacity of battery\n(TWh)',tag='b)') +
  # scale_fill_manual(values=c('#32AC9AFF','#7fffd4')) +
  facet_wrap(vars(Sr2),nrow=1,scales='free') +
  plot_theme +
  theme(legend.position = 'bottom')
plot(g_Fig_2_2)

ggsave(filename=paste0(odir,'/Fig_2_2.png'),g_Fig_2_2,
       width=7,height=2.75)

g_Fig_2 <- g_Fig_2_1+g_Fig_2_2 + plot_layout(ncol=1)
plot(g_Fig_2)

t_2 <- g_Fig_2_2 <- bind_rows(df_stocap,df_stocap_total) %>% 
  filter(Sv%in%c('Cap_Ele_Sto_LiB_Ene','Cap_Ele_Sto_PHS_Ene'),Sc%in%c('w/o grid','w/ grid'),
         Y5==2050) %>%
  group_by(Sc,Sr2,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=c('w/o grid','w/ grid')),
         Sr2=factor(Sr2,levels=c('JPN','CHN','KOR','MNG','Total'))) %>% 
  mutate(Sv=recode(Sv,'Cap_Ele_Sto_LiB_Ene'='Li-ion battery','Cap_Ele_Sto_PHS_Ene'='pumped hydro')) %>% 
  filter(Sv=='Li-ion battery')


# electricity import and export -------------------------------------------

Sys.setlocale("LC_TIME", "en_GB.UTF-8")

df_flw_exp <- foreach (i=c('500C_Elc')) %do% {
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
  mutate(Sc=recode(Sc,'500C_Elc'='default w/ grid')) %>% 
  filter(Sc%in%c('default w/ grid')) 

df_flw_imp <- foreach (i=c('500C_Elc')) %do% {
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
  mutate(Sc=recode(Sc,'500C_Elc'='default w/ grid')) %>% 
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

dmd <- foreach (i=c('500C_Elc')) %do% {
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
  mutate(Sc=recode(Sc,'500C_Elc'='default w/ grid'))

# heat map
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
         H=as.numeric(format(T, "%H"))) %>% 
  left_join(df_lnkcap_agg %>% filter(!str_detect(Sv,'domestic'),Y5==2050) %>% transmute(Sv,vfs=value)) %>% 
  mutate(value=value/vfs*100)

g_15 <- df_flw_heat %>% 
  mutate(Sv=str_replace_all(Sv,'/',' to ')) %>% 
  ggplot() +
  geom_tile(aes(x=D,y=H,fill=value)) +
  facet_wrap(vars(Sv),ncol=1) +
  scale_fill_distiller(palette='Spectral') +
  labs(x='day of the year',y='hour of the day (UTC+9)',
       fill='power flow (% of transmission capacity)') +
  scale_x_date(labels=scales::date_format("%b %d")) + 
  scale_y_continuous(breaks=seq(0,24,6)) +
  plot_theme +
  theme(axis.title.x = element_blank(),
        legend.title = element_text(size = 10),
        axis.text.x=element_text(vjust=1)) +
  guides(fill=guide_colorbar(title.position='top',
                             barwidth = 15,
                             barheight =0.5))
plot(g_15)
ggsave(filename=paste0(odir,'/Fig_3.png'),g_15 + theme(legend.position = 'bottom'),width=4.5,height=5)

t_3_1 <- df_flw_annual %>% 
  left_join(dmd) %>% 
  mutate(rate=value/dmd*100,
         value=value/10^6,
         dmd=dmd/10^6)

t_3_2 <- df_flw_heat %>%
  mutate(value=abs(value)) %>% 
  group_by(Sc,Sv) %>% 
  reframe(value=mean(value))

t_3_3 <- df_flw_heat %>% 
  group_by(Sc,Sv) %>% 
  reframe(value=sum(value))



# Japan generation mix ----------------------------------------------------

g_Fig_4_1 <- df_yr_ele %>%
  filter(Sr%in%c('JPN'),Sc%in%c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid'),
         Sv!='Sec_Ene_Ele_Cur',Sv!='Sec_Ene_Ele_Sto_Los',Sv!='Sec_Ene_Ele_Trd_Los') %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value)) %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         Sc=factor(Sc,levels=c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid'))) %>%
  ggplot() +
  geom_area(aes(x=Y5,y=value/10^6,fill=Sv)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  labs(x='',y='power generation (TWh/yr)') +
  facet_wrap(vars(Sc)) +
  plot_theme +
  theme(legend.position='none')
plot(g_Fig_4_1)

g_Fig_4_2 <- df_yr_ele %>%
  filter(Y5==2050,str_detect(Sr,'JP-'),Sc%in%c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid'),
         Sv!='Sec_Ene_Ele_Cur',Sv!='Sec_Ene_Ele_Sto_Los',Sv!='Sec_Ene_Ele_Trd_Los') %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value)) %>% 
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         Sc=factor(Sc,levels=c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid')),
         Sr=factor(Sr,levels=c('JP-HD','JP-TH','JP-KT','JP-CB','JP-HR','JP-KS','JP-CG','JP-SK','JP-KY'))) %>%
  ggplot() +
  geom_bar(aes(x=Sc,y=value/10^6,fill=Sv),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  labs(x='',y='power generation in 2050 (TWh/yr)') +
  facet_wrap(vars(Sr),nrow=1) +
  theme(legend.position='none',
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank()) +
  plot_theme +
  theme(legend.position = 'none')
plot(g_Fig_4_2)

g_Fig_4_l <- g_legend(g_Fig_4_1+theme(legend.position='bottom')+guides(fill=guide_legend(nrow=2))) 

g_Fig_4 <- (g_Fig_4_1 + g_Fig_4_2 + plot_layout(width=c(4,5))) / g_Fig_4_l + plot_layout(heights=c(4,1))
plot(g_Fig_4)

ggsave(filename=paste0(odir,'/Fig_4.png'),g_Fig_4,
       width=10,height=4)

ggsave(filename=paste0(odir,'/Fig_4.png'),g_Fig_4_2+theme(legend.position='bottom')+guides(fill=guide_legend(nrow=5)),
       width=5,height=4.5)


# interconnetion capacity -------------------------------------------------

df_lnkcap_agg <- df_lnkcap %>%
  separate(PW_LNK,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
  filter(str_detect(N1,'JP-')) %>%
  rename(Sv=PW_LNK) %>% 
  mutate(Sv=str_remove_all(Sv,'-AC')) %>% 
  mutate(Sv=str_replace_all(Sv,'_',' / '))

g_Fig_5 <- df_lnkcap_agg %>%
  mutate(Sc=recode(Sc,
                   '500C_Elc'='500C w/ grid',
                   '500C'='500C w/o grid',
                   '1000C_Elc'='1000C w/ grid',
                   '1000C'='1000C w/o grid')) %>% 
  filter(Sc%in%c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid'),
         Y5>=2020) %>% 
  mutate(Sv=recode(Sv,
                   'JP-CB / JP-HR'='Other',
                   'JP-HR / JP-KS'='Other',
                   'JP-KS / JP-SK'='Other',
                   'JP-CG / JP-SK'='Other',
                   'JP-CG / JP-KY'='Other')) %>% 
  group_by(Sc,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(cpol=case_when(str_detect(Sc,'500C')~'500C',
                        str_detect(Sc,'1000C')~'1000C'),
         tpol=case_when(str_detect(Sc,'w/ grid')~'w/ grid',
                        str_detect(Sc,'w/o grid')~'w/o grid')) %>% 
  mutate(Sv=factor(Sv,levels=c('KR / JP-CG','KR / JP-KY',
                               'JP-HD / JP-TH','JP-TH / JP-KT',
                               'JP-KT / JP-CB','JP-CB / JP-KS',
                               'JP-KS / JP-CG','Other'))) %>% 
  ggplot() +
  geom_line(aes(x=Y5,y=value/1000,linetype=tpol),linewidth=0.3) +
  geom_point(aes(x=Y5,y=value/1000,shape=tpol)) +
  labs(x='',y='interconnection capacity (GW)') +
  scale_color_d3() +
  # scale_color_manual(values=c('orchid','#F6CCFFFF','#b85e1a','#fcca3f','#ACBF4C','#5ea6f2','#1a66b8','#0D2570','grey50')) +
  scale_shape_manual(values=c(1,4)) +
  facet_grid(rows=vars(cpol),cols=vars(Sv)) +
  plot_theme +
  theme(legend.position=c(0.07,0.25),
        strip.text.x = element_text(size=9))
plot(g_Fig_5)
ggsave(filename=paste0(odir,'/Fig_5.png'),g_Fig_5+labs(tag='b)'),
       width=8.5,height=2.75)


# daily balance -----------------------------------------------------------

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

g_Fig_6_0 <- df_da_ele %>% 
  filter(Sr=='JPN',Sc%in%c('500C w/o grid','500C w/ grid'),value!=0) %>%
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value),
         D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('500C w/o grid','500C w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
  guides(fill=guide_legend(nrow=3)) +
  facet_wrap(vars(Sc),nrow=1) +
  labs(x='',y='electricity balance (GWh/day)') +
  plot_theme +
  theme(legend.position = 'bottom')

g_Fig_6_1 <- df_da_ele %>% 
  filter(Sr=='JP-KT',Sc%in%c('500C w/o grid','500C w/ grid'),value!=0) %>%
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value),
         D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('500C w/o grid','500C w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
  guides(fill=guide_legend(nrow=3)) +
  facet_wrap(vars(Sc),nrow=1) +
  labs(x='',y='electricity balance (GWh/day)') +
  plot_theme +
  theme(legend.position = 'bottom')
plot(g_Fig_6_1)
ggsave(filename=paste0(odir,'/Fig_6_KT.png'),g_Fig_6_1,width=10,height=4.5)

g_Fig_6_2 <- df_da_ele %>% 
  filter(Sr=='JP-KS',Sc%in%c('500C w/o grid','500C w/ grid'),value!=0) %>%
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value),
         D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('500C w/o grid','500C w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
  guides(fill=guide_legend(nrow=3)) +
  facet_wrap(vars(Sc),nrow=1) +
  labs(x='',y='electricity balance (GWh/day)') +
  plot_theme +
  theme(legend.position = 'bottom')
plot(g_Fig_6_2)
ggsave(filename=paste0(odir,'/Fig_6_KS.png'),g_Fig_6_2,width=10,height=4.5)

g_Fig_6_3 <- df_da_ele %>% 
  filter(Sr=='JP-HD',Sc%in%c('500C w/o grid','500C w/ grid'),value!=0) %>%
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value),
         D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('500C w/o grid','500C w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
  guides(fill=guide_legend(nrow=3)) +
  facet_wrap(vars(Sc),nrow=1) +
  labs(x='',y='electricity balance (GWh/day)') +
  plot_theme +
  theme(legend.position = 'bottom')
plot(g_Fig_6_3)
ggsave(filename=paste0(odir,'/Fig_6_HD.png'),g_Fig_6_3,width=10,height=4.5)

g_Fig_6_4 <- df_da_ele %>% 
  filter(Sr=='JP-TH',Sc%in%c('500C w/o grid','500C w/ grid'),value!=0) %>%
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value),
         D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('500C w/o grid','500C w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
  guides(fill=guide_legend(nrow=3)) +
  facet_wrap(vars(Sc),nrow=1) +
  labs(x='',y='electricity balance (GWh/day)') +
  plot_theme +
  theme(legend.position = 'bottom')
plot(g_Fig_6_4)
ggsave(filename=paste0(odir,'/Fig_6_TH.png'),g_Fig_6_4,width=10,height=4.5)

g_Fig_6_5 <- df_da_ele %>% 
  filter(Sr=='JP-CB',Sc%in%c('500C w/o grid','500C w/ grid'),value!=0) %>%
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value),
         D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('500C w/o grid','500C w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
  guides(fill=guide_legend(nrow=3)) +
  facet_wrap(vars(Sc),nrow=1) +
  labs(x='',y='electricity balance(GWh/day)') +
  plot_theme +
  theme(legend.position = 'bottom')
plot(g_Fig_6_5)
ggsave(filename=paste0(odir,'/Fig_6_CB.png'),g_Fig_6_5,width=10,height=4.5)

g_Fig_6 <- (g_Fig_6_0+theme(legend.position='none',axis.text.x=element_blank())+labs(y='electricity balance\n(GWh/day)',tag='a)')) + 
  (g_Fig_6_2+labs(y='electricity balance\n(GWh/day)',tag='b)')) + plot_layout(ncol=1)
plot(g_Fig_6)
ggsave(filename=paste0(odir,'/Fig_6.png'),g_Fig_6_0+labs(y='electricity balance\n(GWh/day)')+labs(tag='a)'),width=9,height=4)


# marginal electricity price ----------------------------------------------

df_mcoe <- foreach (i=c('1000C','1000C_Elc','500C','500C_Elc')) %do% {
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
                   '500C_Elc'='500C w/ grid',
                   '500C'='500C w/o grid',
                   '1000C_Elc'='1000C w/ grid',
                   '1000C'='1000C w/o grid'))

df_mcoe_ann <- df_mcoe %>% 
  mutate(Sr=case_when(str_detect(N,'JP-')~'JPN',
                      str_detect(N,'CN-')~'CHN',
                      N=='KR'~'KOR',
                      N=='MN'~'MNG')) %>% 
  group_by(Sr,Sc,I) %>% 
  reframe(value=sum(dmd*eq_npb_m)/sum(dmd))%>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc'='500C w/ grid',
                   '500C'='500C w/o grid',
                   '1000C_Elc'='1000C w/ grid',
                   '1000C'='1000C w/o grid'))

df_mcoe_ann_total <- df_mcoe %>% 
  group_by(Sc,I) %>% 
  reframe(value=sum(dmd*eq_npb_m)/sum(dmd))%>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc'='500C w/ grid',
                   '500C'='500C w/o grid',
                   '1000C_Elc'='1000C w/ grid',
                   '1000C'='1000C w/o grid')) %>% 
  mutate(Sr='Total')

scen_cat <- scen_mat %>% 
  transmute(Sc=Scenario,cat1=scen_dpol,cat2=scen_tpol)

# g_4_19_1 <- df_mcoe_JPN %>%
#   filter(I=='AC') %>% 
#   group_by(Sc,I) %>% 
#   arrange(-value) %>% mutate(T=row_number()) %>% 
#   left_join(scen_cat) %>% 
#   ggplot() +
#   geom_step(aes(x=T,y=value/1000,color=cat1,linetype=cat2)) +
#   scale_color_d3() +
#   scale_x_continuous(breaks=c(1,seq(1000,8000,1000),8760)) +
#   scale_linetype_manual(values=c('w/o grid'='solid','w/ grid'='dashed')) +
#   labs(x='',y='Marginal cost of electricity (USD/kWh)',tag='a)') +
#   plot_theme +
#   theme(legend.position='none')

g_Fig_7_1 <- bind_rows(df_mcoe_ann,df_mcoe_ann_total) %>% 
  filter(I=='AC') %>% 
  mutate(cpol=case_when(str_detect(Sc,'500C')~'500C',
                        str_detect(Sc,'1000C')~'1000C'),
         tpol=case_when(str_detect(Sc,'w/ grid')~'w/ grid',
                        str_detect(Sc,'w/o grid')~'w/o grid')) %>% 
  ggplot() +
  geom_point(aes(x=Sr,y=value,shape=tpol)) +
  labs(y='Averagearginal cost of electricity (USD/kWh)') +
  scale_shape_manual(values=c(1,4)) +
  scale_y_continuous(limits=c(0,NA)) +
  labs(x='',y='Annual average of MCOE (USD/kWh)',tag='a)') +
  facet_wrap(vars(cpol)) +
  plot_theme +
  theme(legend.position=c(0.75,0.25),
        legend.text=element_text(size=10))
plot(g_Fig_7_1)

g_Fig_7_2 <- df_mcoe_JPN %>%
  group_by(Sc,I) %>% 
  filter(I=='AC') %>% 
  mutate(cpol=case_when(str_detect(Sc,'500C')~'500C',
                        str_detect(Sc,'1000C')~'1000C'),
         tpol=case_when(str_detect(Sc,'w/ grid')~'w/ grid',
                        str_detect(Sc,'w/o grid')~'w/o grid')) %>% 
  arrange(-value) %>% mutate(T=row_number()) %>% 
  ggplot() +
  geom_step(aes(x=T,y=value,linetype=tpol),linewidth=0.3) +
  scale_color_d3() +
  labs(y='Marginal cost of electricity (USD/kWh)') +
  scale_x_continuous(breaks=c(1,seq(1000,8000,1000),8760)) +
  scale_y_continuous(limits=c(0,500))  +
  scale_linetype_manual(values=c('w/o grid'='solid','w/ grid'='dashed')) +
  facet_wrap(vars(cpol)) +
  labs(x='',y='MCOE (USD/kWh)',tag='b)') +
  plot_theme +
  theme(legend.position=c(0.7,0.8),
        legend.text=element_text(size=10))
plot(g_Fig_7_2)

g_Fig_7_1_2 <- g_Fig_7_1 + g_Fig_7_2 + plot_layout(ncol=1)
plot(g_Fig_7_1_2)


# system cost -------------------------------------------------------------

gen_inv_map <- tribble(~R,~Sv,
                       'PVC','CAPEX (solar)',
                       'PVDC','CAPEX (solar)',
                       'PVDR','CAPEX (solar)',
                       'WNO','CAPEX (wind)',
                       'WNB','CAPEX (wind)',
                       'WNF','CAPEX (wind)',
                       'GEO','CAPEX (other)',
                       'BMS','CAPEX (other)',
                       'BMS_CCS','CAPEX (other)',
                       'PC','CAPEX (fossil w/o CCS)',
                       'SC','CAPEX (fossil w/o CCS)',
                       'IGCC','CAPEX (fossil w/o CCS)',
                       'ST','CAPEX (fossil w/o CCS)',
                       'OCGT','CAPEX (fossil w/o CCS)',
                       'CCGT','CAPEX (fossil w/o CCS)',
                       'CCGT_CCS','CAPEX (fossil w/ CCS)',
                       'OIL','CAPEX (fossil w/o CCS)')

df_gensto_sys <- foreach (i=c('1000C','1000C_Elc','500C','500C_Elc','NoPOL')) %do% {
  gen_inv <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_gen_inv2') %>% 
    filter(SE0=='ALL') %>% select(-SE0) %>% 
    left_join(gen_inv_map) %>% 
    group_by(N,Sv,YEAR) %>% 
    reframe(value=sum(prm2sec_gen_inv2))
  
  sto_inv <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_sto_inv2') %>% 
    filter(SE0=='ALL') %>% select(-SE0) %>% 
    mutate(Sv='CAPEX (battery)') %>% 
    group_by(N,Sv,YEAR) %>% 
    reframe(value=sum(prm2sec_sto_inv2))
  
  gen_fom <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_gen_fom2') %>% 
    filter(SE0=='ALL') %>% select(-SE0) %>% 
    mutate(Sv='OPEX') %>% 
    group_by(N,Sv,YEAR) %>% 
    reframe(value=sum(prm2sec_gen_fom2))
  
  sto_fom <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_sto_fom2') %>% 
    filter(SE0=='ALL') %>% select(-SE0) %>% 
    mutate(Sv='OPEX') %>% 
    group_by(N,Sv,YEAR) %>% 
    reframe(value=sum(prm2sec_sto_fom2))
  
  gen_vom <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_gen_vom2') %>% 
    filter(SE0=='ALL') %>% select(-SE0) %>% 
    mutate(Sv='Fuel') %>% 
    group_by(N,Sv,YEAR) %>% 
    reframe(value=sum(prm2sec_gen_vom2))
  
  tmp <- bind_rows(gen_inv,sto_inv,gen_fom,sto_fom,gen_vom) %>% 
    mutate(Sc=i)
  
} %>% bind_rows()


df_lnk_sys <- foreach (i=c('1000C','1000C_Elc','500C','500C_Elc','NoPOL')) %do% {
  lnk_inv <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_lnk_inv2') %>% 
    separate(L,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
    mutate(N0=str_remove_all(N0,'-AC$|-DC$|-H2$|-PHS$|-AC2$'),
           N1=str_remove_all(N1,'-AC$|-DC$|-H2$|-PHS$|-AC2$'),
           Sv=case_when(str_detect(L,'.*-AC_.*-AC$')&str_sub(N0,end=2)==str_sub(N1,end=2)~'CAPEX (domestic interconnection)',
                        str_detect(L,'.*-AC_.*-AC$')&str_sub(N0,end=2)!=str_sub(N1,end=2)~'CAPEX (international interconnection)',
                        str_detect(L,'.*-AC_.*-DC$')~'CAPEX (battery)',
                        str_detect(L,'.*-AC_.*-H2$')~'CAPEX (hydrogen)',
                        str_detect(L,'.*-AC_.*-AC2$')~'CAPEX (other)',
                        str_detect(L,'.*-AC_.*-PHS$')~'CAPEX (other)'))
  
  lnk_fom <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_lnk_fom2') %>% 
    separate(L,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
    mutate(N0=str_remove_all(N0,'-AC$|-DC$|-H2$|-PHS$|-AC2$'),
           N1=str_remove_all(N1,'-AC$|-DC$|-H2$|-PHS$|-AC2$')) %>% 
    mutate(Sv='OPEX')
  
  lnk_vom <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_lnk_vom2') %>% 
    separate(L,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
    mutate(N0=str_remove_all(N0,'-AC$|-DC$|-H2$|-PHS$|-AC2$'),
           N1=str_remove_all(N1,'-AC$|-DC$|-H2$|-PHS$|-AC2$')) %>% 
    mutate(Sv='Fuel')
  
  tmp <- bind_rows(lnk_inv %>% filter(str_sub(N0,end=2)==str_sub(N1,end=2)) %>% group_by(N0,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_inv2)) %>% rename(N=N0),
                   lnk_fom %>% filter(str_sub(N0,end=2)==str_sub(N1,end=2)) %>% group_by(N0,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_fom2)) %>% rename(N=N0),
                   lnk_vom %>% filter(str_sub(N0,end=2)==str_sub(N1,end=2)) %>% group_by(N0,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_vom2)) %>% rename(N=N0),
                   lnk_inv %>% filter(str_sub(N0,end=2)!=str_sub(N1,end=2)) %>% group_by(N0,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_inv2)/2) %>% rename(N=N0),
                   lnk_fom %>% filter(str_sub(N0,end=2)!=str_sub(N1,end=2)) %>% group_by(N0,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_fom2)/2) %>% rename(N=N0),
                   lnk_vom %>% filter(str_sub(N0,end=2)!=str_sub(N1,end=2)) %>% group_by(N0,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_vom2)/2) %>% rename(N=N0),
                   lnk_inv %>% filter(str_sub(N0,end=2)!=str_sub(N1,end=2)) %>% group_by(N1,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_inv2)/2) %>% rename(N=N1),
                   lnk_fom %>% filter(str_sub(N0,end=2)!=str_sub(N1,end=2)) %>% group_by(N1,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_fom2)/2) %>% rename(N=N1),
                   lnk_vom %>% filter(str_sub(N0,end=2)!=str_sub(N1,end=2)) %>% group_by(N1,Sv,YEAR) %>% reframe(value=sum(prm2sec_lnk_vom2)/2) %>% rename(N=N1)) %>% 
    mutate(Sc=i)
} %>% bind_rows()

df_sys <- bind_rows(df_gensto_sys,df_lnk_sys) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% mutate(YEAR=as.numeric(YEAR)) %>% 
  mutate(Sr2=case_when(str_detect(N,'JP-')~'JPN',
                       str_detect(N,'CN-')~'CHN',
                       str_detect(N,'KR')~'KOR',
                       str_detect(N,'MN')~'MNG')) %>% 
  filter(YEAR>=2020) %>% 
  group_by(Sr2,Sc,Sv,YEAR) %>%
  reframe(value=sum(value)) %>% 
  group_by(Sr2,Sc,Sv) %>% 
  complete(YEAR=seq(2020,2050,5)) %>% 
  replace_na(list(value=0)) %>% 
  complete(YEAR=2020:2050) %>% 
  mutate(value=na_locf(value,option='nocb')) %>% 
  pivot_wider(names_from=Sc,values_from=value,values_fill=0) %>% 
  mutate(across(c(`500C`,`500C_Elc`,`1000C`,`1000C_Elc`),~.-`NoPOL`)) %>%
  pivot_longer(cols=-c(Sv,Sr2,YEAR),names_to='Sc',values_to='value') %>% 
  mutate(discount=(1-0.05)^(YEAR-2020)) %>% 
  filter(Sc!='NoPOL') %>%
  group_by(Sc,Sv,Sr2) %>% 
  reframe(value=sum(value*discount)) %>% 
  mutate(Sc=recode(Sc,
                   '500C_Elc'='500C w/ grid',
                   '500C'='500C w/o grid',
                   '1000C_Elc'='1000C w/ grid',
                   '1000C'='1000C w/o grid')) %>% 
  filter(Sc%in%c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid'))


df_sys_nea <- df_sys %>%
  group_by(Sc,Sv) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sr2='Total')

df_sys_total <- bind_rows(df_sys,df_sys_nea) %>%
  group_by(Sc,Sr2) %>% 
  reframe(value=sum(value))

df_GDP_country <- read_csv(paste0(ddir,'/define/GDP_SSP2_v3.csv')) %>% 
  mutate(Sr2=recode(Region,'China'='CHN','Japan'='JPN','South Korea'='KOR','Mongolia'='MNG')) %>% 
  select(-Model,-Scenario,-Region,-Variable,-Unit) %>% 
  pivot_longer(cols=-c(Sr2),names_to='YEAR',values_to='GDP',names_transform=as.numeric) %>% 
  group_by(Sr2) %>% 
  complete(YEAR=2020:2050) %>% 
  mutate(GDP=na_interpolation(GDP)) %>% 
  mutate(discount=(1-0.05)^(YEAR-2020)) %>% 
  group_by(Sr2) %>% 
  reframe(GDP=sum(GDP*discount)*10^9*91.9/102.9)

df_GDP_nea <- df_GDP_country %>% 
  reframe(GDP=sum(GDP)) %>% 
  mutate(Sr2='Total')

df_GDP <- bind_rows(df_GDP_country,df_GDP_nea)

plt <- set_plot(ele_inv2)

# g_Fig_7 <- bind_rows(df_sys,df_sys_nea) %>%
#   left_join(df_GDP) %>%
#   mutate(value=value/GDP*100,
#          Sv=case_when(str_detect(Sv,'fossil')~'CAPEX (fossil)',
#                       str_detect(Sv,'solar|wind')~'CAPEX (solar/wind)',
#                       TRUE~Sv)) %>%
#   mutate(Sv=factor(Sv,levels=rev(ele_inv2$Variable)),
#          Sc=factor(Sc,levels=c('w/o grid','w/ grid')),
#          Sr2=factor(Sr2,levels=c('JPN','CHN','KOR','MNG','Total'))) %>%
#   ggplot() +
#   geom_bar(aes(x=Sc,y=value,fill=Sv),stat='identity') +
#   geom_point(data=df_sys_total %>%
#                left_join(df_GDP) %>%
#                mutate(value=value/GDP*100,
#                       Sr2=factor(Sr2,levels=c('JPN','CHN','KOR','MNG','Total'))),
#              aes(x=Sc,y=value,fill=Sv),fill='white',color='black',shape=23,size=1.5) +
#   scale_fill_manual(values=plt$Color,labels=plt$Legend)+
#   facet_wrap(vars(Sr2),scales='free',nrow=1) +
#   labs(x='',y='Cumulative additional system cost (% of GDP)') +
#   plot_theme +
#   theme(legend.position = 'bottom')
# plot(g_Fig_7)
# ggsave(filename=paste0(odir,'/Fig_7.png'),g_Fig_7,width=4,height=5.5)

# g_Fig_7_3 <- df_sys_total %>%
#     left_join(df_GDP) %>%
#     mutate(value=value/GDP*100,
#   Sc=factor(Sc,levels=c('w/ grid','w/o grid')),
#            Sr2=factor(Sr2,levels=c('JPN','CHN','KOR','MNG','Total'))) %>%
#     ggplot() +
#   geom_point(aes(x=Sr2,y=value,shape=Sc)) +
#   scale_shape_manual(values=c(1,4)) +
#   labs(x='',y='Cumulative additional system cost\n(% of GDP)') +
#   plot_theme +
#   theme(legend.position = c(0.3,0.7),
#         legend.text=element_text(size=10))
# plot(g_Fig_7_3)

df <- df_sys_total %>% 
  left_join(df_GDP) %>%
  filter(Sr2=='Total') %>% 
  mutate(value2=value/GDP*100,
         Sc=factor(Sc,levels=c('1000C w/o grid','1000C w/ grid','500C w/o grid','500C w/ grid'))) 

g_Fig_7_3 <- df %>%
  ggplot() +
  geom_bar(aes(x=Sc,y=value/10^12),color='grey30',stat='identity') +
  scale_shape_manual(values=c(1,4)) +
  scale_y_continuous(name='Cumulative additional system cost\n(trillion USD)',
                     sec.axis=sec_axis(~.*10^12/df$GDP*100,name='Cumulative additional system cost\n(% of GDP)')) +
  plot_theme +
  theme(legend.position = c(0.3,0.7),
        legend.text=element_text(size=10)) +
  labs(x='',tag='c)')
plot(g_Fig_7_3)

g_Fig_7 <- g_Fig_7_1 + g_Fig_7_2 + g_Fig_7_3 + plot_layout(nrow=1,widths=c(2,4,1))
plot(g_Fig_7)

ggsave(filename=paste0(odir,'/Fig_7.png'),g_Fig_7,width=9,height=3.5)

check <- df_da_ele %>% 
  filter(Sv%in%c('Sec_Ene_Ele_Sto_Cha_Gro_PHS'))

t_7 <- df


# input data --------------------------------------------------------------


dmda_t <- foreach (i=c('1000C','1000C_Elc','500C','500C_Elc','NoPOL')) %do% {
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
  filter(I%in%c('AC2','H2')) %>% 
  mutate(I=recode(I,
                  'AC2'='Electricity',
                  'H2'='Hydrogen')) %>% 
  group_by(I,Y,Sc) %>%
  reframe(value=sum(dmda_t)) %>% 
  filter(Sc%in%c('NoPOL','500C','1000C')) %>%
  group_by(I) %>% 
  complete(Sc=c('NoPOL','500C','1000C'),Y=seq(2005,2050,5),fill=list(value=0)) %>% 
  ggplot() +
  geom_line(aes(x=Y,y=value/10^9,color=I,linetype=Sc)) +
  geom_point(aes(x=Y,y=value/10^9,shape=I,color=I),fill='white') +
  scale_color_d3() +
  scale_y_continuous(limits=c(0,NA)) +
  scale_shape_manual(values=c(21,25)) +
  labs(x='',y='energy demand (PWh/yr)') +
  plot_theme +
  theme(legend.position='none',plot.tag=element_text(size=10)) +
  labs(tag='b)')
plot(g_3_1)

qmax_t <- foreach (i=c('500C','1000C')) %do% {
  qmax_t <- rgdx.param(paste0(ddir,'/input/',i,'.gdx'),'qmax_t') %>% 
    mutate(Sc=i)
} %>% bind_rows() %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Y=as.numeric(Y))

g_3_2 <- qmax_t %>% 
  group_by(Y,Sc) %>% 
  reframe(qmax_t=sum(qmax_t)) %>% 
  ggplot() +
  geom_line(aes(x=Y,y=qmax_t/10^9,linetype=Sc)) +
  scale_color_d3() +
  scale_y_continuous(limits=c(0,NA)) +
  labs(x='',y=expression(paste('CO'[2],' emission (GtCO'[2],'/yr)'))) +
  plot_theme +
  theme(legend.position='right',plot.tag=element_text(size=10)) +
  labs(tag='a)')
plot(g_3_2)

g_3_l <- g_legend(g_3_1+theme(legend.position='bottom'))

g_3 <- (g_3_2 + g_3_1) / g_3_l
plot(g_3)
ggsave(filename=paste0(odir,'/Fig_input.png'),g_3,width=4.5,height=4)
