
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

date <- '2403211424'
cdir <- getwd()
ddir <- paste0(cdir,'/data/',date)  # output path
pdir <- paste0(cdir,'/prog/inc_prog')
odir <- paste0(cdir,'/output/',date,'/geer2024/precheck')
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
  filter(Scenario%in%c('NoPOL','500C','500C_Elc','1000C','1000C_Elc'))

# extract infeasible scenario
scen_inf <- scen_mat_load %>% filter(Status=='Infeasible')
scen_mat <- scen_mat_load %>% filter(Status!='Infeasible') %>% 
  mutate(Scenario=recode(Scenario,
                         '1000C_Elc'='1000C w/ grid',
                         '1000C'='1000C',
                         '500C_Elc'='500C w/ grid',
                         '500C'='500C'),
         scen_tpol=recode(scen_tpol,
                          'NEAG'='w/ grid',
                          'default'='w/o grid'))

# import AIM/PSY output
df_all_hr <- rgdx.param(paste0(ddir,'/main/hr/',plot_year,'.gdx'),'data_out_hr') %>% 
  mutate(Sc=recode(Sc,
                   '1000C_Elc'='1000C w/ grid',
                   '1000C'='1000C',
                   '500C_Elc'='500C w/ grid',
                   '500C'='500C')) %>% 
  filter(Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid')) %>% 
  filter(Sc%in%scen_mat$Scenario) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>%
  rename(value=data_out_hr)

df_all_yr <- rgdx.param(paste0(ddir,'/main/merged_output.gdx'),'data_all_yr') %>% 
  mutate(Sc=recode(Sc,
                   '1000C_Elc'='1000C w/ grid',
                   '1000C'='1000C',
                   '500C_Elc'='500C w/ grid',
                   '500C'='500C')) %>% 
  filter(Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid','NoPOL')) %>% 
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
# plot --Japan
###########################################################################

# Generation mix ----------------------------------------------------------

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

# Japan
g_1 <- df_yr_ele %>%
  filter(Sr%in%c('JPN'),Sc%in%c('NoPOL','1000C','1000C w/ grid','500C','500C w/ grid')) %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value)) %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         Sc=factor(Sc,levels=c('NoPOL','1000C','1000C w/ grid','500C','500C w/ grid'))) %>%
  ggplot() +
  geom_area(aes(x=Y5,y=value/10^6,fill=Sv)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_y_continuous(limits=c(NA,2400))+
  labs(x='',y='power generation (TWh/yr)') +
  facet_wrap(vars(Sc)) +
  plot_theme +
  theme(legend.position='right')

g_2 <- df_yr_ele %>%
  filter(Sr%in%c('JPN'),Sc%in%c('NoPOL','1000C','1000C w/ grid','500C','500C w/ grid')) %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value)) %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         Sc=factor(Sc,levels=c('NoPOL','1000C','1000C w/ grid','500C','500C w/ grid'))) %>%
  ggplot() +
  geom_area(aes(x=Y5,y=value/10^6,fill=Sv)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_y_continuous(limits=c(NA,2400))+
  labs(x='',y='power generation (TWh/yr)') +
  facet_wrap(vars(Sc)) +
  plot_theme +
  theme(legend.position='right')

t1 <- df_yr_ele %>%
  filter(Sr%in%c('JPN'),Sc%in%c('NoPOL','1000C','1000C w/ grid','500C','500C w/ grid'),
         Sv!='Sec_Ene_Ele_Cur',Sv!='Sec_Ene_Ele_Sto_Los',Sv!='Sec_Ene_Ele_Trd_Los') %>% 
  group_by(Sc,Sr,Y5) %>% 
  mutate(value=value/sum(value)*100) %>% 
  pivot_wider(names_from=Sv,values_from=value,values_fill=0) %>% 
  mutate(Sec_Ene_Ele_WinSol=Sec_Ene_Ele_Solar_Cen+Sec_Ene_Ele_Solar_Dec+
           Sec_Ene_Ele_Win_Ons+Sec_Ene_Ele_Win_Off)

ggsave(filename=paste0(odir,'/Check_1.png'),g_1,
       width=8,heigh=4.5)
ggsave(filename=paste0(odir,'/Check_2.png'),g_2,
       width=8,heigh=4.5)


g_3 <- df_yr_ele %>%
  filter(Y5==2050,str_detect(Sr,'JP-'),Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid')) %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value)) %>% 
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         Sc=factor(Sc,levels=c('1000C','1000C w/ grid','500C','500C w/ grid')),
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
  theme(legend.position = 'right')
plot(g_3)

ggsave(filename=paste0(odir,'/Check_3.png'),g_3,
       width=8,height=4.5)

g_4 <- df_all_hr %>% 
  filter(Sv=='Trd_Sec_Ene_Ele',value!=0,str_detect(Sr,'JP-'),Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid')) %>% 
  mutate(Sv=case_when(value>=0~'import',
                      value<0~'export'),
         Sr=factor(Sr,levels=c('JP-HD','JP-TH','JP-KT','JP-CB','JP-HR','JP-KS','JP-CG','JP-SK','JP-KY'))) %>% 
  group_by(Sc,Sv,Sr) %>% 
  summarise(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=c('1000C','1000C w/ grid','500C','500C w/ grid'))) %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value/10^6,fill=Sv),stat='identity') +
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
plot(g_4)

ggsave(filename=paste0(odir,'/Check_4.png'),g_4,
       width=7,heigh=3.5)


# interconnection ---------------------------------------------------------

df_flw <- foreach (i=c('500C','500C_Elc')) %do% {
  tmp0 <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_flw_snd') %>%
    filter(YEAR==plot_year,str_detect(N0,'JP-')&str_detect(N1,'JP-')) %>%
    mutate(across(where(is.factor),~as.character(.))) %>%
    select(-I0,-I1) %>%
    rename(St=T,Sy=YEAR) %>%
    filter(Sy==plot_year,str_detect(L,'.*-AC_.*-AC$')) %>%
    filter(N0==str_match(L,'(.*?)-AC_(.*?)-AC')[,2])
  tmp1 <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_flw_snd') %>%
    filter(YEAR==plot_year,str_detect(N0,'JP-')&str_detect(N1,'JP-')) %>%
    mutate(across(where(is.factor),~as.character(.))) %>%
    select(-I0,-I1) %>%
    rename(St=T,Sy=YEAR) %>%
    filter(Sy==plot_year,str_detect(L,'.*-AC_.*-AC$')) %>%
    filter(N1==str_match(L,'(.*?)-AC_(.*?)-AC')[,2]) %>%
    mutate(prm2sec_flw_snd=-prm2sec_flw_snd)
  tmp <- bind_rows(tmp0,tmp1) %>%
    group_by(N0,N1,Sy) %>%
    reframe(value=sum(prm2sec_flw_snd)) %>%
    transmute(Sc=i,N0,N1,Sy,value)
  summary <- bind_rows(tmp %>% filter(value>0),
                       tmp %>% filter(value<0) %>% transmute(Sc,tmp=N0,N0=N1,N1=tmp,Sy,value) %>% select(-tmp))
} %>% bind_rows() %>%
  left_join(geometry_NEA %>% select(Sr,centroid_x,centroid_y),by=c(N0='Sr')) %>% select(-geometry) %>% rename(centroid_x0=centroid_x,centroid_y0=centroid_y) %>%
  left_join(geometry_NEA %>% select(Sr,centroid_x,centroid_y),by=c(N1='Sr')) %>% select(-geometry) %>% rename(centroid_x1=centroid_x,centroid_y1=centroid_y) %>%
  mutate(Sc=recode(Sc,'500C'='default',
                   '500C'='default w/ grid')) %>%
  filter(Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid')) %>%
  mutate(Sc=factor(Sc,levels=scen_mat$Scenario)) %>%
  mutate(cat1=scen_mat$scen_dpol[Sc], cat2=scen_mat$scen_tpol[Sc]) %>%
  mutate(cat1=factor(cat1, levels=unique(scen_mat$scen_dpol))) %>%
  mutate(cat2=factor(cat2, levels=unique(scen_mat$scen_tpol)))

df_enebal <- df_all_yr %>%
  filter(Sv%in%c('Trd_Sec_Ene_Ele'),str_detect(Sr,'JP-'),Y5==plot_year,Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid')) %>%
  left_join(geometry_NEA %>% select(Sr,geometry)) %>%
  mutate(Sc=factor(Sc,levels=scen_mat$Scenario)) %>%
  mutate(cat1=scen_mat$scen_dpol[Sc], cat2=scen_mat$scen_tpol[Sc]) %>%
  mutate(cat1=factor(cat1, levels=unique(scen_mat$scen_dpol))) %>%
  mutate(cat2=factor(cat2, levels=unique(scen_mat$scen_tpol)))

df_flw_tmp <- df_flw %>%
  group_by(Sc,N0,N1,centroid_x0,centroid_y0,centroid_x1,centroid_y1) %>%
  reframe(value=sum(value))

df_flw_summary <- bind_rows(df_flw_tmp %>% filter(value>0),
                            df_flw_tmp %>% filter(value<0) %>% mutate(tmp=N0,N0=N1,N1=tmp,
                                                                      tmp=centroid_x0,centroid_x0=centroid_x1,centroid_x1=tmp,
                                                                      tmp=centroid_y0,centroid_y0=centroid_y1,centroid_y1=tmp))

gsmx <- rgdx.param(paste0(ddir,'/gams_output/gdx_primary/500C/2050.gdx'),'gsmx') %>% 
  filter(R%in%c('PVC','PVDC','PVDR','WNO','WNB','WNF')) 
vgs_l1 <- rgdx.param(paste0(ddir,'/gams_output/gdx_primary/500C/2050.gdx'),'vgs_l') %>% 
  filter(R%in%c('PVC','PVDC','PVDR','WNO','WNB','WNF')) %>% 
  right_join(gsmx) %>% 
  replace_na(list(vgs_l=0)) %>% 
  filter(str_detect(N,'JP-')) %>% 
  mutate(Sv='installed in default')
vgs_l2 <- rgdx.param(paste0(ddir,'/gams_output/gdx_primary/500C_Elc/2050.gdx'),'vgs_l') %>% 
  filter(R%in%c('PVC','PVDC','PVDR','WNO','WNB','WNF')) %>% 
  right_join(gsmx) %>% 
  replace_na(list(vgs_l=0)) %>% 
  filter(str_detect(N,'JP-')) %>% 
  mutate(Sv='installed in default w/ grid')
g_5 <- bind_rows(vgs_l1,vgs_l2) %>% 
  select(-gsmx) %>% 
  rename(value=vgs_l) %>% 
  bind_rows(gsmx %>% mutate(Sv='maximum installable capacity') %>% rename(value=gsmx)) %>% 
  mutate(R=recode(R,
                  PVC='solar utility',PVDC='solar rooftop, commercial',PVDR='solar rooftop, residential',
                  WNO='wind onshore',WNB='wind offshore, fixed bottom',WNF='wind offshore, floating')) %>% 
  group_by(N,Sv,R) %>% 
  reframe(value=sum(value)) %>% 
  filter(str_detect(N,'JP-')) %>% 
  mutate(N=factor(N,levels=c('JP-HD','JP-TH','JP-KT','JP-CB','JP-HR','JP-KS','JP-CG','JP-SK','JP-KY'))) %>% 
  ggplot() +
  geom_jitter(aes(x=N,y=value/1000,color=Sv,shape=Sv),height=0,width=0.1) +
  labs(x='',y='renewables generator capacity (GW)') +
  scale_color_manual(values=c("#1F77B4FF","#FF7F0EFF","black")) +
  scale_shape_manual(values=c(1,1,4)) +
  plot_theme +
  facet_wrap(vars(R),scales='free',nrow=2) +
  theme(legend.position = 'bottom')
plot(g_5)
ggsave(filename=paste0(odir,'/Check_5.png'),g_5,
       width=8,height=6)


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
  filter(str_detect(N,'JP-')) %>% 
  drop_na(Sv,YEAR) %>% 
  transmute(Sc,Sr=N,Sv,Sy=YEAR,value=prm2sec_gen_cap)

df_lnkcap2 <- foreach (i=scen_mat_load$Scenario) %do% {
  tmp <- rgdx.param(paste0(ddir,'/gams_output/gdx_secondary/prm2sec/',i,'.gdx'),'prm2sec_lnk_cap') %>% 
    mutate(Sc=i)
} %>% bind_rows() %>% 
  filter(!str_detect(PW_LNK,'.*-AC_.*-AC$')) %>% 
  separate(PW_LNK,sep='_',into=c('N0','N1'),remove=FALSE) %>% 
  filter(str_detect(N0,'JP-')) %>% 
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
  filter(str_detect(N,'JP-')) %>% 
  drop_na(Variable) %>% 
  transmute(Sc,Sr=N,Sv=Variable,Sy=YEAR,value=prm2sec_sto_cap)

df_techcap <- bind_rows(df_gencap,df_lnkcap2,df_stocap) %>% 
  mutate(Sy=as.numeric(as.character(Sy))) %>% 
  arrange(Sy) %>% 
  mutate(Sr2=case_when(str_detect(Sr,'JP-')~'JPN')) %>% 
  mutate(Y5=ceiling(Sy-2001)%/%5*5+2005) %>% 
  group_by(Sc,Sr,Sr2,Sv,Y5) %>% 
  reframe(value=mean(value)) %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid'))

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
  filter(str_detect(N1,'JP-')) %>%
  rename(Sv=PW_LNK) %>% 
  mutate(Sv=str_remove_all(Sv,'-AC')) %>% 
  mutate(Sv=str_replace_all(Sv,'_',' / '))

g_6_1 <- df_lnkcap_agg %>%
  mutate(Sc=recode(Sc,
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid'),!str_detect(Sv,'KR'),Y5>=2020) %>% 
  mutate(Sv=factor(Sv,levels=c('JP-HD / JP-TH','JP-TH / JP-KT',
                               'JP-KT / JP-CB','JP-CB / JP-HR','JP-CB / JP-KS','JP-HR / JP-KS',
                               'JP-KS / JP-CG','JP-KS / JP-SK','JP-CG / JP-SK','JP-CG / JP-KY'))) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value/1000,fill=Sv),stat='identity') +
  labs(x='',y='interconnection capacity (GW)',tag='a)') +
  scale_fill_simpsons()+
  facet_wrap(vars(Sc)) +
  plot_theme +
  theme(legend.position='none')
plot(g_6_1)

g_6_2 <- df_lnkcap_agg %>%
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid'),!str_detect(Sv,'KR'),Y5>=2020) %>% 
  pivot_wider(names_from=Sc,values_from=value) %>% 
  transmute(Sv,Y5,value=`default w/ grid`-default) %>% 
  mutate(Sv=factor(Sv,levels=c('JP-HD / JP-TH','JP-TH / JP-KT',
                               'JP-KT / JP-CB','JP-CB / JP-HR','JP-CB / JP-KS','JP-HR / JP-KS',
                               'JP-KS / JP-CG','JP-KS / JP-SK','JP-CG / JP-SK','JP-CG / JP-KY'))) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value/1000,fill=Sv),stat='identity') +
  labs(x='',y='difference between demand+ and default\n(GW)',tag='b)') +
  scale_fill_simpsons()+
  plot_theme
plot(g_6_2)

g_6 <- g_6_1 + g_6_2 + plot_layout(width=c(2,1))

ggsave(filename=paste0(odir,'/Check_6.png'),g_6,
       width=8,height=4)

g_s3 <- df_lnkcap_agg %>%
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid','demand+','demand+ w/ grid'),Y5==2050) %>% 
  mutate(Sv=recode(Sv,
                   'KR / JP-CG'='Korea / Chugoku',
                   'KR / JP-KY'='Korea / Kyushu',
                   'JP-HD / JP-TH'='Hokkaido / Tohoku',
                   'JP-TH / JP-KT'='Tohoku / Kanto',
                   'JP-KT / JP-CB'='Kanto / Chubu',
                   'JP-CB / JP-HR'='Other',
                   'JP-CB / JP-KS'='Chubu / Kansai',
                   'JP-HR / JP-KS'='Other',
                   'JP-KS / JP-CG'='Kansai / Chugoku',
                   'JP-KS / JP-SK'='Other',
                   'JP-CG / JP-SK'='Other',
                   'JP-CG / JP-KY'='Chugoku / Kyushu')) %>% 
  group_by(Sc,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sv=factor(Sv,levels=c('Korea / Chugoku','Korea / Kyushu',
                               'Hokkaido / Tohoku','Tohoku / Kanto',
                               'Kanto / Chubu','Chubu / Kansai',
                               'Kansai / Chugoku','Chugoku / Kyushu',
                               'Other')),
         Sc=factor(Sc,levels=c('1000C','1000C w/ grid','500C','500C w/ grid','demand+','demand+ w/ grid'))) %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value/1000,fill=Sv),stat='identity') +
  labs(x='',y='interconnection capacity (GW)') +
  scale_fill_manual(values=c('orchid','#F6CCFFFF','#b85e1a','#fcca3f','#ACBF4C','#5ea6f2','#1a66b8','#0D2570','grey50')) +
  plot_theme +
  theme(legend.position='right')
plot(g_s3)
ggsave(filename=paste0(odir,'/../Presentation/Fig_S3.png'),g_s3,
       width=3.5,height=4)

g_7_1 <- df_techcap %>% 
  filter(Sv%in%c('Cap_Ele_Sto_LiB_Ene','Cap_Ele_Sto_PHS_Ene'),Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid'),Y5>=2020) %>%
  mutate(Sv=recode(Sv,'Cap_Ele_Sto_LiB_Ene'='Li-ion battery','Cap_Ele_Sto_PHS_Ene'='pumped hydro')) %>% 
  group_by(Sc,Sr2,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=c('1000C','1000C w/ grid','500C','500C w/ grid'))) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value/1000,fill=Sv),stat='identity') +
  labs(x='',y='battery storage energy capacity (GWh)',tag='a)') +
  scale_fill_manual(values=c('#32AC9AFF','#7fffd4')) +
  facet_wrap(vars(Sc)) +
  plot_theme +
  theme(legend.position = 'none')
plot(g_7_1)

g_7_2 <- df_techcap %>% 
  filter(Sv%in%c('Cap_Ele_Sto_LiB_Ene','Cap_Ele_Sto_PHS_Ene'),Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid'),
         Y5==2050) %>%
  group_by(Sc,Sr,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=c('1000C','1000C w/ grid','500C','500C w/ grid','demand+','demand+ w/ grid')),
         Sr=factor(Sr,levels=c('JP-HD','JP-TH','JP-KT','JP-CB','JP-HR','JP-KS','JP-CG','JP-SK','JP-KY'))) %>% 
  mutate(Sv=recode(Sv,'Cap_Ele_Sto_LiB_Ene'='Li-ion battery','Cap_Ele_Sto_PHS_Ene'='pumped hydro')) %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value/1000,fill=Sv),stat='identity') +
  labs(x='',y='energy storage energy capacity (GWh)',tag='b)') +
  scale_fill_manual(values=c('#32AC9AFF','#7fffd4')) +
  facet_wrap(vars(Sr),nrow=1) +
  plot_theme +
  theme(legend.position = 'none',
        axis.title=element_blank())
plot(g_7_2)

g_7 <- (g_7_1 + g_7_2 + plot_layout(widths=c(3.5,7))) /  g_legend(g_7_2 + theme(legend.position='bottom')) + plot_layout(heights = c(5,1))
plot(g_7)
ggsave(filename=paste0(odir,'/Check_7.png'),g_7,
       width=8,height=4)


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

g_8 <- df_da_ele %>% 
  filter(Sr=='JPN',Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid'),value!=0) %>%
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value),
         D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('1000C','1000C w/ grid','500C','500C w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
  facet_wrap(vars(Sc),ncol=1) +
  labs(x='',y='electricity balance (GWh/day)') +
  plot_theme +
  theme(legend.position = 'bottom')
ggsave(filename=paste0(odir,'/Check_8.png'),g_8,width=7,height=8)


###########################################################################
# plot --East Asia
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

g_9 <- df_yr_ele %>%
  filter(Sr%in%c('JPN','CHN','KR','MN','NEA'),Sc%in%c('NoPOL','1000C','1000C w/ grid','500C','500C w/ grid')) %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value),
         Sr=recode(Sr,'KR'='KOR','MN'='MNG')) %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         Sc=factor(Sc,levels=c('NoPOL','1000C','1000C w/ grid','500C','500C w/ grid')),
         Sr=factor(Sr,levels=c('JPN','CHN','KOR','MNG','NEA'))) %>%
  ggplot() +
  geom_area(aes(x=Y5,y=value/10^6,fill=Sv)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  labs(x='',y='power generation (TWh/yr)') +
  facet_grid(cols=vars(Sc),rows=vars(Sr),scales='free_y') +
  plot_theme +
  theme(legend.position='bottom')
plot(g_9)

ggsave(filename=paste0(odir,'/Check_9.png'),g_9,
       width=8,height=11)

g_10 <- df_yr_ele %>%
  filter(Sr%in%c('JPN','CHN','KR','MN','NEA'),
         Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid'),
         Y5==2050) %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value),
         Sr=recode(Sr,'KR'='KOR','MN'='MNG')) %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         Sc=factor(Sc,levels=c('1000C','1000C w/ grid','500C','500C w/ grid')),
         Sr=factor(Sr,levels=c('JPN','CHN','KOR','MNG','NEA'))) %>%
  ggplot() +
  geom_bar(aes(x=Sc,y=value/10^6,fill=Sv),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  labs(x='',y='power generation (TWh/yr)') +
  facet_wrap(vars(Sr),nrow=1,scales='free_y') +
  plot_theme +
  theme(legend.position='bottom')
plot(g_10)

ggsave(filename=paste0(odir,'/Check_10.png'),g_10,
       width=7.5,height=5.5)

g_11 <- df_yr_ele %>%
  filter(str_detect(Sr,'JP-'),
         Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid'),
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
plot(g_11)

ggsave(filename=paste0(odir,'/Check_11.png'),g_11,
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
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid')) %>% 
  left_join(scen_cat)

g_12 <- df_lnkcap_agg %>%
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
plot(g_12)

ggsave(filename=paste0(odir,'/Check_12.png'),g_12,
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
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid'))

g_13 <- df_techcap %>% 
  filter(Sv%in%c('Cap_Ele_Sto_LiB_Ene','Cap_Ele_Sto_PHS_Ene'),Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid'),
         Y5==2050) %>% 
  group_by(Sc,Sr2,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=c('1000C','1000C w/ grid','500C','500C w/ grid')),
         Sr2=factor(Sr2,levels=c('JPN','CHN','KOR','MNG'))) %>% 
  mutate(Sv=recode(Sv,'Cap_Ele_Sto_LiB_Ene'='Li-ion battery','Cap_Ele_Sto_PHS_Ene'='pumped hydro')) %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value/10^6,fill=Sv),stat='identity') +
  labs(x='',y='energy storage energy capacity (TWh)') +
  scale_fill_manual(values=c('#32AC9AFF','#7fffd4')) +
  facet_wrap(vars(Sr2),nrow=1,scales='free') +
  plot_theme +
  theme(legend.position = 'right')
plot(g_13)

ggsave(filename=paste0(odir,'/Check_13.png'),g_13,
       width=8,height=3.5)


# import and export -------------------------------------------------------

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
  mutate(Sc=recode(Sc,
                   '500C_Elc'='default w/ grid')) %>% 
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
  mutate(Sc=recode(Sc,
                   '500C_Elc'='default w/ grid')) %>% 
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

g_14 <- df_flw_annual %>%
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

ggsave(filename=paste0(odir,'/Check_14.png'),g_14,
       width=5,height=4)

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
  mutate(Sc=recode(Sc,
                   '500C_Elc'='default w/ grid'))

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

g_15 <- df_flw_heat %>% 
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
plot(g_15)
ggsave(filename=paste0(odir,'/Check_15.png'),g_15 + theme(legend.position = 'bottom'),width=8,height=4)

g_16 <- df_flw_heat %>% 
  filter(Sv=='China/Korea') %>% 
  ggplot() +
  geom_tile(aes(x=D,y=H,fill=value/1000)) +
  facet_wrap(vars(Sc)) +
  scale_fill_distiller(palette='Spectral',
                       limits=c(-max(abs(max(df_flw_heat %>% filter(Sv=='China/Korea') %>% select(value), na.rm = TRUE))/1000,
                                     abs(min(df_flw_heat %>% filter(Sv=='China/Korea') %>% select(value), na.rm = TRUE)))/1000,
                                max(abs(max(df_flw_heat %>% filter(Sv=='China/Korea') %>% select(value), na.rm = TRUE))/1000,
                                    abs(min(df_flw_heat %>% filter(Sv=='China/Korea') %>% select(value), na.rm = TRUE))/1000))) +
  labs(x='day of the year',y='hour of the day (UTC+9)',
       fill='power flow from China to Korea (GW)') +
  scale_x_date(labels=scales::date_format("%b %d")) +
  plot_theme +
  theme(axis.title.x = element_blank(),
        legend.title = element_text(size = 8.5)) +
  guides(fill=guide_colorbar(title.position='top',
                             barwidth = 15))
plot(g_16)
ggsave(filename=paste0(odir,'/Check_16.png'),g_16 + theme(legend.position = 'bottom'),width=8,height=4)

g_17 <- df_flw_heat %>% 
  filter(Sv=='China/Mongolia') %>% 
  ggplot() +
  geom_tile(aes(x=D,y=H,fill=value/1000)) +
  facet_wrap(vars(Sc)) +
  scale_fill_distiller(palette='Spectral',
                       limits=c(-max(abs(max(df_flw_heat %>% filter(Sv=='China/Mongolia') %>% select(value), na.rm = TRUE))/1000,
                                     abs(min(df_flw_heat %>% filter(Sv=='China/Mongolia') %>% select(value), na.rm = TRUE)))/1000,
                                max(abs(max(df_flw_heat %>% filter(Sv=='China/Mongolia') %>% select(value), na.rm = TRUE))/1000,
                                    abs(min(df_flw_heat %>% filter(Sv=='China/Mongolia') %>% select(value), na.rm = TRUE))/1000))) +
  labs(x='day of the year',y='hour of the day (UTC+9)',
       fill='power flow from China to Mongolia (GW)') +
  scale_x_date(labels=scales::date_format("%b %d")) +
  plot_theme +
  theme(axis.title.x = element_blank(),
        legend.title = element_text(size = 8.5)) +
  guides(fill=guide_colorbar(title.position='top',
                             barwidth = 15))
plot(g_17)
ggsave(filename=paste0(odir,'/Check_17.png'),g_17 + theme(legend.position = 'bottom'),width=8,height=4)


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
g_18 <- df_da_ele %>% 
  filter(Sr=='JPN',Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid'),value!=0) %>%
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value),
         D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('1000C','1000C w/ grid','500C','500C w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
  facet_wrap(vars(Sc),ncol=1) +
  labs(x='',y='electricity balance (GWh/day)') +
  plot_theme +
  theme(legend.position = 'bottom')
plot(g_18)
ggsave(filename=paste0(odir,'/Check_18.png'),g_18,width=7,height=8)

g_19 <- df_hr_ele %>% 
  filter(Sr=='JPN',Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid'),
         T%in%seq(calc_d2h('2050-07-25','2050-08-07')[[1]], calc_d2h('2050-07-25','2050-08-07')[[2]]),
         value!=0) %>%
  mutate(T=as.POSIXct(paste0(plot_year,'-01-01'))+(T+24-1)*3600) %>% 
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value)) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('1000C','1000C w/ grid','500C','500C w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=T,y=value/1000,fill=Sv),stat='identity',width=3600) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_datetime(date_breaks='1 day',labels=scales::date_format("%b %d")) +
  facet_wrap(vars(Sc),ncol=1) +
  labs(x='',y='electricity balance (GWh/h)') +
  plot_theme +
  theme(legend.position = 'bottom')
plot(g_19)
ggsave(filename=paste0(odir,'/Check_19.png'),g_19,width=7,height=7)

g_20 <- df_hr_ele %>% 
  filter(Sr=='JPN',Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid'),
         T%in%seq(calc_d2h('2050-10-25','2050-11-07')[[1]], calc_d2h('2050-10-25','2050-11-07')[[2]]),
         value!=0) %>%
  mutate(T=as.POSIXct(paste0(plot_year,'-01-01'))+(T+24-1)*3600) %>% 
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value)) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('1000C','1000C w/ grid','500C','500C w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=T,y=value/1000,fill=Sv),stat='identity',width=3600) +
  scale_x_datetime(date_breaks='1 day',labels=scales::date_format("%b %d")) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  facet_wrap(vars(Sc),ncol=1) +
  labs(x='',y='electricity balance (GWh/h)') +
  plot_theme +
  theme(legend.position = 'bottom')
plot(g_20)
ggsave(filename=paste0(odir,'/Check_20.png'),g_20,width=7,height=7)


###########################################################################
# plot --cost
###########################################################################

# marginal electricity price ----------------------------------------------

df_mcoe <- foreach (i=c('500C','500C_Elc')) %do% {
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
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default'))

df_mcoe_JPN_ann <- df_mcoe %>% 
  filter(str_detect(N,'JP-')) %>% 
  group_by(Sc,I) %>% 
  reframe(value=sum(dmd*eq_npb_m)/sum(dmd))%>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default'))

scen_cat <- scen_mat %>% 
  transmute(Sc=Scenario,cat1=scen_dpol,cat2=scen_tpol)

g_21_1 <- df_mcoe_JPN %>%
  filter(I=='AC') %>% 
  group_by(Sc,I) %>% 
  arrange(-value) %>% mutate(T=row_number()) %>% 
  left_join(scen_cat) %>% 
  filter(cat1=='default') %>% 
  ggplot() +
  geom_step(aes(x=T,y=value/1000,color=cat1,linetype=cat2)) +
  scale_color_d3() +
  scale_x_continuous(breaks=c(1,seq(1000,8000,1000),8760)) +
  scale_linetype_manual(values=c('w/o grid'='solid','w/ grid'='dashed')) +
  labs(x='',y='Marginal cost of electricity (USD/kWh)',tag='a)') +
  plot_theme +
  theme(legend.position='none')

g_21_2 <- df_mcoe_JPN %>%
  group_by(Sc,I) %>% 
  filter(I=='AC',) %>% 
  arrange(-value) %>% mutate(T=row_number()) %>% 
  left_join(scen_cat) %>% 
  filter(cat1=='default') %>% 
  ggplot() +
  geom_step(aes(x=T,y=value/1000,color=cat1,linetype=cat2),linewidth=0.3) +
  scale_color_d3() +
  labs(y='Marginal cost of electricity (USD/kWh)') +
  scale_x_continuous(breaks=c(1,seq(1000,8000,1000),8760)) +
  scale_y_continuous(limits=c(0,0.50))  +
  scale_linetype_manual(values=c('w/o grid'='solid','w/ grid'='dashed')) +
  labs(x='',y='Marginal cost of electricity (USD/kWh)',tag='b)') +
  plot_theme +
  theme(legend.position=c(0.8,0.8),
        axis.title.y = element_blank())

g_21 <- g_21_1 + g_21_2

ggsave(filename=paste0(odir,'/Check_21.png'),g_21,width=6,height=3.5)


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

df_gensto_sys <- foreach (i=c('500C','500C_Elc','NoPOL')) %do% {
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


df_lnk_sys <- foreach (i=c('500C','500C_Elc','NoPOL')) %do% {
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
  mutate(across(c(`500C`,`500C_Elc`,`500C_dmd+_CCS`,`500C_dmd+_CCS_Elc`,`NoPOL`),~.-`NoPOL`)) %>%
  pivot_longer(cols=-c(Sv,Sr2,YEAR),names_to='Sc',values_to='value') %>% 
  mutate(discount=(1-0.05)^(YEAR-2020)) %>% 
  filter(Sc!='NoPOL') %>%
  group_by(Sc,Sv,Sr2) %>% 
  reframe(value=sum(value*discount)) %>% 
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('1000C','1000C w/ grid','500C','500C w/ grid','demand+','demand+ w/ grid'))


df_sys_nea <- df_sys %>%
  group_by(Sc,Sv) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sr2='NEA')

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
  mutate(Sr2='NEA')

df_GDP <- bind_rows(df_GDP_country,df_GDP_nea)

plt <- set_plot(ele_inv)

g_22 <- bind_rows(df_sys,df_sys_nea) %>% 
  left_join(df_GDP) %>% 
  mutate(value=value/GDP*100) %>% 
  mutate(Sv=factor(Sv,levels=rev(ele_inv$Variable)),
         Sc=factor(Sc,levels=c('1000C','1000C w/ grid','500C','500C w/ grid','demand+','demand+ w/ grid')),
         Sr2=factor(Sr2,levels=c('JPN','CHN','KOR','MNG','NEA'))) %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value,fill=Sv),stat='identity') +
  geom_point(data=df_sys_total %>% 
               left_join(df_GDP) %>% 
               mutate(value=value/GDP*100,
                      Sr2=factor(Sr2,levels=c('JPN','CHN','KOR','MNG','NEA'))),
             aes(x=Sc,y=value,fill=Sv),fill='white',color='black',shape=23,size=1.5) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  facet_wrap(vars(Sr2),scales='free',nrow=1) +
  labs(x='',y='Cumulative additional system cost (% of GDP)') +
  plot_theme +
  theme(legend.position = 'bottom')
ggsave(filename=paste0(odir,'/Check_22.png'),g_22,width=8,height=5.5)


