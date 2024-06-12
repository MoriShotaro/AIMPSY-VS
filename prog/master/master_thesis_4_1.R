
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

# setting -----------------------------------------------------------------

date <- '2403191936'
cdir <- getwd()
ddir <- paste0(cdir,'/data/',date)  # output path
pdir <- paste0(cdir,'/prog/inc_prog')
odir <- paste0(cdir,'/output/',date,'/Chapter_4_1')
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

# load modules ------------------------------------------------------------

# import variable definition
source(paste0(ddir,'/define/var.R'))

# import plot function
source(paste0(pdir,'/func_analysis.R'))


###########################################################################
# plot
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
g_4_1 <- df_yr_ele %>%
  filter(Sr%in%c('JPN'),Sc%in%c('NoPOL','default','demand+')) %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value)) %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         Sc=factor(Sc,levels=c('NoPOL','default','demand+'))) %>%
  ggplot() +
  geom_area(aes(x=Y5,y=value/10^6,fill=Sv)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_y_continuous(limits=c(NA,2400))+
  labs(x='',y='power generation (TWh/yr)') +
  facet_wrap(vars(Sc)) +
  plot_theme +
  theme(legend.position='right')

t1 <- df_yr_ele %>%
  filter(Sr%in%c('JPN'),Sc%in%c('NoPOL','default','demand+'),
         Sv!='Sec_Ene_Ele_Cur',Sv!='Sec_Ene_Ele_Sto_Los',Sv!='Sec_Ene_Ele_Trd_Los') %>% 
  group_by(Sc,Sr,Y5) %>% 
  mutate(value=value/sum(value)*100) %>% 
  pivot_wider(names_from=Sv,values_from=value,values_fill=0) %>% 
  mutate(Sec_Ene_Ele_WinSol=Sec_Ene_Ele_Solar_Cen+Sec_Ene_Ele_Solar_Dec+
           Sec_Ene_Ele_Win_Ons+Sec_Ene_Ele_Win_Off)

ggsave(filename=paste0(odir,'/Fig_4_1.png'),g_4_1,
       width=8,heigh=4.5)


g_4_2 <- df_yr_ele %>%
  filter(Y5==2050,str_detect(Sr,'JP-'),Sc%in%c('default','demand+')) %>%
  mutate(value=case_when(Sv%in%flag_eleyr~-value,
                         TRUE~value)) %>% 
  mutate(Sv=factor(Sv,levels=rev(eleyr$Variable)),
         Sc=factor(Sc,levels=c('default','demand+')),
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

ggsave(filename=paste0(odir,'/Fig_4_2.png'),g_4_2,
       width=8,height=4.5)

g_4_3 <- df_all_hr %>% 
  filter(Sv=='Trd_Sec_Ene_Ele',value!=0,str_detect(Sr,'JP-'),Sc%in%c('default','demand+')) %>% 
  mutate(Sv=case_when(value>=0~'import',
                      value<0~'export'),
         Sr=factor(Sr,levels=c('JP-HD','JP-TH','JP-KT','JP-CB','JP-HR','JP-KS','JP-CG','JP-SK','JP-KY'))) %>% 
  group_by(Sc,Sv,Sr) %>% 
  summarise(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=c('default','demand+'))) %>% 
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
plot(g_4_3)
 
ggsave(filename=paste0(odir,'/Fig_4_3.png'),g_4_3,
       width=7,heigh=3.5)

# presentation slide
plt <- set_plot(eleyr2)

df_yr_ele2 <- df_all_yr %>% 
  filter(Sv%in%eleyr2$Variable) %>%
  mutate(Sv=recode(Sv,'Sec_Ene_Ele_Sto_Dis_PHS'='Sec_Ene_Ele_Hyd')) %>% 
  group_by(Sc,Sv,Sr,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=scen_mat$Scenario)) %>%
  mutate(cat1=scen_mat$scen_dpol[Sc], cat2=scen_mat$scen_tpol[Sc]) %>%
  mutate(cat1=factor(cat1, levels=unique(scen_mat$scen_dpol))) %>%
  mutate(cat2=factor(cat2, levels=unique(scen_mat$scen_tpol)))

g_s1_1 <- df_yr_ele2 %>%
  filter(Sr%in%c('JPN'),Sc%in%c('NoPOL','default','demand+')) %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr2$Variable)),
         Sc=factor(Sc,levels=c('NoPOL','default','demand+'))) %>%
  ggplot() +
  geom_bar(aes(x=Y5,y=value/10^6,fill=Sv),stat='identity',width=4.9) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_y_continuous(limits=c(NA,2400))+
  labs(x='',y='power generation (TWh/yr)') +
  facet_wrap(vars(Sc)) +
  plot_theme +
  theme(legend.position='none')

g_s1_2 <- df_yr_ele2 %>%
  filter(Sr%in%c('JPN'),Y5==2050,Sc!='NoPOL') %>%
  mutate(Sv=factor(Sv,levels=rev(eleyr2$Variable)),
         Sc=factor(Sc,levels=c('NoPOL','default','default w/ grid','demand+','demand+ w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=Sc,y=value/10^6,fill=Sv),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_y_continuous(limits=c(NA,2400))+
  labs(x='',y='power generation (TWh/yr)') +
  facet_wrap(vars(Y5)) +
  plot_theme +
  theme(legend.position='right',
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank())

g_s1 <- g_s1_1 + g_s1_2 + plot_layout(widths=c(5,1))

ggsave(filename=paste0(odir,'/../Presentation/Fig_S1.png'),g_s1,
       width=7.5,height=4.5)

plt <- set_plot(eleyr3)

df_yr_ele3 <- df_all_yr %>% 
  mutate(Sv=recode(Sv,
                   'Sec_Ene_Ele_Hyd'='other',
                   'Sec_Ene_Ele_Geo'='other',
                   'Sec_Ene_Ele_Nuc'='other'
                   )) %>% 
  group_by(Sc,Sv,Sr,Y5) %>% 
  reframe(value=sum(value)) %>% 
  filter(Sv%in%eleyr3$Variable) %>%
  mutate(Sv=recode(Sv,'Sec_Ene_Ele_Sto_Dis_PHS'='Sec_Ene_Ele_Hyd')) %>% 
  mutate(Sc=factor(Sc,levels=scen_mat$Scenario)) %>%
  mutate(cat1=scen_mat$scen_dpol[Sc], cat2=scen_mat$scen_tpol[Sc]) %>%
  mutate(cat1=factor(cat1, levels=unique(scen_mat$scen_dpol))) %>%
  mutate(cat2=factor(cat2, levels=unique(scen_mat$scen_tpol)))

g_s2 <- df_yr_ele3 %>%
  filter(Y5==2050,str_detect(Sr,'JP-'),Sc%in%c('default','demand+')) %>%
  mutate(Sr=recode(Sr,'JP-HD'='Hokkaido','JP-TH'='Tohoku','JP-KT'='Kanto',
                   'JP-CB'='Chubu','JP-HR'='Hokuriku','JP-KS'='Kansai',
                   'JP-CG'='Chugoku','JP-SK'='Shikoku','JP-KY'='Kyushu')) %>% 
  mutate(Sv=factor(Sv,levels=rev(eleyr3$Variable)),
         Sc=factor(Sc,levels=c('default','demand+')),
         Sr=factor(Sr,levels=c('Hokkaido','Tohoku','Kanto',
                               'Chubu','Hokuriku','Kansai',
                               'Chugoku','Shikoku','Kyushu'))) %>%
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
plot(g_s2)
ggsave(filename=paste0(odir,'/../Presentation/Fig_S2.png'),g_s2,
       width=8.1,height=4)


# interconnection ---------------------------------------------------------

df_flw <- foreach (i=c('500C','500C_dmd+_CCS')) %do% {
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
  mutate(Sc=recode(Sc,'500C_dmd+_CCS'='demand+',
                   '500C'='default')) %>%
  filter(Sc%in%c('default','demand+')) %>%
  mutate(Sc=factor(Sc,levels=scen_mat$Scenario)) %>%
  mutate(cat1=scen_mat$scen_dpol[Sc], cat2=scen_mat$scen_tpol[Sc]) %>%
  mutate(cat1=factor(cat1, levels=unique(scen_mat$scen_dpol))) %>%
  mutate(cat2=factor(cat2, levels=unique(scen_mat$scen_tpol)))

df_enebal <- df_all_yr %>%
  filter(Sv%in%c('Trd_Sec_Ene_Ele'),str_detect(Sr,'JP-'),Y5==plot_year,Sc%in%c('default','demand+')) %>%
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
vgs_l2 <- rgdx.param(paste0(ddir,'/gams_output/gdx_primary/500C_dmd+_CCS/2050.gdx'),'vgs_l') %>% 
  filter(R%in%c('PVC','PVDC','PVDR','WNO','WNB','WNF')) %>% 
  right_join(gsmx) %>% 
  replace_na(list(vgs_l=0)) %>% 
  filter(str_detect(N,'JP-')) %>% 
  mutate(Sv='installed in demand+')
g_4_4 <- bind_rows(vgs_l1,vgs_l2) %>% 
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
plot(g_4_4)
ggsave(filename=paste0(odir,'/Fig_4_4.png'),g_4_4,
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
  filter(Sc%in%c('default','default w/ grid','demand+','demand+ w/ grid'))

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

g_4_5_1 <- df_lnkcap_agg %>%
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('default','demand+'),Y5>=2020) %>% 
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

g_4_5_2 <- df_lnkcap_agg %>%
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('default','demand+'),Y5>=2020) %>% 
  pivot_wider(names_from=Sc,values_from=value) %>% 
  transmute(Sv,Y5,value=`demand+`-default) %>% 
  mutate(Sv=factor(Sv,levels=c('JP-HD / JP-TH','JP-TH / JP-KT',
                               'JP-KT / JP-CB','JP-CB / JP-HR','JP-CB / JP-KS','JP-HR / JP-KS',
                               'JP-KS / JP-CG','JP-KS / JP-SK','JP-CG / JP-SK','JP-CG / JP-KY'))) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value/1000,fill=Sv),stat='identity') +
  labs(x='',y='difference between demand+ and default\n(GW)',tag='b)') +
  scale_fill_simpsons()+
  plot_theme

g_4_5 <- g_4_5_1 + g_4_5_2 + plot_layout(width=c(2,1))

ggsave(filename=paste0(odir,'/Fig_4_5.png'),g_4_5,
       width=8,height=4)

g_s3 <- df_lnkcap_agg %>%
  mutate(Sc=recode(Sc,
                   '500C_dmd+_CCS_Elc'='demand+ w/ grid',
                   '500C_dmd+_CCS'='demand+',
                   '500C_Elc'='default w/ grid',
                   '500C'='default')) %>% 
  filter(Sc%in%c('default','default w/ grid','demand+','demand+ w/ grid'),Y5==2050) %>% 
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
         Sc=factor(Sc,levels=c('default','default w/ grid','demand+','demand+ w/ grid'))) %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value/1000,fill=Sv),stat='identity') +
  labs(x='',y='interconnection capacity (GW)') +
  scale_fill_manual(values=c('orchid','#F6CCFFFF','#b85e1a','#fcca3f','#ACBF4C','#5ea6f2','#1a66b8','#0D2570','grey50')) +
  plot_theme +
  theme(legend.position='right')
ggsave(filename=paste0(odir,'/../Presentation/Fig_S3.png'),g_s3,
       width=3.5,height=4)

g_4_6_1 <- df_techcap %>% 
  filter(Sv%in%c('Cap_Ele_Sto_LiB_Ene','Cap_Ele_Sto_PHS_Ene'),Sc%in%c('default','demand+'),Y5>=2020) %>%
  mutate(Sv=recode(Sv,'Cap_Ele_Sto_LiB_Ene'='Li-ion battery','Cap_Ele_Sto_PHS_Ene'='pumped hydro')) %>% 
  group_by(Sc,Sr2,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=c('default','default w/ grid','demand+','demand+ w/ grid'))) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value/1000,fill=Sv),stat='identity') +
  labs(x='',y='battery storage energy capacity (GWh)',tag='a)') +
  scale_fill_manual(values=c('#32AC9AFF','#7fffd4')) +
  facet_wrap(vars(Sc)) +
  plot_theme +
  theme(legend.position = 'none')

g_4_6_2 <- df_techcap %>% 
  filter(Sv%in%c('Cap_Ele_Sto_LiB_Ene','Cap_Ele_Sto_PHS_Ene'),Sc%in%c('default','demand+'),
         Y5==2050) %>%
  group_by(Sc,Sr,Sv,Y5) %>% 
  reframe(value=sum(value)) %>% 
  mutate(Sc=factor(Sc,levels=c('default','default w/ grid','demand+','demand+ w/ grid')),
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

g_4_6 <- (g_4_6_1 + g_4_6_2 + plot_layout(widths=c(3.5,7))) /  g_legend(g_4_6_2 + theme(legend.position='bottom')) + plot_layout(heights = c(5,1))
plot(g_4_6)
ggsave(filename=paste0(odir,'/Fig_4_6.png'),g_4_6,
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

g_4_7 <- df_da_ele %>% 
  filter(Sr=='JPN',Sc%in%c('default','demand+'),value!=0,
         Sv!='Trd_Sec_Ene_Ele') %>%
  mutate(value=case_when(Sv%in%flag_elehr~-value,
                         TRUE~value),
         D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr$Variable)),
         Sc=factor(Sc,levels=c('default','demand+'))) %>%
  ggplot() +
  geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
  facet_wrap(vars(Sc),ncol=1) +
  labs(x='',y='electricity balance (GWh/day)') +
  plot_theme +
  theme(legend.position = 'bottom')
ggsave(filename=paste0(odir,'/Fig_4_7.png'),g_4_7,width=7,height=8)

# presentation slide
plt <- set_plot(elehr2)

# hourly electricity balance
df_hr_ele2 <- df_all_hr %>%
  filter(Sv%in%elehr2$Variable) %>% 
  complete(Sc,Sv,Sr,St,fill=list(value=0)) %>%
  mutate(T=(as.numeric(str_sub(St,start=2,end=4))-1)*24+as.numeric(str_sub(St,start=6,end=7))+1+9) %>% 
  mutate(Sc=factor(Sc,levels=scen_mat$Scenario)) %>%
  mutate(cat1=scen_mat$scen_dpol[Sc], cat2=scen_mat$scen_tpol[Sc])

# daily electricity balance
df_da_ele2 <- df_hr_ele2 %>% 
  mutate(D=(T-1)%/%24+1) %>% 
  group_by(Sc,Sv,Sr,D,cat1,cat2) %>%
  reframe(value=sum(value)) %>% 
  filter(cat1!='NoPOL')

g_s4 <- df_da_ele2 %>% 
  filter(Sr=='JPN',Sc%in%c('default','default w/ grid'),value!=0) %>%
  filter(Sc=='default w/ grid'|Sv!='Trd_Sec_Ene_Ele') %>% 
  mutate(value=case_when(Sv%in%flag_elehr2~-value,
                         TRUE~value),
         D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
  mutate(Sv=factor(Sv,levels=rev(elehr2$Variable)),
         Sc=factor(Sc,levels=c('default','default w/ grid'))) %>%
  ggplot() +
  geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend)+
  scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
  facet_wrap(vars(Sc),ncol=1) +
  labs(x='',y='electricity balance (GWh/day)') +
  plot_theme +
  theme(legend.position = 'right')
plot(g_s4)
ggsave(filename=paste0(odir,'/../Presentation/Fig_S4.png'),g_s4,width=8,height=6)
