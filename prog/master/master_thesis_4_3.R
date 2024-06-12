
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
odir <- paste0(cdir,'/output/',date,'/Chapter_4_3')
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

# marginal electricity price ----------------------------------------------

df_mcoe <- foreach (i=c('500C','500C_Elc','500C_dmd+_CCS','500C_dmd+_CCS_Elc')) %do% {
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

g_4_19_1 <- df_mcoe_JPN %>%
  filter(I=='AC') %>% 
  group_by(Sc,I) %>% 
  arrange(-value) %>% mutate(T=row_number()) %>% 
  left_join(scen_cat) %>% 
  ggplot() +
  geom_step(aes(x=T,y=value/1000,color=cat1,linetype=cat2)) +
  scale_color_d3() +
  scale_x_continuous(breaks=c(1,seq(1000,8000,1000),8760)) +
  scale_linetype_manual(values=c('w/o grid'='solid','w/ grid'='dashed')) +
  labs(x='',y='Marginal cost of electricity (USD/kWh)',tag='a)') +
  plot_theme +
  theme(legend.position='none')

g_4_19_2 <- df_mcoe_JPN %>%
  group_by(Sc,I) %>% 
  filter(I=='AC') %>% 
  arrange(-value) %>% mutate(T=row_number()) %>% 
  left_join(scen_cat) %>% 
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

g_4_19 <- g_4_19_1 + g_4_19_2

ggsave(filename=paste0(odir,'/Fig_4_19.png'),g_4_19,width=6,height=3.5)


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

df_gensto_sys <- foreach (i=c('500C','500C_Elc','500C_dmd+_CCS','500C_dmd+_CCS_Elc','NoPOL')) %do% {
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


df_lnk_sys <- foreach (i=c('500C','500C_Elc','500C_dmd+_CCS','500C_dmd+_CCS_Elc','NoPOL')) %do% {
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
  filter(Sc%in%c('default','default w/ grid','demand+','demand+ w/ grid'))
  

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

g_4_20 <- bind_rows(df_sys,df_sys_nea) %>% 
  left_join(df_GDP) %>% 
  mutate(value=value/GDP*100) %>% 
  mutate(Sv=factor(Sv,levels=rev(ele_inv$Variable)),
         Sc=factor(Sc,levels=c('default','default w/ grid','demand+','demand+ w/ grid')),
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
ggsave(filename=paste0(odir,'/Fig_4_20.png'),g_4_20,width=8,height=5.5)

g_s7 <- df_sys_total %>% 
  left_join(df_GDP) %>% 
  mutate(value=value/GDP*100) %>% 
  filter(Sr2=='NEA') %>%
  mutate(Sc=factor(Sc,levels=c('default','default w/ grid','demand+','demand+ w/ grid'))) %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value),fill='grey30',stat='identity') +
  labs(x='',y='Cumulative additional system cost\n(trillion USD)') +
  plot_theme +
  theme(legend.position = 'bottom')
ggsave(filename=paste0(odir,'/../Presentation/Fig_S7.png'),g_s7,width=2,height=4)