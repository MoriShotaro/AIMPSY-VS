set_plot <- function(var){
  plt <- list()
  plt$Color <- as.character(var$Color); names(plt$Color) <- as.character(var$Variable)
  plt$Legend <- as.character(var$Legend); names(plt$Legend) <- as.character(var$Variable)
  return(plt)
}

calc_d2h <- function(start_date, end_date) {
  # Convert the strings to date objects
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Calculate the start of the year
  year_start <- as.Date(paste0(format(start_date, '%Y'), '-01-01'))
  
  # Calculate the hour ranges
  start_hour <- as.numeric(difftime(start_date, year_start, units = 'hours')) + 1
  end_hour <- as.numeric(difftime(end_date, year_start, units = 'hours')) + 24 # Including the end date
  
  return(c(start_hour, end_hour))
}

func_dispatch_hourly_summary <- function(time_range,region,year) {
  g <- df_hr_ele %>%
    filter(Sr==region,
           T%in%seq(calc_d2h(time_range[[1]], time_range[[2]])[[1]], calc_d2h(time_range[[1]], time_range[[2]])[[2]]),
           value!=0) %>%
    mutate(value=case_when(Sv%in%flag_elehr~-value,
                           TRUE~value),
           T=as.POSIXct(paste0(plot_year,'-01-01'))+(T+24-1)*3600) %>% 
    mutate(Sv=factor(Sv,levels=rev(elehr$Variable))) %>%
    ggplot() +
    geom_bar(aes(x=T,y=value/1000,fill=Sv),stat='identity',width=3600) +
    scale_fill_manual(values=plt$Color,labels=plt$Legend)+
    scale_x_datetime(date_breaks='1 day',labels=scales::date_format("%b %d")) +
    labs(y='electricity balance (GWh/h)') +
    plot_theme +
    theme(axis.title.x=element_blank())
  
  if(all(is.na(scen_mat$scen_tpol))){
    g <- g + facet_grid(.~cat1)
  } else {
    g <- g + facet_grid(cat2~cat1)}
  
  ncat1 <- length(unique(df_hr_ele$cat1)); ncat2 <- length(unique(df_hr_ele$cat2))
  
  if (run_mode=='local') {
    ggsave(filename=paste0(odir,'/plot/analysis/',region,'/',year,'_',time_range[[1]],'_',time_range[[2]],'.png'),
           width=9*(calc_d2h(time_range[[1]],time_range[[2]])[[2]]-calc_d2h(time_range[[1]],time_range[[2]])[[1]])/7/24*length(unique(df_hr_ele$cat1)),
           height=5*length(unique(df_hr_ele$cat2)))
  } else if (run_mode=='hpc') {
    CairoPNG(filename=paste0(odir,'/plot/',region,'/summary/',year,'_',time_range[[1]],'_',time_range[[2]],'.png'),units='in',dpi=200,
             width=9*(calc_d2h(time_range[[1]],time_range[[2]])[[2]]-calc_d2h(time_range[[1]],time_range[[2]])[[1]])/7/24*length(unique(df_hr_ele$cat1)),
             height=5*length(unique(df_hr_ele$cat2)))
    plot(g)
    dev.off()
  }
}

func_dispatch_hourly_scenario <- function(time_range,region,year,scenario) {
  g <- df_hr_ele %>% 
    filter(Sr==region,Sc==scenario,
           T%in%seq(calc_d2h(time_range[[1]], time_range[[2]])[[1]], calc_d2h(time_range[[1]], time_range[[2]])[[2]]),
           value!=0) %>%
    mutate(value=case_when(Sv%in%flag_elehr~-value,
                           TRUE~value),
           T=as.POSIXct(paste0(plot_year,'-01-01'))+(T+24-1)*3600) %>% 
    mutate(Sv=factor(Sv,levels=rev(elehr$Variable))) %>%
    ggplot() +
    geom_bar(aes(x=T,y=value/1000,fill=Sv),stat='identity',width=3600) +
    scale_fill_manual(values=plt$Color,labels=plt$Legend)+
    scale_x_datetime(date_breaks='1 day',labels=scales::date_format("%b %d")) +
    labs(x='hour of year',y='electricity balance (GW)') +
    plot_theme +
    theme(axis.title.x=element_blank())
  if (run_mode=='local') {
    ggsave(filename=paste0(odir,'/plot/',region,'/',scenario,'/',year,'_',time_range[[1]],'_',time_range[[2]],'.png'),
           width=9*(calc_d2h(time_range[[1]],time_range[[2]])[[2]]-calc_d2h(time_range[[1]],time_range[[2]])[[1]]+1)/7/24,
           height=5)
  } else if (run_mode=='hpc') {
    CairoPNG(filename=paste0(odir,'/plot/',region,'/',scenario,'/',year,'_',time_range[[1]],'_',time_range[[2]],'.png'),units='in',dpi=200,
             width=9*(calc_d2h(time_range[[1]],time_range[[2]])[[2]]-calc_d2h(time_range[[1]],time_range[[2]])[[1]]+1)/7/24,
             height=5)
    plot(g)
    dev.off()
  }
}

func_dispatch_daily_summary <- function(region,year) {
  g <- df_da_ele %>%
    filter(Sr==region,value!=0) %>%
    mutate(value=case_when(Sv%in%flag_elehr~-value,
                           TRUE~value),
           D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
    mutate(Sv=factor(Sv,levels=rev(elehr$Variable))) %>%
    ggplot() +
    geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
    scale_fill_manual(values=plt$Color,labels=plt$Legend)+
    scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
    labs(x='hour of year',y='electricity balance (GWh/day)') +
    facet_wrap(vars(Sc)) +
    plot_theme
  
  # if(all(is.na(scen_mat$scen_tpol))){
  #   g <- g + facet_grid(.~cat1)
  # } else {
  #   g <- g + facet_grid(cat2~cat1)}
  
  ncat1 <- length(unique(df_hr_ele$cat1)); ncat2 <- length(unique(df_hr_ele$cat2))
  
  if (run_mode=='local') {
    ggsave(filename=paste0(odir,'/plot/analysis/',region,'/',year,'_daily.png'),
           width=15*length(unique(df_hr_ele$cat1)),
           height=5*length(unique(df_hr_ele$cat2)))
  } else if (run_mode=='hpc') {
    CairoPNG(filename=paste0(odir,'/plot/analysis/daily/',year,'_',region,'.png'),units='in',dpi=200,
             width=15*length(unique(df_hr_ele$cat1)),
             height=5*length(unique(df_hr_ele$cat2)))
    plot(g)
    dev.off()
  }
}

func_dispatch_daily_scenario <- function(region,year,scenario) {
  g <- df_da_ele %>% 
    filter(Sr==region,Sc==scenario,value!=0) %>%
    mutate(value=case_when(Sv%in%flag_elehr~-value,
                           TRUE~value),
           D=as.Date(D-1,origin=paste0(plot_year,'-01-01'))) %>% 
    mutate(Sv=factor(Sv,levels=rev(elehr$Variable))) %>%
    ggplot() +
    geom_bar(aes(x=D,y=value/1000,fill=Sv),stat='identity',width=1) +
    scale_fill_manual(values=plt$Color,labels=plt$Legend)+
    scale_x_date(date_breaks='1 month',labels=scales::date_format("%b %d")) +
    labs(x='hour of year',y='electricity balance (GWh/day)') +
    plot_theme
  if (run_mode=='local') {
    ggsave(filename=paste0(odir,'/plot/analysis/',region,'/',year,'_daily.png'),
           width=20,
           height=5)
  } else if (run_mode=='hpc') {
    CairoPNG(filename=paste0(odir,'/plot/',region,'/summary/',year,'_daily_',scenario,'.png'),units='in',dpi=200,
             width=20,
             height=5)
    plot(g)
    dev.off()
  }
}

func_genmix_summary <- function(region) {
  g <- df_yr_ele %>%
    filter(Sr==region) %>%
    mutate(value=case_when(Sv%in%flag_eleyr~-value,
                           TRUE~value)) %>% 
    mutate(Sv=factor(Sv,levels=rev(eleyr$Variable))) %>%
    ggplot() +
    geom_area(aes(x=as.numeric(Sy),y=value/10^6,fill=Sv),stat='identity',width=1) +
    scale_fill_manual(values=plt$Color,labels=plt$Legend)+
    labs(x='hour of year',y='power generation (TWh/yr)') +
    plot_theme
  
  if(all(is.na(scen_mat$scen_tpol))){
    g <- g + facet_grid(.~cat1)
  } else {
    g <- g + facet_grid(cat2~cat1)}
  
  ncat1 <- length(unique(df_yr_ele$cat1)); ncat2 <- length(unique(df_yr_ele$cat2))
  
  if (run_mode=='local') {
    ggsave(filename=paste0(odir,'/plot/summary/',region,'_genmix.png'),g,
           width=9*length(unique(df_yr_ele$cat1)),height=5*length(unique(df_yr_ele$cat2)))
  } else if (run_mode=='hpc') {
    CairoPNG(filename=paste0(odir,'/plot/summary/',region,'_genmix.png'),units='in',dpi=200,
             width=9*length(unique(df_yr_ele$cat1)),
             height=5*length(unique(df_yr_ele$cat2)))
    plot(g)
    dev.off()
  }
}

func_genmix_scenario <- function(year,scenario) {
  g <- df_hr_ele %>% 
    filter(Sr==region,Sc==scenario,
           T%in%seq(calc_d2h(time_range[[1]], time_range[[2]])[[1]], calc_d2h(time_range[[1]], time_range[[2]])[[2]]),
           value!=0) %>%
    mutate(value=case_when(Sv%in%flag_elehr~-value,
                           TRUE~value)) %>% 
    mutate(Sv=factor(Sv,levels=rev(elehr$Variable))) %>%
    ggplot() +
    geom_bar(aes(x=T,y=value/1000,fill=Sv),stat='identity',width=1) +
    scale_fill_manual(values=plt$Color,labels=plt$Legend)+
    labs(x='hour of year',y='electricity balance (GW)') +
    plot_theme
  
  if (run_mode=='local') {
    ggsave(filename=paste0(odir,'/plot/',region,'/',scenario,'/',year,'_',time_range[[1]],'_',time_range[[2]],'.png'),
           width=9*(calc_d2h(time_range[[1]],time_range[[2]])[[2]]-calc_d2h(time_range[[1]],time_range[[2]])[[1]]+1)/7/24,
           height=5)
  } else if (run_mode=='hpc') {
    CairoPNG(filename=paste0(odir,'/plot/',region,'/',scenario,'/',year,'_',time_range[[1]],'_',time_range[[2]],'.png'),units='in',dpi=200,
             width=9*(calc_d2h(time_range[[1]],time_range[[2]])[[2]]-calc_d2h(time_range[[1]],time_range[[2]])[[1]]+1)/7/24,
             height=5)
    plot(g)
    dev.off()
  }
}

plot_theme <- theme_bw() +
  theme(
    panel.border=element_blank(),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour="black",linewidth=0.3),
    panel.background=element_rect(fill = "white"),
    panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=10, colour = "black", angle = 0,face="plain"),
    strip.text.y = element_text(size=10, colour = "black", angle = 270,face="plain"),
    axis.text.x=element_text(size = 10,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    axis.text.y=element_text(size = 10,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    legend.text = element_text(size = 8.5),
    legend.title = element_blank(),
    axis.ticks=element_line(linewidth=0.3),
    axis.ticks.length=unit(0.15,"cm"),
    axis.text = element_blank(),
    plot.tag=element_text(size=10,face='bold')
  )

