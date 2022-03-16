# summarise daily data
daysum_fun <- function(dlydat){
  
  out <- dlydat %>%
    rowwise() %>% 
    mutate(
      Date = as.Date(DateTime, tz = 'EST'), 
      chg = sum(W, S, na.rm = T)
    ) %>% 
    group_by(Date) %>% 
    summarise(
      nfeed = sum(!is.na(`Length (min)`)), 
      sumfd = sum(`Length (min)`, na.rm = T) / 60,
      nchge = sum(chg > 0), 
      sumwt = sum(W, na.rm = T), 
      sumst = sum(S, na.rm = T),
      .groups = 'drop'
    )
  
  return(out)
  
}

# tabulate daily summaries
daysum_tab <- function(dlydat){
  
  totab <- daysum_fun(dlydat) %>% 
    arrange(desc(Date))
  
  out <- reactable(
    totab,
    columns = list(
      nfeed = colDef(name = '# of feedings'),
      sumfd = colDef(name = 'Sum feed time (hr)', format = colFormat(digits = 1)),
      nchge = colDef(name = '# of changes'), 
      sumwt = colDef(name = 'Sum wet'), 
      sumst = colDef(name = 'Sum stool')
    ), 
    defaultColDef = colDef(format = colFormat(digits = 0), align = 'left'), 
    resizable = T, 
    defaultPageSize = 10
  )
  
  return(out)
  
}

# tabulate total summaries
totsum_tab <- function(dlydat){
  
  totab <- daysum_fun(dlydat) %>% 
    summarise(
      totdy = length(unique(Date)), 
      nfeed = sum(nfeed),
      sumfd = sum(sumfd), 
      nchge = sum(nchge),
      sumwt = sum(sumwt), 
      sumst = sum(sumst)
    ) %>% 
    mutate(sumfd = sumfd / 24)
  
  out <- reactable(
    totab,
    columns = list(
      totdy = colDef(name = 'Total days'),
      nfeed = colDef(name = '# of feedings'),
      sumfd = colDef(name = 'Sum feed time (dys)', format = colFormat(digits = 1)),
      nchge = colDef(name = '# of changes'), 
      sumwt = colDef(name = 'Sum wet'), 
      sumst = colDef(name = 'Sum stool')
    ), 
    defaultColDef = colDef(format = colFormat(digits = 0), align = 'left'), 
    resizable = T, 
    defaultPageSize = 10
  )
  
  return(out)
  
}

# feed summary plot
fdsum_plo <- function(dlydat){
  
  hrsum <- dlydat %>%
    mutate(
      Date = as.Date(DateTime, tz = 'EST'), 
      hr = hour(DateTime)
    ) %>% 
    group_by(Date, hr) %>% 
    summarise(
      `Length (min)` = sum(`Length (min)`, na.rm = T), 
      .groups = 'drop'
    ) %>% 
    complete(Date, hr, fill = list(`Length (min)` = NA)) %>% 
    mutate(
      `Length (min)` = case_when(
        `Length (min)` == 0 ~ NA_real_, 
        T ~ `Length (min)`
      )
    )

  p <- ggplot(hrsum, aes(x = Date, y = hr, fill = `Length (min)`)) +
    geom_tile(color = 'black') +
    scale_fill_distiller(palette = 'Greens', direction = 2, na.value = 'lightgrey') + 
    scale_y_reverse(expand = c(0,0), breaks = c(0:23)) + 
    scale_x_date(expand = c(0, 0), date_breaks = 'day', date_labels = '%m-%d') + 
    labs(
      y = 'Hour of day', 
      x = NULL
    ) + 
    theme_minimal() + 
    theme(
      legend.position = 'top', 
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(), 
      axis.text.x = element_text(size = 7, angle = 90, hjust = 1)
    )
  
  out <- ggplotly(p, width = 900)
  
  return(out)
  
}

# change summary plot
chsum_plo <- function(dlydat){
  
  hrsum <- dlydat %>%
    filter(!(is.na(W) & is.na(S))) %>% 
    rowwise() %>% 
    mutate(
      Date = as.Date(DateTime, tz = 'EST'), 
      hr = hour(DateTime),
      chg = sum(W, S, na.rm = T), 
      chg = pmin(chg, 1), 
      chg = factor(chg, levels = c(0, 1), label = c('No', 'Yes')), 
      hr = factor(hr, levels = c(23:0))
    ) %>% 
    complete(Date, hr, fill = list(chg = 'No'))
  
  p <- ggplot(hrsum, aes(x = Date, y = hr, fill = chg)) +
    geom_tile() +
    scale_fill_manual(values = c('white', 'darkblue')) +
    scale_y_discrete(expand = c(0,0)) + 
    scale_x_date(expand = c(0, 0), date_breaks = 'day', date_labels = '%m-%d') + 
    labs(
      y = 'Hour of day', 
      x = NULL, 
      fill = 'Change?'
    ) + 
    theme_minimal() + 
    theme(
      legend.position = 'top', 
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(), 
      axis.text.x = element_text(size = 7, angle = 90, hjust = 1)
    )
  
  out <- ggplotly(p, width = 900)
  
  return(out)
  
}

# https://statistic-on-air.blogspot.com/2011/09/implementation-of-cdc-growth-charts-in.html
#Type:
#	(1) wac36 	= Weight-for-age charts, birth to 36 months
#	(2) lac36 	= Length-for-age charts, birth to 36 months
#	(3) wlc 	= Weight-for-recumbent length charts, birth to 36 months
#	(4) hac 	= Head circumference-for-age charts, birth to 36 months
#	(5) wsc 	= Weight-for-stature charts
#	(6) wac20 	= Weight-for-age charts, 2 to 20 years
#	(7) lac20 	= Stature-for-age charts, 2 to 20 years
#	(8) bac 	= BMI-for-age charts, 2 to 20 years
#	(9) bmi.adv	= wac20 + lac20 + bac (for pdf export)

MygrowthFun <- function( sex=c("m", "f"), 
                         type=c("wac36", "lac36", "wlc", "hac", "wsc", "wac20", "lac20", "bac", "bmi.adv"), 
                         path= here("Growth"),
                         name = NULL,
                         surname = NULL,
                         birth_date = NULL,
                         mydataAA = NULL){
  
  if( sex == "m" ) {
    switch(type,
           "wac36"	= source(paste(path, "grafici1m.R", sep="/")),
           "lac36"	= source(paste(path, "grafici2m.R", sep="/")),
           "wlc"	= source(paste(path, "grafici3m.R", sep="/")),
           "hac"	= source(paste(path, "grafici4m.R", sep="/")),
           "wsc"	= source(paste(path, "grafici5m.R", sep="/")),
           "wac20"	= source(paste(path, "grafici6m.R", sep="/")),
           "lac20"	= source(paste(path, "grafici7m.R", sep="/")),
           "bac"	= source(paste(path, "grafici8m.R", sep="/"))
    )
    if( type == "bmi.adv" ){
      source(paste(path, "grafici6m.R", sep=""))
      with(mydataAA, points(months, weight, type="l", col="red", lwd=2))
      source(paste(path, "grafici7m.R", sep=""))
      with(mydataAA, points(months, stature, type="l", col="red", lwd=2))
      source(paste(path, "grafici8m.R", sep=""))
      with(mydataAA, points(months, weight / (stature*stature/10000), type="l", col="red", lwd=2))
    }
  }
  if( sex == "f" ) {
    switch(type,
           "wac36"	= source(paste(path, "grafici1f.R", sep="/")),
           "lac36"	= source(paste(path, "grafici2f.R", sep="/")),
           "wlc"	= source(paste(path, "grafici3f.R", sep="/")),
           "hac"	= source(paste(path, "grafici4f.R", sep="/")),
           "wsc"	= source(paste(path, "grafici5f.R", sep="/")),
           "wac20"	= source(paste(path, "grafici6f.R", sep="/")),
           "lac20"	= source(paste(path, "grafici7f.R", sep="/")),
           "bac"	= source(paste(path, "grafici8f.R", sep="/"))
    )
    if( type == "bmi.adv" ){
      source(paste(path, "grafici6m.R", sep=""))
      with(mydataAA, points(months, weight, type="l", col="red", lwd=2))
      source(paste(path, "grafici7m.R", sep=""))
      with(mydataAA, points(months, stature, type="l", col="red", lwd=2))
      source(paste(path, "grafici8m.R", sep=""))
      with(mydataAA, points(months, weight / (stature*stature/10000), type="l", col="red", lwd=2))
    }
  }
  
  mtext(
    substitute(paste(bolditalic("Name: "), name), list(name=name)), side=1, outer=T, adj=0, line=-1)
  mtext(
    substitute(paste(bolditalic("Surname: "), surname), list(surname=surname)), side=1, outer=T, line=-1)
  mtext(
    substitute(paste(bolditalic("Birth Date: "), birth_date), list(birth_date=birth_date)), side=1, outer=T, adj=1, line=-1)
  
  points(mydataAA, type="l", col="red", lwd=2)
  
}