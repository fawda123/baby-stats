# summarise daily data
daysum_fun <- function(dat){
  
  out <- dat %>%
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
daysum_tab <- function(dat){
  
  totab <- daysum_fun(dat)
  
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
totsum_tab <- function(dat){
  
  totab <- daysum_fun(dat) %>% 
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
fdsum_plo <- function(dat){
  
  hrsum <- dat %>%
    mutate(
      Date = as.Date(DateTime, tz = 'EST'), 
      hr = hour(DateTime)
    ) %>% 
    group_by(Date, hr) %>% 
    summarise(
      `Length (min)` = sum(`Length (min)`, na.rm = T), 
      .groups = 'drop'
    ) %>% 
    complete(Date, hr, fill = list(`Length (min)` = NA))
  
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
      panel.grid.major = element_blank()
    )
  
  out <- ggplotly(p, width = 900)
  
  return(out)
  
}

# change summary plot
chsum_plo <- function(dat){
  
  hrsum <- dat %>%
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
    geom_tile(color = 'black') +
    scale_fill_manual(values = c('lightgrey', 'darkblue')) +
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
      panel.grid.major = element_blank()
    )
  
  out <- ggplotly(p, width = 900)
  
  return(out)
  
}