#' wqdat is input data
#' station is station character string
#'
plo_fun <- function(wqdat, station, cols, parms){

  # scale ranges
  rngs <- wqdat %>% 
    group_by(var) %>% 
    summarise(
      minv = min(val, na.rm = T), 
      maxv = max(val, na.rm = T),
      .groups = 'keep'
      ) %>% 
    nest %>% 
    deframe %>% 
    lapply(., unlist)
    
  # parameter labels
  lbs <- parms %>% 
    select(var, lbs) %>% 
    deframe
  
  # subset to statio
  toplo <- wqdat %>% 
    filter(station %in% !!station) %>% 
    spread(var, val)
  
  ylbsz <- 16
  
  p1 <- plot_ly(toplo) %>% 
    add_markers(x = ~date, y = ~tp, type = 'scatter', color = I(cols[6]), mode = 'markers', line = list(shape = 'linear'), showlegend = F) %>% 
    layout(
      yaxis = list(title = lbs['tp'], range = rngs$tp, titlefont = list(size = ylbsz))
      )

  p2 <- plot_ly(toplo) %>% 
    add_markers(x = ~date, y = ~tn, type = 'scatter', color = I(cols[5]), mode = 'markers', line = list(shape = 'linear'), showlegend = F) %>% 
    layout(
      yaxis = list(title = lbs['tn'], range = rngs$tn, titlefont = list(size = ylbsz))
      )
  
  p3 <- plot_ly(toplo) %>% 
    add_markers(x = ~date, y = ~nh3, type = 'scatter', color = I(cols[4]), mode = 'markers', line = list(shape = 'linear'), showlegend = F) %>% 
    layout(
      yaxis = list(title = lbs['nh3'], range = rngs$nh3, titlefont = list(size = ylbsz))
    )  
  
  p4 <- plot_ly(toplo) %>% 
    add_markers(x = ~date, y = ~chla, type = 'scatter', color = I(cols[3]), mode = 'markers', line = list(shape = 'linear'), showlegend = F) %>% 
    layout(
      yaxis = list(title = lbs['chla'], range = rngs$chla, titlefont = list(size = ylbsz))
      )  
    
  p5 <- plot_ly(toplo) %>% 
    add_markers(x = ~date, y = ~ph, type = 'scatter', color = I(cols[2]), mode = 'markers', line = list(shape = 'linear'), showlegend = F) %>% 
    layout(
      yaxis = list(title = lbs['ph'], range = rngs$ph, titlefont = list(size = ylbsz))
      )  
    
  p6 <- plot_ly(toplo) %>% 
    add_markers(x = ~date, y = ~sal, type = 'scatter', color = I(cols[1]), mode = 'markers', line = list(shape = 'linear'), showlegend = F) %>% 
    layout(
      yaxis = list(title = lbs['sal'], range = rngs$sal, titlefont = list(size = ylbsz))
      )

  p <- subplot(p1, p2, p3, p4, p5, p6, nrows = 6, shareX = T, shareY = F, titleY = T) %>% 
    layout(
      xaxis = list(title = NA, range = c(as.Date('1995-01-01'), as.Date('2022-01-01')))
    )
  
  return(p)

}