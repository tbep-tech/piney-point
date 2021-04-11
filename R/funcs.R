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
  
  ylbsz <- 13
  
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

# function for plotting macroalgae data
# modified from show_transect in tbpetools
show_macrotransect <- function(dat, station, genus = c("Acan", "Codium", "Grac/Acan", "Grac/Euch", "Grac/Haly", "Grad", 
                                                           "Hyp/Grac", "Laur", "Mixed Red", "Ulva", "Unknown"),
                          base_size = 12,
                          facet = FALSE, ncol = NULL, plotly = FALSE){
  
  # genus pool
  gen <- c("Acan", "Codium", "Grac/Acan", "Grac/Euch", "Grac/Haly", "Grad", 
           "Hyp/Grac", "Laur", "Mixed Red", "Ulva", "Unknown")

  # prep plot data
  dat <- dat %>%
    dplyr::filter(station %in% !!station) %>%
    dplyr::mutate(
      genus = factor(genus, levels = genus)
    ) %>%
    dplyr::mutate(
      Year = lubridate::year(date),
      location = as.numeric(location),
      pa = ifelse(weight_g == 0, 0, 1)
    ) 
  
  # sort color palette so its the same regardless of species selected
  gencol <- qualitative_hcl(length(gen), palette = "Harmonic")
  names(gencol) <- gen
  gencol <- gencol[levels(dat$genus)]
  
  # legend labels
  leglab <- 'Weight (g)'
  
  # data with species
  toplo1 <- dat %>%
    dplyr::filter(pa == 1) %>%
    dplyr::mutate(weight_g = round(weight_g, 1))
  
  # data w/o species, no facet
  toplo2 <- dat %>%
    group_by(date, location) %>%
    filter(sum(pa) == 0) %>%
    ungroup() %>%
    select(date, location) %>%
    unique()
  
  # data w/o species, facet
  toplo3 <- dat %>%
    dplyr::filter(pa == 0)
  
  if(!facet)
    p <- ggplot2::ggplot(toplo1, ggplot2::aes(y = date, x = location)) +
      ggplot2::geom_point(data = toplo2, alpha = 0.6, colour = 'darkgrey', size = 0.5) +
      ggplot2::geom_point(aes(size = weight_g, fill = genus), alpha = 0.6, pch = 21) +
      ggplot2::scale_fill_manual(values = gencol)
    
  if(facet)
    p <- ggplot2::ggplot(toplo1, ggplot2::aes(y = date, x = location, group = genus)) +
      ggplot2::geom_point(data = toplo3, alpha = 0.6, colour = 'darkgrey', size = 0.5) +
      ggplot2::geom_point(aes(size = weight_g, fill = genus), alpha = 0.6, pch = 21,) +
      ggplot2::scale_fill_manual(values = gencol) +
      ggplot2::guides(fill = FALSE) +
      ggplot2::facet_wrap(~genus, ncol = ncol)
  
  # finish plot
  p <- p +
    ggplot2::scale_size(breaks = as.numeric(levels(factor(toplo1$weight_g)))) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0)
    ) +
    ggplot2::labs(
      x = 'Transect distance (m)',
      title = paste0(station, ', ', leglab)
    )
  
  if(plotly)
    p <- plotly::ggplotly(p)
  
  return(p)
  
}