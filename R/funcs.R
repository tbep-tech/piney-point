g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  }

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
      yaxis = list(title = lbs['tp'], titlefont = list(size = ylbsz))#, range = rngs$tp)
      )

  p2 <- plot_ly(toplo) %>% 
    add_markers(x = ~date, y = ~tn, type = 'scatter', color = I(cols[5]), mode = 'markers', line = list(shape = 'linear'), showlegend = F) %>% 
    layout(
      yaxis = list(title = lbs['tn'], titlefont = list(size = ylbsz))#, range = rngs$tn)
      )
  
  p3 <- plot_ly(toplo) %>% 
    add_markers(x = ~date, y = ~nh34, type = 'scatter', color = I(cols[4]), mode = 'markers', line = list(shape = 'linear'), showlegend = F) %>% 
    layout(
      yaxis = list(title = lbs['nh34'], titlefont = list(size = ylbsz))#, range = rngs$nh34)
    )  
  
  p4 <- plot_ly(toplo) %>% 
    add_markers(x = ~date, y = ~chla, type = 'scatter', color = I(cols[3]), mode = 'markers', line = list(shape = 'linear'), showlegend = F) %>% 
    layout(
      yaxis = list(title = lbs['chla'], titlefont = list(size = ylbsz))#, range = rngs$chla)
      )  
    
  p5 <- plot_ly(toplo) %>% 
    add_markers(x = ~date, y = ~ph, type = 'scatter', color = I(cols[2]), mode = 'markers', line = list(shape = 'linear'), showlegend = F) %>% 
    layout(
      yaxis = list(title = lbs['ph'], titlefont = list(size = ylbsz))#, range = rngs$ph)
      )  
    
  p6 <- plot_ly(toplo) %>% 
    add_markers(x = ~date, y = ~sal, type = 'scatter', color = I(cols[1]), mode = 'markers', line = list(shape = 'linear'), showlegend = F) %>% 
    layout(
      yaxis = list(title = lbs['sal'], titlefont = list(size = ylbsz))#, range = rngs$sal)
      )

  p <- subplot(p1, p2, p3, p4, p5, p6, nrows = 6, shareX = T, shareY = F, titleY = T) %>% 
    layout(
      xaxis = list(title = NA, range = c(as.Date('1995-01-01'), as.Date('2022-01-01')))
    ) %>% 
    plotly::config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot"
      )
    )
  
  return(p)

}

# function for plotting rapid response transect data
# modified from show_transect in tbpetools
show_rstransect <- function(savdat, mcrdat, savsel, mcrsel, base_size = 12){

  savlevs <- c('Thalassia testudinum', 'Halodule wrightii', 'Syringodium filiforme', 'Ruppia maritima', 'Halophila engelmannii', 'Halophila decipiens')
  grplevs <- c('Red', 'Green', 'Brown', 'Cyanobacteria')
  abulabs <- c('<1%', '1-5%', '6-25%', '26-50%', '51-75%', '76-100%')
  abubrks <- c(0, 1, 2, 3, 4, 5)

  colpal <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, name = 'Dark2'))
  
  szrng <- c(2, 16)

  # xlims
  savxlms <- savdat %>%
    pull(location) %>% 
    unique %>% 
    sort
  
  mcrxlms <- mcrdat %>%
    pull(location) %>% 
    unique %>% 
    sort
  
  xlms <- range(savxlms, mcrxlms)
  
  # get dates for factor levels
  # this makes sure that y values on plots are shared
  dts1 <- savdat %>% 
    pull(date)
  dts2 <- mcrdat %>% 
    pull(date)
  dts <- c(dts1, dts1) %>% 
    unique %>%
    sort %>% 
    format('%b %d')

  # prep sav plot data
  savdatfrm <- savdat %>%
    dplyr::mutate(
      Year = lubridate::year(date),
      location = as.numeric(as.character(location)),
      pa = ifelse(bb == 0, 0, 1), 
      date = format(date, '%b %d'), 
      date = factor(date, levels = dts)
    ) %>% 
    dplyr::select(Year, date, location, taxa, abundance, pa, bb)
  
  # sort color palette so its the same regardless of species selected
  savcol <- colpal(length(savlevs))
  names(savcol) <- savlevs
  savcol <- savcol[savsel]
  
  # legend labels
  leglab <- 'Abundance (bb)'

  # data with species
  toplo1a <- savdatfrm %>%
    dplyr::filter(taxa %in% !!savsel) %>% 
    dplyr::filter(pa == 1) %>%
    dplyr::mutate(
      bb = round(bb, 1),
      tltp = paste0(taxa, ', ', abundance)
      ) %>% 
    dplyr::arrange(date, location)

  # find overplots
  dups1 <- duplicated(toplo1a[, c('date', 'location')])
  dups2 <- duplicated(toplo1a[, c('date', 'location')], fromLast = T)
  dups <- apply(cbind(dups1, dups2), 1, any)
  toplo1a <- toplo1a %>% 
    mutate(
      dups = dups
    ) %>% 
    group_by(date, location) %>% 
    mutate(
      location = case_when(
        dups ~ location + seq(-1 * length(dups) / 3, length(dups) / 3, length.out = length(dups)), 
        T ~ location
      )
    ) %>% 
    ungroup()
  
  # data w/o species, no facet
  toplo2a <- savdatfrm %>%
    group_by(date, location) %>%
    filter(sum(pa) == 0) %>%
    ungroup() %>%
    select(date, location) %>%
    unique()
  
  pa <- ggplot2::ggplot(toplo1a, ggplot2::aes(y = date, x = location)) +
    ggplot2::geom_point(data = toplo2a, alpha = 1, colour = 'black', size = 2) +
    ggplot2::geom_point(aes(size = bb, fill = taxa), alpha = 0.8, pch = 21) +
    ggplot2::scale_fill_manual(values = savcol) +
    ggplot2::scale_radius(limits = range(abubrks), labels = abulabs, breaks = abubrks, range = szrng) +
    ggplot2::theme_minimal(base_size = base_size, base_family = 'Roboto') +
    ggplot2::scale_y_discrete(limits = dts, breaks = dts) + 
    ggplot2::scale_x_continuous(breaks = savxlms) +
    ggplot2::coord_cartesian(xlim = xlms) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0), 
      axis.title.y = element_blank(), 
      axis.title.x = element_blank()
    ) +
    ggplot2::labs(
      
      x = 'Transect distance (m)',
      title = 'Submerged aquatic vegetation'
    ) + 
    guides(fill = guide_legend(override.aes = list(size = 7), order = 1))

  # prep mcr plot data
  mcrdatfrm <- mcrdat %>%
    dplyr::mutate(
      Year = lubridate::year(date),
      location = as.numeric(as.character(location)),
      pa = ifelse(bb == 0, 0, 1), 
      date = format(date, '%b %d'), 
      date = factor(date, levels = dts)
    ) %>% 
    dplyr::select(Year, date, location, taxa, abundance, pa, bb)
  
  # sort color palette so its the same regardless of species selected
  mcrcol <- c('tomato1', 'lightgreen', 'burlywood3', 'lightblue')
  names(mcrcol) <- grplevs
  mcrcol <- mcrcol[mcrsel]
  
  # legend labels
  leglab <- 'Abundance (bb)'
  
  # data with species
  toplo1b <- mcrdatfrm %>%
    dplyr::filter(taxa %in% mcrsel) %>% 
    dplyr::filter(pa == 1) %>%
    dplyr::mutate(
      bb = round(bb, 1),
      tltp = paste0(taxa, ', ',  abundance)
      )
  
  # jitter duplicates
  dups1 <- duplicated(toplo1b[, c('date', 'location')])
  dups2 <- duplicated(toplo1b[, c('date', 'location')], fromLast = T)
  dups <- apply(cbind(dups1, dups2), 1, any)
  toplo1b <- toplo1b %>% 
    mutate(
      dups = dups
    ) %>% 
    group_by(date, location) %>% 
    mutate(
      location = case_when(
        dups ~ location + seq(-1 * length(dups) / 3, length(dups) / 3, length.out = length(dups)), 
        T ~ location
      )
    ) %>% 
    ungroup()

  # data w/o species, no facet
  toplo2b <- mcrdatfrm %>%
    group_by(date, location) %>%
    filter(sum(pa) == 0) %>%
    ungroup() %>%
    select(date, location) %>%
    unique()
  
  pb <- ggplot2::ggplot(toplo1b, ggplot2::aes(y = date, x = location)) +
    ggplot2::geom_point(data = toplo2b, colour = 'black', alpha = 1, size = 2) +
    ggplot2::geom_point(inherit.aes = F, aes(colour = 'Empty sample'), x = NA, y = NA) +
    ggplot2::geom_point(aes(size = bb, fill = taxa), alpha = 0.8, pch = 21) +
    ggplot2::scale_fill_manual(values = mcrcol) +
    ggplot2::scale_colour_manual(values = 'black') +
    ggplot2::scale_radius(limits = range(abubrks), labels = abulabs, breaks = abubrks, range = szrng, guide = F) +
    ggplot2::theme_minimal(base_size = base_size, base_family = 'Roboto') +
    ggplot2::scale_y_discrete(limits = dts, breaks = dts) + 
    ggplot2::scale_x_continuous(breaks = mcrxlms) +
    ggplot2::coord_cartesian(xlim = xlms) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0), 
      axis.title.y = element_blank()
    ) +
    ggplot2::labs(
      x = 'Transect distance (m)', 
      title = 'Macroalgae'
    ) +
    guides(
      fill = guide_legend(override.aes = list(size = 7), order = 1), 
      colour = guide_legend(override.aes = list(size = 2))
      )
  
  # out
  p <- pa + pb + plot_layout(ncol = 1, heights = c(0.9, 1), guides = 'collect')
  
  return(p)
  
}

# function for creating popup plot with mapview click
rswqpopup_plo <- function(station, datin){
  
  # monitoring data
  toplo1 <- datin %>% 
    filter(station == !!station) %>% 
    mutate(
      minrng = gsub('(^.*)\\-.*$', '\\1', nrmrng), 
      maxrng = gsub('^.*\\-(.*)$', '\\1', nrmrng)
    ) %>% 
    select(-nrmrng) %>% 
    mutate(
      minrng = as.numeric(minrng),
      maxrng = as.numeric(maxrng)
    )

  # reference data
  toplo2 <- toplo1 %>% 
    select(date, minrng, maxrng) %>%
    mutate(
      mo = lubridate::month(date)
    ) %>% 
    group_by(mo) %>% 
    mutate(
      datestr = floor_date(date, unit = 'month'),
      dateend = ceiling_date(date, unit = 'month')
    ) %>% 
    ungroup %>% 
    select(-date) %>% 
    unique
  
  ylb <- unique(toplo1$lbs)
  minrng <- unique(toplo1$minrng)
  maxrng <- unique(toplo1$maxrng)
  ylm <- range(c(minrng, maxrng, toplo1$val), na.rm = TRUE)

  out <- ggplot() + 
    geom_line(data = toplo1, aes(x = date, y = val)) + 
    geom_point(data = toplo1, aes(x = date, y = val), size = 2) + 
    geom_rect(data = toplo2, aes(xmin = datestr, xmax = dateend, ymin = minrng, ymax = maxrng, fill = 'Monthly normal range', group = mo), alpha = 0.2) +
    coord_cartesian(ylim = ylm, xlim = range(toplo1$date)) + 
    scale_fill_manual(values = 'blue') +
    theme_minimal(base_size = 18) + 
    theme(
      legend.title = element_blank(),
      axis.title.x = element_blank(), 
      legend.position = 'top'
    ) +
    labs(
      y = ylb, 
      subtitle = paste('Station', station)
    )
  
  return(out)
  
}

# estimate TBBI, used in dat_proc, source code from tbeptools
tbbi_fun <- function(datin, salin){

  taxdat <- datin %>% 
    select(date = Date, station = StationNumber, FAMILY, NAME, TaxaCount = Count, AdjCount) %>% 
    filter(FAMILY != 'NULL') %>% 
    mutate(
      date = as.Date(date)
    )
  
  # taxa counts aggregated by station & taxa list id
  taxasums <- taxdat %>%
    dplyr::group_by(station, FAMILY, NAME) %>%
    dplyr::summarise(
      SumofCount = sum(TaxaCount, na.rm = T),
      SumofAdjCount = sum(AdjCount, na.rm = T),
      .groups = 'drop'
    )
  
  # biology stats aggregated by station
  biostats <- taxasums %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(
      SpeciesRichness = length(na.omit(NAME)),
      RawCountAbundance = sum(SumofCount, na.rm = T),
      AdjCountAbundance = sum(SumofAdjCount, na.rm = T),
      .groups = 'drop'
    )
  
  spionid <- taxasums %>%
    dplyr::filter(FAMILY %in% 'Spionidae') %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(SpionidAbundance = sum(SumofAdjCount, na.rm = T), .groups = 'drop')
  capitellid <-  taxasums %>%
    dplyr::filter(FAMILY %in% 'Capitellidae') %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(CapitellidAbundance = sum(SumofAdjCount, na.rm = T), .groups = 'drop')
  
  # calculate biology populations/abundance by station
  biostatspopulation <- biostats %>%
    dplyr::left_join(spionid, by = 'station') %>%
    dplyr::left_join(capitellid, by = 'station') %>%
    dplyr::mutate(
      Salinity = salin,
      StandPropLnSpecies = dplyr::case_when(
        is.na(SpeciesRichness) | is.na(Salinity) ~ 0,
        T ~ ((log(SpeciesRichness + 1) / log(10))
             / ((( 3.2983 - 0.23576 * Salinity ) +  0.01081 * Salinity^2) - 0.00015327 * Salinity^3)
             - 0.84227
        ) / 0.18952
      ),
      SpeciesRichness = ifelse(is.na(SpeciesRichness), 0, SpeciesRichness),
      RawCountAbundance = ifelse(is.na(RawCountAbundance), 0, RawCountAbundance),
      TotalAbundance = ifelse(is.na(AdjCountAbundance), 0, AdjCountAbundance),
      SpionidAbundance = ifelse(is.na(SpionidAbundance), 0, SpionidAbundance),
      CapitellidAbundance = ifelse(is.na(CapitellidAbundance), 0, CapitellidAbundance)
    ) %>%
    dplyr::select(station, SpeciesRichness, RawCountAbundance, TotalAbundance, SpionidAbundance, CapitellidAbundance, StandPropLnSpecies)
  
  biostatstbbi <- biostatspopulation %>%
    dplyr::mutate(
      TBBI = dplyr::case_when(
        CapitellidAbundance == 0 & SpionidAbundance != 0 ~
          (((-0.11407) + (StandPropLnSpecies * 0.32583 ) +
              (((asin(SpionidAbundance / TotalAbundance) - 0.11646 ) / (0.18554)) *
                 (-0.1502)) + ((-0.51401) * (-0.60943))) - (-3.3252118)) / (0.7578544 + 3.3252118),
        
        CapitellidAbundance != 0 & SpionidAbundance == 0 ~
          (((-0.11407) + (StandPropLnSpecies * 0.32583) + ((-0.62768) * (-0.1502)) +
              ((( asin( CapitellidAbundance / TotalAbundance) - 0.041249) / 0.08025) *
                 (-0.60943))) - (-3.3252118)) / (0.7578544 + 3.3252118),
        
        CapitellidAbundance == 0 & SpionidAbundance == 0 & TotalAbundance != 0 ~
          (((-0.11407) + (StandPropLnSpecies * 0.32583) + ((-0.62768) * (-0.1502)) +
              ((-0.51401) * (-0.60943))) - (-3.3252118)) / ( 0.7578544 + 3.3252118),
        
        TotalAbundance == 0 ~ 0,
        T ~ ((( -0.11407) + (StandPropLnSpecies * 0.32583) +
                (((asin(SpionidAbundance / TotalAbundance) - 0.11646) / 0.18554) * (-0.1502)) +
                (((asin( CapitellidAbundance / TotalAbundance) - 0.041249) / 0.08025) *
                   (-0.60943))) - (-3.3252118)) / (0.7578544 + 3.3252118)
      ),
      TBBI = round(100 * TBBI, 2)
    ) %>%
    dplyr::select(station, TotalAbundance, SpeciesRichness, TBBI) %>%
    dplyr::filter(!is.na(station))
  
  dts <- taxdat %>% 
    select(station, date) %>% 
    unique
  
  # family sums
  taxfams <- taxdat %>% 
    group_by(station, FAMILY) %>% 
    summarise(
      TotalAbundance = sum(AdjCount, na.rm = T), 
      .groups = 'drop'
    ) %>% 
    group_by(station) %>% 
    nest() %>% 
    rename(family_abu = data)
  
  # total abundance is individuals/m2
  out <- biostatstbbi %>%
    dplyr::mutate(
      TBBICat = dplyr::case_when(
        TBBI == 0 ~ 'Empty Sample',
        TBBI < 73 ~ 'Degraded',
        TBBI >= 73 & TBBI < 87 ~ 'Intermediate',
        TBBI >= 87 ~ 'Healthy',
        T ~ NA_character_
      ), 
      col = dplyr::case_when(
        TBBICat == 'Empty Sample' ~ 'grey', 
        TBBICat == 'Degraded' ~ 'red', 
        TBBICat == 'Intermediate' ~ 'yellow', 
        TBBICat == 'Healthy' ~ 'darkgreen', 
      )
    ) %>% 
    left_join(dts, ., by = 'station') %>% 
    mutate(
      modt = paste0(as.character(month(date, abbr = F, label = T)), ' 2021')
    ) %>% 
    left_join(taxfams, by = 'station')
  
  return(out)

}

#' Convert DO into % saturation for 1-m depth 
#' Use convention of expressing saturation at 1 atm.
#' data(sfbay)
#'
#' @param do, dissolved oxygen mg/l 
#' @param t tem temperature, degrees C
#' @param S salinity, on the Practical Salinity Scale
#' @param P pressure, atm
DOsat <- function (do, t, S, P = NULL)
{
  T = t + 273.15
  lnCstar = -139.34411 + 157570.1/T - 66423080/T^2 + 1.2438e+10/T^3 -
    862194900000/T^4 - S * (0.017674 - 10.754/T + 2140.7/T^2)
  Cstar1 <- exp(lnCstar)
  if (is.null(P)) {
    out <- Cstar1
  }
  else {
    Pwv = (1 - 0.000537 * S) * exp(18.1973 * (1 - 373.16/T) +
                                     3.1813e-07 * (1 - exp(26.1205 * (1 - T/373.16))) -
                                     0.018726 * (1 - exp(8.03945 * (1 - 373.16/T))) +
                                     5.02802 * log(373.16/T))
    theta = 0.000975 - 1.426e-05 * t + 6.436e-08 * t^2
    out <- Cstar1 * P * (1 - Pwv/P) * (1 - theta * P)/((1 - Pwv) *
                                                  (1 - theta))
  }
  
  out <- 100 * do / Cstar1
  
  return(out)
  
}
