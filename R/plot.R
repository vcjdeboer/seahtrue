# background
#' Plot Raw background. Visualizes the fluorescent emission (Au) of each background well against time.
#'
#' @param var The type of Raw Emission correction, "O2_em_corr" or "pH_em_corr".
#' @param xf_raw_pr The preprocessed Raw data sheet of the Seahorse Excel file (converted from .asyr)
#' @param O2_targetEmission Target emission of the O2 (Usually 12500 Fluorescence Emission (Au)).
#' @param pH_targetEmission Target emission of the pH (Usually 30000 Fluorescence Emission (Au)).
#' @param flnme Name of the imported Seahorse file.
#' @return A variable gg_plot which contains information about how to plot/visualise the fluorescent emission (Au) of each background well against time.

#' @examples plot_raw_BKGD("O2_em_corr", xf_plate_pr$raw_data[[1]], 12500, 30000, "20191219_SciRep_PBMCs_donor_A")

plot_raw_BKGD <- function(var, xf_raw_pr, O2_targetEmission, pH_targetEmission, flnme){
  logger::log_info("Plot Raw background. Visualizes the fluorescent emission (Au) of each background well against time.")

  theme_maxTick <<- function(){
    theme_classic(base_size = 10) %+replace%
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_line(),
            axis.ticks.y = element_line(),
            axis.line.y = element_line(),
            legend.text = element_text(size = rel(1)),
            legend.title = element_text(size = rel(1)),
            axis.title.x = element_text(size = rel(1)),
            axis.title.y = element_text(size = rel(1), angle = 90)
      )
  }

  custom.col <- c("#D16103","#4E84C4","#52854C","#C4961A",  "#FFDB6D", "#C4961A", "#F4EDCA",
                           "#EF0E0F", "#557577", "#2F1708", "#4D88CC", "#8EB33C", "#F1A927", "#70D8EA",
                           "#382A40", "#C8B280", "#84A76B", "#303762", "#E194AE", "#00000", "#C8CFCB",
                           "#B293B9", "#9B4430")

  df<- xf_raw_pr %>% dplyr::filter(group == "Background") %>%
    dplyr::select(measurement, well, group,timescale, minutes,tick, emission = .data[[var]])
  df <- df[!is.na(df$emission), ]

  if (var == "O2_em_corr"){targetEMS<-  O2_targetEmission} else {targetEMS <- pH_targetEmission}

  gg_plot = ggplot2::ggplot(df, aes(x=minutes, y=emission, color = well))+
    ggiraph::geom_point_interactive(mapping = aes(colour  = well, tooltip = well, data_id = well), alpha = 0.5, size = 3)+
    ggiraph::geom_hline_interactive(yintercept = targetEMS, linetype = "dashed", color = "#D16103")+
    theme_maxTick()+
    ggplot2::labs(title = var,
                  subtitle = flnme,
                  x = "time (min)",
                  y = "emission (AU)")+
    ggplot2::geom_smooth(span = 0.75) +
    ggplot2::scale_color_hue() +
    ggplot2::theme(plot.title = element_text(hjust = 2, size = 10),
                   plot.subtitle = element_text(hjust = 0.5, size = 10))

  return(girafe(ggobj = gg_plot))


}

#' Visualizes the mean fluorescence emission (Au) of each background well against time.
#'
#' @param var The type of Raw Emission correction, "O2_em_corr" or "pH_em_corr"
#' @param xf_raw_pr The preprocessed Raw data sheet of the Seahorse Excel file (converted from .asyr)
#' @param O2_targetEmission Target emission of the O2 (Usually 12500 Fluorescence Emission (Au))
#' @param pH_targetEmission Target emission of the pH (Usually 30000 Fluorescence Emission (Au))
#' @param flnme Name of the imported Seahorse file.
#' @return a variable gg_plot which contains information about how to plot/visualise the mean fluorescence emission (Au) of each background well against time.
#'
#' @examples plot_raw_BKGD_means("O2_em_corr", xf_plate_pr$raw_data[[1]], 12500, 30000, "20191219_SciRep_PBMCs_donor_A")
#'
plot_raw_BKGD_means <- function(var, xf_raw_pr, O2_targetEmission, pH_targetEmission, flnme){

  logger::log_info("Visualizes the mean fluorescence emission (Au) of each background well against time.")
  theme_maxTick <- function(){
    theme_classic(base_size = 15) %+replace%
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_line(),
            axis.ticks.y = element_line(),
            axis.line.y = element_line(),
            legend.text = element_text(size = rel(0.7)),
            legend.title = element_text(size = rel(0.7)),
            axis.title.x = element_text(size = rel(0.8)),
            axis.title.y = element_text(size = rel(0.8), angle = 90)
      )
  }

  custom.col <- c("#D16103","#4E84C4","#52854C","#C4961A",  "#FFDB6D", "#C4961A", "#F4EDCA",
                           "#EF0E0F", "#557577", "#2F1708", "#4D88CC", "#8EB33C", "#F1A927", "#70D8EA",
                           "#382A40", "#C8B280", "#84A76B", "#303762", "#E194AE", "#00000", "#C8CFCB",
                           "#B293B9", "#9B4430")

  df<- xf_raw_pr %>% dplyr::filter(group == "Background") %>%
    dplyr::select(measurement, well, group,timescale, minutes,tick, emission = .data[[var]])
  df <- df[!is.na(df$emission), ]


  mean_O2_em_corr <- df %>% dplyr::group_by(minutes) %>%
    dplyr::summarize(O2_em_corr_mean = mean(emission),
                     sd = sd(emission))


  if (var == "O2_em_corr"){targetEMS<-  O2_targetEmission} else {targetEMS <- pH_targetEmission}

  gg_plot = ggplot(mean_O2_em_corr, aes(x = minutes, y = O2_em_corr_mean))+
    ggiraph::geom_point_interactive(mapping = aes(x = minutes, y = O2_em_corr_mean, data_id = O2_em_corr_mean), alpha = 0.5, size = 3)+
    ggplot2::geom_errorbar(mapping = aes(x = minutes,
                                         y = O2_em_corr_mean,
                                         ymin = O2_em_corr_mean - sd,
                                         ymax = O2_em_corr_mean + sd),
                           width = 0,
                           color = "#293352",
                           alpha = 0.5,
                           size = 0.3)+
    ggplot2::geom_hline(yintercept = targetEMS, linetype = "dashed", color = "#D16103")+
    theme_maxTick()+
    ggplot2::scale_color_manual(values = custom.col)+
    ggplot2::labs(title = var,
                  subtitle = flnme,
                  x = "time (min)",
                  y = "emission (AU)")+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 15),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10))+
    ggplot2::geom_smooth(method = "lm")

  return(girafe(ggobj = gg_plot))
}

#' Visualizes the fluorescence emission (Au) of each background well and is based on the area under the curve (auc).
#'
#' @param var The type of Raw Emission correction, "O2_em_corr" or "pH_em_corr"
#' @param auc_var The selected aus var (auc or auc2)
#' @param xf_raw_pr The preprocessed Raw data sheet of the Seahorse Excel file (converted from .asyr)
#' @param O2_targetEmission Target emission of the O2 (Usually 12500 Fluorescence Emission (Au))
#' @param pH_targetEmission Target emission of the pH (Usually 30000 Fluorescence Emission (Au))
#' @param file_name Name of the imported Seahorse file
#' @return a variable gg_plot which contains information about how to plot/visualise the the fluorescence emission (Au) of each background well, based on the area under the curve (auc).
#'
#' @examples plot_BKGD_auc_facet("O2_em_corr", "auc", xf_plate_pr$raw_data[[1]], 12500, 30000, "20191219_SciRep_PBMCs_donor_A")
plot_BKGD_auc_facet <- function(var, auc_var, xf_raw_pr, O2_targetEmission, pH_targetEmission, file_name){
  logger::log_info("Visualizes the fluorescence emission (Au) of each background well and is based on the area under the curve (auc).")
  theme_maxTick <- function(){
    theme_classic(base_size = 10) %+replace%
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_line(),
            axis.ticks.y = element_line(),
            axis.line.y = element_line(),
            legend.text = element_text(size = rel(0.7)),
            legend.title = element_text(size = rel(0.7)),
            axis.title.x = element_text(size = rel(0.8)),
            axis.title.y = element_text(size = rel(0.8), angle = 90)
      )
  }

  custom.col <- c("#D16103","#4E84C4","#52854C","#C4961A",  "#FFDB6D", "#C4961A", "#F4EDCA",
                           "#EF0E0F", "#557577", "#2F1708", "#4D88CC", "#8EB33C", "#F1A927", "#70D8EA",
                           "#382A40", "#C8B280", "#84A76B", "#303762", "#E194AE", "#00000", "#C8CFCB",
                           "#B293B9", "#9B4430")

  #' Get background area under curve.
  #'
  #' @param xf_raw_pr The preprocessed Raw data sheet of the Seahorse Excel file (converted from .asyr)
  #' @param var The type of Raw Emission correction, "O2_em_corr" or "pH_em_corr".
  #' @param targetEMS target Emission.
  #' @return requires description
  #' #'
  #' @param x temp_df$timescale (exp. [1]   0  15  31  47  63  79  95 110 126 142 158 173
  #'                                  [1] 328 344 360 375 391 407 423 439 455 470 486 502 etc.)
  #'
  #' @param y temp_df$emission (exp. [1] 12422.08 12384.71 12365.38 12343.05 12321.03 12307.01 12295.71 12280.69 12270.68 12256.66 12246.65 12235.64
  #'                                 [1] 12466.85 12424.47 12397.12 12377.10 12357.78 12338.76 12324.74 12307.43 12295.42 12286.41 12273.40 12260.39 etc.)
  #'
  #' @return auc_func (exp. [1] 1.038633, [1] 1.138149, [1] 0.9902363 etc.)
  #'
  #' examples background_drift(temp_df$timescale, temp_df$emission)
  #'
  get_BKGD_auc <- function(xf_raw_pr, var, targetEMS){

    background_drift <- function(x,y){
      y <- y/min(y)-1
      auc_func <- bayestestR::area_under_curve(x, y, method = "trapezoid")
      return(auc_func)
    }
    #' Title
    #'
    #' @param baseline target Emission
    #'
    #' @return auc_func background area under curve (exp. [1] 1.038633, [1] 1.138149, [1] 0.9902363 etc.)
    #' @keywords internal
    #'
    #' examples background_drift2(temp_df$timescale, temp_df$emission, targetEMS)
    background_drift2 <- function(x,y, baseline){
      y <- y-baseline
      auc_func <- bayestestR::area_under_curve(x, y, method = "trapezoid")
      return(auc_func)
    }

    df<- xf_raw_pr %>% dplyr::filter(group == "Background") %>%
      dplyr::select(measurement, well, group,timescale, tick, emission = .data[[var]])
    df <- df[!is.na(df$emission), ]

    temp_df <- NULL
    temp_auc <- tibble::tibble(well = 0, measurement = 0, auc = 0)
    AUC_bkgd <- NULL
    wellList <- unique(as.vector((df$well)))
    measurementList <- unique(as.vector((df$measurement)))
    for (j in wellList){
      for (k in measurementList){
        temp_df <- df %>% dplyr::filter(well == j & measurement == k)
        temp_auc$auc <- background_drift(temp_df$timescale, temp_df$emission)
        temp_auc$auc2 <- background_drift2(temp_df$timescale, temp_df$emission, targetEMS)
        temp_auc$well <- j
        temp_auc$measurement <- k

        AUC_bkgd <- rbind(AUC_bkgd, temp_auc)
      }
    }
    return(AUC_bkgd)
  }

  #' Title: make_raw_em_corr_BKGD_plot.
  #'
  #' @param AUC_bkgd Area Under Curve background information
  #' @param var The type of Raw Emission correction, "O2_em_corr" or "pH_em_corr"
  #' @param auc_var The selected aus var (auc or auc2)
  #' @param flnme Name of the imported Seahorse file
  #'
  #' @return a variable gg_plot which contains information about how to plot/visualise the the fluorescence emission (Au) of each background well, based on the area under the curve (auc).
  make_raw_em_corr_BKGD_plot <- function(AUC_bkgd, var, auc_var, flnme){
    logger::log_info("Plot ")

    gg_plot <- ggplot(data = AUC_bkgd,
                      mapping = aes(x = measurement, y = BKG_auc))+
      geom_smooth(method = 'loess', formula = 'y~x', se=F, color = "#293352")+
      geom_col(alpha = 0.5, aes(fill = well)) +
      scale_fill_manual(values=custom.col)+
      facet_wrap(~well, nrow = 2)+
      theme_maxTick()+
      labs(title = paste0(var, " - ", auc_var),
           subtitle = flnme,
           x = "measurement",
           y = "auc background wells (AU)")+
      theme(plot.title = element_text(hjust = 0.5, size = 18),
            plot.subtitle = element_text(hjust = 0.5, size = 10))+
      scale_x_continuous(limits = c(0,max(AUC_bkgd$measurement)+1),
                         breaks = seq(1, max(AUC_bkgd$measurement), by = 1),
                         expand = c(0, -0.3))
    return(gg_plot)


  }

  if (var == "O2_em_corr"){targetEMS<-  O2_targetEmission} else {targetEMS <- pH_targetEmission}

  df_forPlot <- get_BKGD_auc(xf_raw_pr, var, targetEMS) %>% select(well, measurement, BKG_auc = .data[[auc_var]])

  gg_plot <- make_raw_em_corr_BKGD_plot(df_forPlot, var, auc_var, file_name)

  return(gg_plot)

}

#plate layout
#' Title: Visualises the layout of the Seahorse plate, through different colors.
#'
#' @param XFe96data A Raw data sheet from a Seahorse Excel file (converted from .asyr file) which is put in a variable called XFe96data.
#' @param flnme Name of the imported Seahorse file.
#'
#' @return variable gg_plot which contains information about to plot the plate layout.
#'
# @examples map_groupMaker(xf_plate_pr$raw_data[[1]], "20191219_SciRep_PBMCs_donor_A")
map_groupMaker <- function(XFe96data, flnme){

  logger::log_info("Visualises the layout of the Seahorse plate, through different colors.")
  wholeWell <- XFe96data

  theme_htmp<- function(){
    theme_bw(base_size = 60) %+replace%
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_line(),
            axis.ticks.y = element_line(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            legend.position = "bottom",
            legend.direction = "vertical",
            legend.justification = "center",
            legend.key.size = unit(5, 'cm') #change legend key size
      )

  }

  wholeWell$well2 <- wholeWell$well
  wholeWell_RC <- wholeWell %>%  separate(well2, into = c("row", "column"), sep = 1, convert = TRUE)
  wholeWell_measurement <- filter(wholeWell_RC, measurement == 1)

  nb.cols <- nlevels(as.factor(wholeWell$group))
  groupColors3 <- colorRampPalette(brewer.pal(8, "BrBG"))(nb.cols)
  #map
  gg_plot <- ggplot(data = wholeWell_measurement, aes(x = column, y = row)) +
    geom_tile_interactive(aes(fill = as.factor(group)), color = "grey50",show.legend = TRUE)+
    scale_fill_manual(values= groupColors3 )+
    labs(title = flnme)+
    scale_x_continuous(limits= c(0.5, 13),
                       breaks = c(1:12),
                       position = "top")+
    scale_y_discrete(limits = rev(levels(as.factor(wholeWell_measurement$row))))+
    theme_htmp()

  return(girafe(
    ggobj = gg_plot,
    width_svg = (60),
    height_svg = (40)
  ))
}


# first tick emission (facet_wrap)
#' Title: Visualizes the fluorescence emission (Au) of each well for different Seahorse groups.
#'
#' @param var The type of Raw Emission correction, "O2_em_corr" or "pH_em_corr"
#' @param xf_raw_pr The preprocessed Raw data sheet of the Seahorse Excel file (converted from .asyr)
#' @param O2_targetEmission Target emission of the O2 (Usually 12500 Fluorescence Emission (Au))
#' @param pH_targetEmission Target emission of the pH (Usually 30000 Fluorescence Emission (Au))
#' @param flnme Name of the imported Seahorse file.
#'
#' @return variable gg which contains information about how to plot/visualise fluorescence emission (Au) of each well for different Seahorse groups.
#'
# @examples plot_group_emissions("O2_em_corr", xf_plate_pr$raw_data[[1]], 12500, 30000, "20191219_SciRep_PBMCs_donor_A")
plot_group_emissions <- function(var, xf_raw_pr, O2_targetEmission, pH_targetEmission, flnme){
  logger::log_info("Visualizes the fluorescence emission (Au) of each well for different Seahorse groups.")

  theme_maxTick <- function(){
    theme_classic(base_size = 10) %+replace%
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_line(),
            axis.ticks.y = element_line(),
            axis.line.y = element_line(),
            legend.text = element_text(size = rel(0.7)),
            legend.title = element_text(size = rel(0.7)),
            axis.title.x = element_text(size = rel(0.8)),
            axis.title.y = element_text(size = rel(0.8), angle = 90)
      )
  }

  df <- xf_raw_pr %>% select(measurement, well, group, timescale, minutes, tick, emission = .data[[var]] )
  df <- df[!is.na(df$emission), ]
  df <- df[!df$group == "Background",]
  df <- df %>% group_by(well,measurement) %>%
    slice(which.min(tick))


  if (var == "O2_em_corr"){targetEMS<-  O2_targetEmission} else {targetEMS <- pH_targetEmission}

  mean_O2_em_corr <- df %>% group_by(group,measurement) %>%
    summarize(m_em = mean(emission),
              sd_em = sd(emission))


  gg <- ggplot(data = df,
               mapping = aes(x = measurement, y = emission, group = measurement))+
    geom_line_interactive(aes(x = measurement, y = emission, group = well, data_id = df$well, tooltip = df$well))+
    geom_point(data = mean_O2_em_corr, mapping = aes(x = measurement,
                                                     y = m_em),
               color = "#293352")+
    geom_errorbar(data = mean_O2_em_corr, mapping = aes(x = measurement,
                                                        y = m_em,
                                                        ymin = m_em - sd_em,
                                                        ymax = m_em + sd_em),
                  width = 0.5,
                  color = "#293352",
                  alpha = 1,
                  size = 1)+
    geom_hline(yintercept = targetEMS, linetype = "dashed", color = "#D16103")+
    scale_x_continuous(limits = c(0,max(df$measurement)+1),
                       breaks = seq(1, max(df$measurement), by = 1),
                       expand = c(0, -0.3))+
    theme_maxTick()+
    labs(title = var,
         subtitle = flnme,
         x = "measurement",
         y = "emission (AU)")+
    theme(plot.title = element_text(hjust = 0.5, size = 5)) +
  #       plot.subtitle = element_text(hjust = 0.5, size = 5))+
  facet_wrap(~group, nrow = 2)

  return(gg)
}

# range plot emission (facet_wrap)
#' Title: Visualises the oxygen concentration (mmHg) of the background wells within a range plot.
#'
#' @param var The type of Raw Emission correction, "O2_em_corr" or "pH_em_corr"
#' @param df The preprocessed Raw data sheet of the Seahorse Excel file (converted from .asyr).
#' @param flnme Name of the imported Seahorse file.
#'
#' @return variable gg_plot which contains information about how to plot/visualise the oxygen concentration (mmHg) of the background wells within a range plot.
#'
# @examples make_range_plot("O2_em_corr", xf_plate_pr$raw_data[[1]], "20191219_SciRep_PBMCs_donor_A")
make_range_plot <- function(var, df, flnme){
  logger::log_info("Visualises the oxygen concentration (mmHg) of the background wells within a range plot.")

  theme_maxTick <- function(){
    theme_classic(base_size = 15) %+replace%
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_line(),
            axis.ticks.y = element_line(),
            axis.line.y = element_line(),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 10, angle = 90),
            plot.title=element_text(size=12),
            axis.title=element_text(size=12) )}

  get_range <- function(var, xf_raw_pr){

    df<- xf_raw_pr %>% filter(!group == "Background") %>%
      select(measurement, well, group,timescale, tick, minutes, interval, param = .data[[var]])
    df <- df[!is.na(df$param), ]

    temp_df <- NULL
    temp_range <- tibble(group = 0, measurement = 0, interval = 0, well = 0, min = 0, max = 0, range =0)
    range_allGroups <- NULL
    wellList <- unique(as.vector((df$well)))
    measurementList <- unique(as.vector((df$measurement)))
    for (g in wellList){
      for (k in measurementList){
        temp_df <- df %>% filter(measurement == k & well ==g)
        temp_range$min <- min(temp_df$param)
        temp_range$max<- max(temp_df$param)
        temp_range$range<- temp_range$max - temp_range$min
        temp_range$group <- temp_df$group[1]
        temp_range$interval <- temp_df$interval[1]
        temp_range$measurement <- k
        temp_range$well <- g
        range_allGroups <- rbind(range_allGroups, temp_range)
      }
    }

    mean_range_measurement <- range_allGroups %>% group_by(group,measurement) %>%
      summarize(m_min = mean(min),
                m_max = mean(max),
                m_range = mean(range))


    return(mean_range_measurement)
  }

  # var <- param_toPlot
  # df <- df_seahorse$Total
  range_allGroups <- get_range(var,df)

  mean_range <- range_allGroups

  injection_maxes <- df %>% dplyr::group_by(interval) %>% summarize(max = max(measurement))
  injection_xpoints <- injection_maxes$max +0.5
  injection_xpoints <- injection_xpoints[-length(injection_xpoints)]
  injection_names <- unique(df$injection)

  pH_0 <- 7.4
  O2_0_mmHg <- 151.6900241
  if (var == "O2_mmHg"){target<-  O2_0_mmHg} else {target <- pH_0}

  gg_plot <- ggplot(data = mean_range)+
    geom_crossbar(aes(ymin = m_min, ymax = m_max, x = measurement, y = m_min),
                  fill = "#293352", color = NA, alpha = 0.5, fatten = 0)+
    geom_hline(yintercept = target, linetype = "dashed", color = "#D16103")+
    geom_vline(xintercept = injection_xpoints, linetype = "dotted", color = "#D16103", size = 0.2)+
    theme_maxTick()+
    scale_x_continuous(limits = c(0,max(mean_range$measurement)+1),
                       breaks = seq(1, max(mean_range$measurement), by = 1),
                       expand = c(0, -0.3))+
    labs(title = var,
         subtitle = flnme,
         x = "measurement",
         y = "O2 (mmHg) or pH")+
    theme(plot.title = element_text(hjust = 0.5, size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 10))
  # facet_wrap(~df$group)

  return(gg_plot)
}

# all wells first tick raw (with highlight on tooltip)
#' Plot wells of first tick.
#'
#' @param var The type of Raw Emission correction, "O2_em_corr" or "pH_em_corr"
#' @param xf_raw_pr The preprocessed Raw data sheet of the Seahorse Excel file (converted from .asyr)
#' @param O2_targetEmission Target emission of the O2 (Usually 12500 Fluorescence Emission (Au))
#' @param pH_targetEmission Target emission of the pH (Usually 30000 Fluorescence Emission (Au))
#' @param flnme Name of the imported Seahorse file.
#' @param well_letter letter of the well to be plot (exp. A, for well A01)
#'
#' @return a variable gg_plot which contains information about how to plot/visualise the the fluorescence emission (Au) for all wells first tick raw.
#'
# @examples plot_allWells_firstTicks("O2_em_corr", xf_plate_pr$raw_data[[1]], 12500, 30000, "20191219_SciRep_PBMCs_donor_A", "A")
plot_allWells_firstTicks <- function(var, xf_raw_pr, O2_targetEmission, pH_targetEmission, flnme, well_letter){
  logger::log_info("Plot all wells of first tick.")

  if(grepl('[A-Z]', well_letter) == TRUE){

    theme_maxTick <- function(){
      theme_classic(base_size = 10) %+replace%
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.border = element_blank(),
              axis.ticks.x = element_line(),
              axis.ticks.y = element_line(),
              axis.line.y = element_line(),
              legend.text = element_text(size = rel(0.7)),
              legend.title = element_text(size = rel(0.7)),
              axis.title.x = element_text(size = rel(0.8)),
              axis.title.y = element_text(size = rel(0.8), angle = 90)
        )
    }

    df<- xf_raw_pr %>% select(measurement, well, group, timescale, minutes, tick, emission = .data[[var]] )

    df <- df[!is.na(df$emission), ]

    df <- df %>% group_by(well,measurement) %>%
      slice(which.min(tick))

    df <- df %>% group_by(well,measurement)

    df <- df %>%
      dplyr::filter(str_detect(well, glue::glue("^{well_letter}")))

    if (var == "O2_em_corr"){targetEMS<-  O2_targetEmission} else {targetEMS <- pH_targetEmission}

    gg_plot <- ggplot(data = df,
                      mapping = aes(x = measurement, y = emission, group = measurement, tooltip = well, data_id = well))+
      ggiraph::geom_line_interactive(aes(x = measurement, y = emission, group = well), alpha = 0.5, color = "#4E84C4")+
      ggiraph::geom_point_interactive(aes(x = measurement, y = emission, group = well), alpha = 0.8, color = "#4E84C4")+
      ggiraph::geom_hline_interactive(yintercept = targetEMS, linetype = "dashed", color = "#D16103")+
      ggplot2::scale_x_continuous(limits = c(0,max(df$measurement)+1),
                                  breaks = seq(1, max(df$measurement), by = 1),
                                  expand = c(0, -0.3))+
      theme_maxTick()+
      ggplot2::labs(title = var,
                    subtitle = flnme,
                    x = "measurement",
                    y = "emission (AU)")+
      ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 15),
                     plot.subtitle = element_text(hjust = 0.5, size = 10))

    return(girafe(ggobj = gg_plot))
  }

  else{
    logger::ERROR("provide right well information to plot.")
    break()
  }
}
