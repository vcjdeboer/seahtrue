#' @title Function used to collect background parameters.
#'
#' @param total_df The preprocessed Raw data sheet of the Seahorse Excel file (converted from .asyr)
#' @param var The type of Raw Emission correction, "O2_em_corr" or "pH_em_corr"
#' @param targetEMS The target Emission (12500 Au environmental fluorescence emission)
#' @keywords internal
#' @return AUC_bkgd background parameters for area under the curve (in list)
get_BKGD_auc <- function(total_df, var, targetEMS) {
  background_drift <- function(x, y) {
    y <- y / min(y) - 1
    auc_func <- bayestestR::area_under_curve(x,
      y,
      method = "trapezoid"
    )
    auc_func <- auc_func / max(x)
    return(auc_func)
  }
  background_drift2 <- function(x,
                                y,
                                baseline) {
    y <- y - baseline
    auc_func <- bayestestR::area_under_curve(x,
      y,
      method = "trapezoid"
    )
    return(auc_func)
  }

  background_drift3 <- function(x,
                                y,
                                baseline) {
    y[1] <- y - baseline
    return(auc_func)
  }

  df <- total_df %>%
    dplyr::filter(group == "Background") %>%
    dplyr::select(measurement,
      well,
      group,
      timescale,
      tick,
      emission = tidyr::all_of(var)
    )

  df <- df[!is.na(df$emission), ]

  temp_df <- NULL
  temp_auc <- tibble::tibble(
    well = 0,
    measurement = 0,
    auc = 0
  )
  AUC_bkgd <- NULL
  wellList <- unique(as.vector((df$well)))
  measurementList <- unique(as.vector((df$measurement)))
  for (j in wellList) {
    for (k in measurementList) {
      temp_df <- df %>% dplyr::filter(well == j & measurement == k)
      temp_auc$auc <- background_drift(
        temp_df$timescale,
        temp_df$emission
      )
      temp_auc$auc2 <- background_drift2(
        temp_df$timescale,
        temp_df$emission,
        targetEMS
      )
      temp_auc$dev_fromTarget <- temp_df$emission[1] - targetEMS # takes only the first tick here!!
      temp_auc$well <- j
      temp_auc$measurement <- k

      AUC_bkgd <- rbind(AUC_bkgd, temp_auc)
    }
  }
  return(AUC_bkgd)
}
