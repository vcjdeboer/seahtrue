# Bioenergetic space / trajectory analysis

# new functions ---------------------------------------

#' Calculate Bioenergetic Indices from OCR and ECAR Data
#'
#' @description
#' Computes bioenergetic parameters (OCR/ECAR) including spare capacity, glycolytic index,
#' and supply flexibility index. Compatible with both oligomycin- and monensin-based protocols.
#'
#' @param rate A tibble from the `rate_data` list of `revive_xfplate()`.
#' @param param_set_ocr Named character vector defining OCR injections (e.g. `init_ocr = "m3"`).
#' @param param_set_ecar Named character vector defining ECAR injections (e.g. `om_ecar = "m6"`).
#' @param conversion_model Either `"mookerjee"` (default) or `"agilent"`.
#' @param ug_protein_scaling_factor Protein content per well (in ug, default = 20).
#' @param ocr_var Name of column containing OCR values (default = `"J_oxphos"`).
#' @param ecar_var Name of column containing ECAR values (default = `"J_glyco"`).
#'
#' @section Canonical injection names:
#' `param_set_ocr` and `param_set_ecar` map measurement columns (e.g. `"m3"`)
#' to *canonical* injection names. Downstream metrics and the plotting
#' functions rely on these names. Use one of `init`, `fccp`, `amrot`, `om`,
#' `mon` suffixed with `_ocr` or `_ecar` (e.g. `init_ocr`, `amrot_ecar`).
#' Derived columns (`basal_*`, `max_*`, `spare_*`, `supply_index`,
#' `glyco_index`) are computed from these. `plot_bioenergetic_space()` and
#' `plot_bioenergetic_trajectory()` require `basal_ocr`, `fccp_ocr`,
#' `amrot_ocr`, `basal_ecar`, `fccp_ecar`, `amrot_ecar`; a warning is issued
#' if `calculate_space()` cannot produce them.
#'
#' @return A tibble with one row per group and multiple bioenergetic indices.
#' @export
#'
#' @examples
#'  data <- revive_xfplate(
#'   system.file("extdata", 
#'   "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue")
#'  ) 
#' calculate_space(
#'   rate = purrr::pluck(data, "rate_data", 1),
#'   param_set_ocr = c(init_ocr = "m3", fccp_ocr = "m4", amrot_ocr = "m9", mon_ocr = "m12"),
#'   param_set_ecar = c(init_ecar = "m3", fccp_ecar = "m4", amrot_ecar = "m9", mon_ecar = "m12"),
#'   conversion_model = "mookerjee")
#' 
calculate_space <- function(rate,
                            param_set_ocr,
                            param_set_ecar,
                            conversion_model = "mookerjee",
                            ug_protein_scaling_factor = 20,
                            ocr_var = "J_oxphos",
                            ecar_var = "J_glyco") {
  # Validate model
  if (!conversion_model %in% c("mookerjee", "agilent")) {
    stop("conversion_model must be either 'mookerjee' or 'agilent'")
  }

  # --- input validation ---
  if (!is.data.frame(rate)) {
    cli::cli_abort("{.arg rate} must be a data frame/tibble, not {.cls {class(rate)}}.")
  }
  required_rate_cols <- c("group", "measurement", "OCR_wave_bc", "ECAR_wave_bc")
  missing_rate <- setdiff(required_rate_cols, names(rate))
  if (length(missing_rate) > 0) {
    cli::cli_abort(c(
      "{.arg rate} is missing required column{?s}: {.val {missing_rate}}.",
      "i" = "Pass the tibble from {.code purrr::pluck(revive_xfplate(file), \"rate_data\", 1)}."
    ))
  }
  for (arg_name in c("param_set_ocr", "param_set_ecar")) {
    val <- get(arg_name)
    if (is.null(val) || is.null(names(val)) || any(names(val) == "")) {
      cli::cli_abort(c(
        "{.arg {arg_name}} must be a *named* character vector.",
        "i" = "e.g. {.code c(init_ocr = \"m3\", fccp_ocr = \"m4\", amrot_ocr = \"m9\")}"
      ))
    }
  }

  # Built-in ATP conversion factors
  atp_factors <- if (conversion_model == "mookerjee") {
    list(
      Jglyco_ecar_factor = 7.23,
      Jglyco_ocr_factor = 0.469,
      Joxphos_ocr_factor = 4.6988
    )
  } else {
    list(
      Jglyco_ecar_factor = 8.7552,
      Jglyco_ocr_factor = 0.8,
      Joxphos_ocr_factor = 5.5
    )
  }
  
  # Supply Flexibility Index (SFI)
  SFI <- function(x, y, max_ecar, max_ocr) {
    x_0 <- max(x + y - max_ocr, 0)
    y_0 <- max(x + y - max_ecar, 0)
    angle_ocr <- atan(max_ocr / x_0) * (180 / pi)
    angle_ecar <- atan(y_0 / max_ecar) * (180 / pi)
    ((angle_ocr - angle_ecar) / 90) * 100
  }
  
  # ATP-normalized OCR + ECAR rates
  rate_J <- rate %>%
    dplyr::mutate(
      J_glyco = (ECAR_wave_bc * atp_factors$Jglyco_ecar_factor -
                   OCR_wave_bc * atp_factors$Jglyco_ocr_factor) / ug_protein_scaling_factor,
      J_oxphos = (OCR_wave_bc * atp_factors$Joxphos_ocr_factor) / ug_protein_scaling_factor
    )
  
  df_ocr <- rate_J %>%
    dplyr::select(group, measurement, my_OCR = all_of(ocr_var)) %>%
    dplyr::summarize(OCR = mean(my_OCR, na.rm = TRUE), .by = c(group, measurement)) %>%
    tidyr::pivot_wider(names_from = measurement, names_prefix = "m", values_from = OCR) %>%
    dplyr::rename(!!!param_set_ocr)
  
  # Assign non_mito_ocr if present
  if ("amrot_ocr" %in% names(df_ocr)) {
    df_ocr <- dplyr::mutate(df_ocr, non_mito_ocr = amrot_ocr)
  } else {
    df_ocr <- dplyr::mutate(df_ocr, non_mito_ocr = NA_real_)
  }
  
  # Continue with dependent metrics
  df_ocr <- df_ocr %>%
    dplyr::mutate(
      basal_ocr = if ("init_ocr" %in% names(.)) init_ocr - non_mito_ocr else NA_real_,
      max_ocr = if ("fccp_ocr" %in% names(.)) fccp_ocr - non_mito_ocr else NA_real_,
      spare_ocr = max_ocr - basal_ocr,
      spare_ocr_index = spare_ocr / max_ocr,
      max_ocr_index = max_ocr / basal_ocr,
      atp_linked = if (all(c("init_ocr", "om_ocr") %in% names(.))) init_ocr - om_ocr else NA_real_,
      proton_leak = if ("om_ocr" %in% names(.)) om_ocr - non_mito_ocr else NA_real_,
      leak_index = proton_leak / basal_ocr,
      coupling_index = atp_linked / basal_ocr
    ) %>%
    dplyr::select(-dplyr::matches("^m\\d+$"))
  
  #conditionals
  df_ecar <- rate_J %>%
    dplyr::select(group, measurement, my_ECAR = all_of(ecar_var)) %>%
    dplyr::summarize(ECAR = mean(my_ECAR, na.rm = TRUE), .by = c(group, measurement)) %>%
    tidyr::pivot_wider(names_from = measurement, names_prefix = "m", values_from = ECAR) %>%
    dplyr::rename(!!!param_set_ecar)
  
  # Dynamically assign basal_ecar and max_ecar
  if ("init_ecar" %in% names(df_ecar)) {
    df_ecar <- dplyr::mutate(df_ecar, basal_ecar = .data[["init_ecar"]])
  } else {
    df_ecar <- dplyr::mutate(df_ecar, basal_ecar = NA_real_)
  }
  
  if ("mon_ecar" %in% names(df_ecar)) {
    df_ecar <- dplyr::mutate(df_ecar, max_ecar = .data[["mon_ecar"]])
  } else if ("om_ecar" %in% names(df_ecar)) {
    df_ecar <- dplyr::mutate(df_ecar, max_ecar = .data[["om_ecar"]])
  } else {
    df_ecar <- dplyr::mutate(df_ecar, max_ecar = NA_real_)
  }
  
  # Now compute derived ECAR metrics
  df_ecar <- df_ecar %>%
    dplyr::mutate(
      spare_ecar = max_ecar - basal_ecar,
      spare_ecar_index = spare_ecar / max_ecar,
      max_ecar_index = max_ecar / basal_ecar
    ) %>%
    dplyr::select(-dplyr::matches("^m\\d+$"))
  
  # Merge and derive global indices
  df_space <- df_ocr %>%
    dplyr::left_join(df_ecar, by = "group") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      bioenergetic_scope = max_ecar + max_ocr,
      glyco_index = (basal_ecar / bioenergetic_scope) * 100,
      bio_index = ((basal_ecar + basal_ocr) / bioenergetic_scope) * 100,
      glyco_index_max = (max_ecar / bioenergetic_scope) * 100,
      supply_index = purrr::pmap_dbl(
        list(basal_ecar, basal_ocr, max_ecar, max_ocr),
        function(b_ecar, b_ocr, m_ecar, m_ocr) {
          if (any(is.na(c(b_ecar, b_ocr, m_ecar, m_ocr)))) NA_real_
          else SFI(b_ecar, b_ocr, m_ecar, m_ocr)
        }
      )
    )

  # Names required by the plotting functions downstream.
  plot_required <- c("basal_ocr", "fccp_ocr", "amrot_ocr",
                     "basal_ecar", "fccp_ecar", "amrot_ecar")
  missing_for_plots <- setdiff(plot_required, names(df_space))
  if (length(missing_for_plots) > 0) {
    cli::cli_warn(c(
      "Output lacks column{?s} {.val {missing_for_plots}} needed by {.fn plot_bioenergetic_space}/{.fn plot_bioenergetic_trajectory}.",
      "i" = "Use the canonical injection names in {.arg param_set_ocr}/{.arg param_set_ecar}: one of {.val {c('init','fccp','amrot','om','mon')}} suffixed with {.val _ocr} or {.val _ecar}."
    ))
  }

  return(df_space)
}


#' Plot Bioenergetic Space for Each Group
#'
#' @description
#' Visualizes the metabolic space for each group using rectangles, basal points,
#' and maximal capacity for both ECAR and OCR axes.
#' This function expects output from `calculate_space()`.
#'
#' @param df A tibble returned by `calculate_space()`. Must include columns:
#'   `group`, `basal_ocr`, `fccp_ocr`, `basal_ecar`, `amrot_ecar`.
#' @param ecar_title Label for the x-axis (ECAR). Default: `"ECAR (pmol ATP/min/ug)"`.
#' @param ocr_title Label for the y-axis (OCR). Default: `"OCR (pmol ATP/min/ug)"`.
#' @param palette Optional named vector of fill colors (e.g., group names as names).
#' @param title Optional plot title (default `NULL`).
#' @param legend_title Optional title for the legend. Default: "Group".
#'
#' @return A `ggplot` object
#' @export
#' @importFrom dplyr mutate select summarize rename if_any filter all_of
#' @importFrom dplyr left_join ungroup matches
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom purrr pmap_dbl
#' @importFrom ggplot2 ggplot aes geom_rect geom_point geom_path geom_segment
#' @importFrom ggplot2 scale_colour_manual scale_color_manual scale_fill_manual labs theme_classic
#' @importFrom ggplot2 geom_hline geom_vline coord_cartesian
#' @importFrom grid arrow unit
#' @importFrom ggrepel geom_label_repel
#' @importFrom stats setNames
#' @importFrom rlang .data
#' @importFrom scales alpha
#' @examples
#' # Simulate output from calculate_space()
#' df_space <- tibble::tibble(
#'   group = c("control", "treated"),
#'   basal_ocr = c(25, 30),
#'   fccp_ocr = c(50, 60),
#'   basal_ecar = c(20, 28),
#'   amrot_ecar = c(40, 42)
#' )
#'
#' # Define group colors
#' palette <- c("control" = "#1b9e77", "treated" = "#d95f02")
#'
#' # Plot bioenergetic space
#' plot_bioenergetic_space(
#'   df = df_space,
#'   ecar_title = "J ATP glycolysis (pmol/min/ug)",
#'   ocr_title = "J ATP oxphos (pmol/min/ug)",
#'   palette = palette,
#'   title = "Example Bioenergetic Profiles"
#')
plot_bioenergetic_space <- function(df,
                                    ecar_title = "J ATP glycolysis (pmol/min/ug)",
                                    ocr_title = "J ATP oxphos (pmol/min/ug)",
                                    legend_title = "Group",
                                    palette = NULL,
                                    title = NULL) {
  
  required_cols <- c("group", "basal_ocr", "fccp_ocr", "basal_ecar", "amrot_ecar")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in input data: ", paste(missing_cols, collapse = ", "))
  }
  
  df <- df %>%
    dplyr::filter(!dplyr::if_any(dplyr::all_of(required_cols), is.na))
  
  x_max <- max(df$amrot_ecar, df$basal_ecar, na.rm = TRUE) * 1.1
  y_max <- max(df$fccp_ocr, df$basal_ocr, na.rm = TRUE) * 1.1
  
  p <- ggplot2::ggplot(df, ggplot2::aes(
    xmin = 0, ymin = 0,
    xmax = .data$amrot_ecar, ymax = .data$fccp_ocr,
    fill = .data$group
  )) +
    ggplot2::geom_rect(alpha = 0.5, color = "black") +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$amrot_ecar, y = 0, yend = .data$fccp_ocr),
                          color = "grey10", linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(x = .data$basal_ecar, y = .data$basal_ocr, fill = .data$group),
                        shape = 21, color = "black", size = 4) +
    ggplot2::labs(
      x = ecar_title,
      y = ocr_title,
      fill = legend_title,
      title = title
    ) +
    ggplot2::coord_cartesian(xlim = c(0, x_max), ylim = c(0, y_max), expand = FALSE) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black"),
      plot.title = ggplot2::element_text(hjust = 0)
    ) +
    ggplot2::geom_hline(yintercept = seq(0, y_max, by = 10), color = "grey90", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = seq(0, x_max, by = 10), color = "grey90", linewidth = 0.3) +
    ggplot2::facet_wrap(ggplot2::vars(.data$group))
  
  if (!is.null(palette)) {
    p <- p +
      ggplot2::scale_fill_manual(values = colorspace::lighten(palette, 0.2)) +
      ggplot2::scale_colour_manual(values = palette)
  }
  
  return(p)
}

#' Plot Metabolic Trajectories in J-space
#'
#' @description
#' Converts the wide-format `df_space` tibble into a trajectory plot showing the progression
#' of metabolic states (e.g., Baseline -> FCCP -> AM/Rot) for each group in OCR/ECAR space.
#' Arrows indicate direction of metabolic shifts across conditions.
#'
#' @param df A tibble as returned by `calculate_space()`. Must contain columns:
#'   `group`, `basal_ocr`, `fccp_ocr`, `amrot_ocr`, `basal_ecar`, `fccp_ecar`, `amrot_ecar`.
#' @param palette Optional named vector of fill colors (names = groups).
#' @param title Optional title for the plot. Default: `"Metabolic J-space"`.
#' @param label_map Optional named vector to rename default state labels (e.g., `c("Baseline" = "Basal")`).
#'
#' @return A `ggplot` object showing OCR vs ECAR trajectories with labeled points and arrows.
#' @export
#' @importFrom dplyr mutate select summarize rename if_any filter all_of
#' @importFrom dplyr left_join ungroup matches
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom purrr pmap_dbl
#' @importFrom ggplot2 ggplot aes geom_rect geom_point geom_path geom_segment
#' @importFrom ggplot2 scale_colour_manual scale_color_manual scale_fill_manual labs theme_classic
#' @importFrom ggplot2 geom_hline geom_vline coord_cartesian
#' @importFrom grid arrow unit
#' @importFrom ggrepel geom_label_repel
#' @importFrom stats setNames
#' @importFrom rlang .data
#' @importFrom scales alpha
#' @examples
#' df_space <- tibble::tibble(
#'   group = c("control", "treated"),
#'   basal_ocr = c(25, 30), fccp_ocr = c(50, 60), amrot_ocr = c(15, 18),
#'   basal_ecar = c(20, 28), fccp_ecar = c(22, 32), amrot_ecar = c(18, 21)
#' )
#' palette <- c("control" = "#1f78b4", "treated" = "#33a02c")
#' plot_bioenergetic_trajectory(df_space, palette = palette)
plot_bioenergetic_trajectory <- function(df,
                                         palette = NULL,
                                         title = "Metabolic J-space",
                                         label_map = c("Baseline" = "Basal", "FCCP" = "FCCP", "AM/Rot" = "AM/Rot")) {
  required_cols <- c(
    "group",
    "basal_ocr", "fccp_ocr", "amrot_ocr",
    "basal_ecar", "fccp_ecar", "amrot_ecar"
  )
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in input data: ", paste(missing_cols, collapse = ", "))
  }
  
  df_long <- df %>%
    dplyr::select(.data$group, dplyr::all_of(required_cols[-1])) %>%
    tidyr::pivot_longer(
      cols = !dplyr::all_of("group"),
      names_to = c("phase", ".value"),
      names_pattern = "(.*)_(ocr|ecar)"
    ) %>%
    dplyr::mutate(
      phase = dplyr::recode(
        as.character(.data$phase),
        "basal" = "Baseline",
        "fccp" = "FCCP",
        "amrot" = "AM/Rot",
        "mon" = "Monensin"
      ),
      phase = factor(.data$phase, levels = c("Baseline", "FCCP", "AM/Rot", "Monensin")),
      phase_label = dplyr::recode(as.character(.data$phase), !!!label_map)
    )
  
  x_max <- max(df_long$ecar, na.rm = TRUE) * 1.1
  y_max <- max(df_long$ocr, na.rm = TRUE) * 1.1
  
  p <- ggplot(df_long, ggplot2::aes(x = .data$ecar, y = .data$ocr, group = .data$group, fill = .data$group)) +
    ggplot2::geom_path(ggplot2::aes(color = .data$group),
                       linewidth = 1.2,
                       arrow = grid::arrow(type = "closed", length = grid::unit(0.15, "inches"))) +
    ggplot2::geom_point(shape = 21, size = 4, color = "black") +
    ggrepel::geom_label_repel(ggplot2::aes(label = .data$phase_label),
                              size = 3,
                              max.overlaps = 10,
                              box.padding = 0.4,
                              point.padding = 0.3,
                              label.size = NA,
                              fill = scales::alpha("white", 0.8),
                              segment.color = "grey60") +
    ggplot2::labs(
      title = title,
      x = expression(J[glyco] ~ "(ECAR-derived)"),
      y = expression(J[oxphos] ~ "(OCR-derived)"),
      fill = "Group",
      color = "Group"
    ) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black"),
      plot.title = ggplot2::element_text(hjust = 0)
    ) +
    ggplot2::geom_hline(yintercept = seq(0, y_max, by = 10), color = "grey90", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = seq(0, x_max, by = 10), color = "grey90", linewidth = 0.3) +
    ggplot2::coord_cartesian(xlim = c(0, x_max), ylim = c(0, y_max), expand = FALSE)
  
  if (!is.null(palette)) {
    p <- p +
      ggplot2::scale_color_manual(values = palette) +
      ggplot2::scale_fill_manual(values = colorspace::lighten(palette, 0.2))
  }
  
  return(p)
}
