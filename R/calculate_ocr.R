library(dplyr)

#functions
substract_O2_bkgd <- function(O2, O2_bkgd, O2_0_mmHg){

  O2_corrected <- O2 - O2_bkgd + O2_0_mmHg

  return(O2_corrected)
}

add_extraTicks_new_2<- function(well_df, O2_var){

  well_df <- well_df %>% arrange(timescale)

  well_df$O2 <- well_df %>% pull(.data[[O2_var]])

  ticksPerMeasure <- well_df %>% group_by(measurement) %>% summarize(n = n())
  tickStart <- cumsum(ticksPerMeasure$n)+1
  tickEnd <- tickStart -1
  tickStart <- c(1, tickStart)
  tickStart <- tickStart[-length(tickStart)]
  tickStart_withoutFirst <- tickStart[-1]
  tickEnd_withoutLast <- tickEnd[-length(tickEnd)]

  seconds_perMeasure <- well_df$timescale[tickEnd]-well_df$timescale[tickStart]
  seconds_perGap <- well_df$timescale[tickStart_withoutFirst]-well_df$timescale[tickEnd_withoutLast]
  seconds_perTick <- min(seconds_perMeasure/ticksPerMeasure$n)
  ticks_perGap <- min(floor(seconds_perGap/seconds_perTick))

  well_df$extraTick <- FALSE

  extra_timescaleCalc <- function(x){
    x+(1:ticks_perGap)*seconds_perTick
  }

  O2 <- well_df$O2
  for (i in 1:length(tickEnd_withoutLast)){
    well_df <- well_df %>% add_row(
      O2 =
        O2[tickStart_withoutFirst[i]] -
        (O2[tickStart_withoutFirst[i]]-O2[tickEnd_withoutLast[i]]) *
        exp(-(((1:ticks_perGap)*seconds_perTick)/30)),
      timescale =
        extra_timescaleCalc(well_df$timescale[tickEnd[i]]),
      extraTick = TRUE)

  }

  well_df<-well_df[order(well_df$timescale),] # important to put the ticks in the right order

  return(well_df)

}

call_OCRticks <- function(well_df){

  #well_df <- df

  #input tickstart tickend

  #add extra column that tells whether the OCR is valid or not
  kernelSize <- 7 #kernel size that is used for the polynomial calcs, this means that 3 at beginning and end are not calculated
  offsetOCR <- (kernelSize -1)/2 # minus one because that is the actual row that is calculated

  ticksPerMeasure <- well_df %>% group_by(measurement) %>% summarize(n = n())
  tickStart <- cumsum(ticksPerMeasure$n)+1
  tickEnd <- tickStart -1
  tickStart <- c(1, tickStart) #?
  tickStart <- tickStart[-length(tickStart)] #? why three different tickStarts?

  well_df <- well_df %>% mutate(OCRisvalid = FALSE)
  totalMeasurements <- length(unique(well_df$measurement))
  for (x in 1:(totalMeasurements-1)) {
    well_df$OCRisvalid[(tickStart[x]+offsetOCR):(tickEnd[x]-offsetOCR)] <- TRUE
  }
  # because the last integral gives NA it cannot be used for OCR median/mean calculation (therefore for last measurement one less OCRisvalid)
  well_df$OCRisvalid[(tickStart[totalMeasurements]+offsetOCR):(tickEnd[totalMeasurements]-(offsetOCR+1))] <- TRUE

  return(well_df)
} #called in calculate_OCR

calculate_OCR_new <- function(well_df, assay_info_df){

  Tau_W <- assay_info_df$Tau_W
  Tau_AC <- assay_info_df$Tau_AC
  Tau_P <- assay_info_df$Tau_P
  Tau_C <- assay_info_df$Tau_C
  V_C <- assay_info_df$V_C
  O2_0_mM <- assay_info_df$O2_0_mM
  O2_0_mmHg <- assay_info_df$O2_0_mmHg

  df <- well_df

  df$smoothedO2<-0
  df$firstOrderDiff<-0
  df$secondOrderDiff<-0
  df$exponential <-0
  df$integral <- 0
  df$solvedIntegral<-0
  df$totalIntegral <- 0
  df$O2_W_mmHg <- 0
  df$OCR_mmHg <- 0
  df$OCR <- 0

  for(t in 4:nrow(df)){
    df$smoothedO2[t] <- (-2/21)*(df$O2[t-3])+(3/21)*(df$O2[t-2])+(6/21)*(df$O2[t-1])+
      (7/21)*(df$O2[t])+(6/21)*(df$O2[t+1])+(3/21)*(df$O2[t+2])-(2/21)*(df$O2[t+3])

    df$firstOrderDiff[t] <- ((3/28)*(df$O2[t-3])+(2/28)*(df$O2[t-2])+(1/28)*(df$O2[t-1])+
                               (-1/28)*(df$O2[t+1])+(-2/28)*(df$O2[t+2])+(-3/28)*(df$O2[t+3]))/
      ((3/28)*(df$timescale[t-3])+(2/28)*(df$timescale[t-2])+(1/28)*(df$timescale[t-1])+
         (-1/28)*(df$timescale[t+1])+(-2/28)*(df$timescale[t+2])+(-3/28)*(df$timescale[t+3]))

    df$secondOrderDiff[t] <- ((5/84)*(df$O2[t-3])+(-3/84)*(df$O2[t-1])+(-4/84)*(df$O2[t])+
                                (-3/84)*(df$O2[t+1])+(5/84)*(df$O2[t+3]))/
      (((3/28)*(df$timescale[t-3])+(2/28)*(df$timescale[t-2])+(1/28)*(df$timescale[t-1])+
          (-1/28)*(df$timescale[t+1])+(-2/28)*(df$timescale[t+2])+(-3/28)*(df$timescale[t+3])))^2
  }

  df$smoothedO2[1] <- df$smoothedO2[4]
  df$smoothedO2[2] <- df$smoothedO2[4]
  df$smoothedO2[3] <- df$smoothedO2[4]

  for(t in 1:nrow(df)){
    tp <- df$timescale[t]
    df$exponential[t] <- exp(((1/Tau_W)+(0))*tp)
    df$integral[t] <- df$exponential[t]*((1/Tau_W)*(df$smoothedO2[t]+Tau_P*df$firstOrderDiff[t]))
  }

  for(t in 2:nrow(df)){
    tp_min3 <- df$timescale[t-3]
    tp_min2 <- df$timescale[t-2]
    tp_min1 <- df$timescale[t-1]
    tp <- df$timescale[t]
    tp_plus1 <- df$timescale[t+1]
    tp_plus2 <- df$timescale[t+2]
    tp_plus3 <- df$timescale[t+3]

    df$solvedIntegral[t] <- -((tp_min1-tp)*((2*tp_min1+tp-3*tp_plus1)*(tp-tp_plus1)*df$integral[t-1]+
                                              (tp_min1+2*tp-3*tp_plus1)*(tp_min1-tp_plus1)*df$integral[t]-
                                              ((tp_min1-tp)^2)*df$integral[t+1]))/(6*(tp_min1-tp_plus1)*(tp-tp_plus1))
    df$totalIntegral[t]<-df$totalIntegral[t-1]+df$solvedIntegral[t]

  }

  for(t in 1:nrow(df)){
    O2_M_zero <- df$O2[1]
    df$O2_W_mmHg[t] <- (1/df$exponential[t])*(O2_M_zero+df$totalIntegral[t])
  }

  for(t in 4:nrow(df)){

    df$OCR_mmHg[t] <- (1/Tau_AC)*O2_0_mmHg -
      ((1/Tau_AC)+(1/Tau_C))*df$smoothedO2[t] +
      (1/Tau_C)*df$O2_W_mmHg[t]-
      (((1/Tau_AC)+
          (1/Tau_P)+
          (1/Tau_C))*df$firstOrderDiff[t])*Tau_P -
      (df$secondOrderDiff[t]*Tau_P)
    df$OCR[t] <- (df$OCR_mmHg[t]*(O2_0_mM/O2_0_mmHg))*10^9*60*10^-6*V_C

  }

  df <- df[df$extraTick==FALSE,] %>% select(timescale, measurement,OCR)

  df <- call_OCRticks(df)

  df <- df[df$OCRisvalid==TRUE,] %>% select(measurement, OCR) %>%
    group_by(measurement) %>%
    summarize(ocr = mean(OCR))

  #df_vector <- as.vector(unlist(df$OCR_mean))

  return(df)

}

sternVolmer <- function(x, KSV, F0){

  sternVolmer =(1/KSV)*((F0/x)-1)
}

#data

data <<- load("/cloud/project/data/data.rda")
#script
processed <- data %>%
  mutate(pr_raw_data = purrr::map2(raw_data,
                            assay_info,
                            ~mutate(.x,
                                    O2 = sternVolmer(.x$O2_em_corr, .y$KSV, .y$F0),
                                    O2_bkgd = sternVolmer(.x$O2_em_corr_bkg, .y$KSV, .y$F0),
                                    O2_M_mmHg = substract_O2_bkgd(O2, O2_bkgd, .y$O2_0_mmHg)))) %>%
  mutate(ocr_without_meta = purrr::map2(pr_raw_data,
                    assay_info,
                    ~{.x %>%
                        split(f = as.factor(.$well)) %>%
                        purrr::map2(rep(list(.y), length(.)),
                             ~{add_extraTicks_new_2(.x, "O2_M_mmHg") %>% calculate_OCR_new(.y)}) %>%
                        bind_rows(.id = "well")})) %>%
  mutate(ocr = purrr::map2(raw_data,
                    ocr_without_meta,
                    ~{.x %>%
                      distinct(well, measurement, .keep_all = TRUE) %>%
                      select(well, measurement,
                              group, interval, bufferfactor,
                              cell_n, flagged_well) %>%
                      left_join(.y, by= c("well", "measurement"))})) %>%
  select(-ocr_without_meta)







