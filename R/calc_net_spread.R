#' Calculate Net Spread for tows missing net width using a glm.
#'
#' The Marport Deep Sea Technologies Inc. net mensuration system was used during the deployment of each tow to record net spread and net height. Net width was measured as the horizontal distance between two sensors attached immediately forward of the junction of the upper breastline and the dandyline, and net height was measured from the headrope center to the seafloor. A custom-made AFSC bottom contact sensor (accelerometer) attached to the center of the footrope was used to determine tow duration based on footrope contact with the seafloor. Mean calc_net_spread values for estimating area swept for the tow duration were calculated according to the methods described by Lauth and Kotwicki (2014).
#'
#' In race_data, this will manifest as...
#' net_mensuration_code = Net Mensuration Method
#' 0* Unidentified method. Will make racebase.haul$net_mesured = "N".
#' 1 Scanmar net mensuration - don't use, historical
#' 2 NetMind net mensuration - don't use, historical
#' 3 Furuno net mensuration - don't use, historical
#' 4* Estimated from other hauls - when missing net spread, or spread and hieght. Will make racebase.haul$net_mesured = "N". Estimated using GLM
#' 5* Estimated from warp angle - hopefully, will not come up and shouldn't be used
#' 6 Marport net mensuration - shouldn't be there, indicates raw data
#' 7* Marport with sequential outlier rejection, smoothed mean, and adjusted for MKII offset (see AFSC Proc. Report Lauth & Kotwicki 2014). Will make racebase.haul$net_mesured = "Y".
#'
#' @param dat data.frame. This can be data from GIDES or race_data.hauls, including these columns: WIRE_OUT, NET_HEIGHT, NET_SPREAD
#'
#' @export
#' @return a list of equations
#'
#' @examples
#' # Here is an example using 202101 Alaska Night Data from race_data.hauls:
#' path <- system.file("exdata/calc_net_spread/VESTY202101_RACE_DATA.HAULS.csv", package = "GAPsurvey")
#' dat <- read.csv(file=path, header=TRUE, sep=",", stringsAsFactors = FALSE)
#' dat <- dat[dat$PERFORMANCE >= 0 & dat$HAUL_TYPE == 3,]
#' dat[dat$NET_SPREAD_METHOD != 7, "NET_SPREAD"] <- NA # a normal net width
#' dat[dat$NET_HEIGHT_METHOD != 6, "NET_HEIGHT"] <- NA # a normal net height
#' calc_net_spread(dat)
calc_net_spread <- function(dat) {

  dat0 <- dat[,names(dat) %in%
                c("HAUL", "NET_SPREAD",	"NET_HEIGHT",	"WIRE_OUT")]
  dat0$NET_SPREAD_METHODS <- 7 # No Issues
  dat0$NET_HEIGHT_METHOD <- 6 # No Issues
  dat0$WIRE_OUT_METHODS <- 5 # No Issues

  out <- data.frame("method_col" = c(),
                    "method_code" = c(),
                    "n" = c(),
                    "desc" = c())
  # 7: NET_SPREAD_METHOD Marport with sequential outlier rejection, smoothed mean, and adjusted for MKII offset (see AFSC Proc. Report Lauth & Kotwicki 2014). Will make racebase.haul$net_mesured = "Y".
  # if (nrow(dat0[complete.cases(dat0), ])>0) {
  #   n7 <- nrow(dat0[complete.cases(dat0), ])

  if (sum(!is.na(dat0$NET_SPREAD), na.rm = TRUE)>0) {
    n7 <- sum(!is.na(dat0$NET_SPREAD), na.rm = TRUE)
    out <- rbind.data.frame(out,
                            data.frame("method_col" = "NET_SPREAD_METHOD",
                                       "method_code" = 7,
                                       "n" = n7,
                                       "desc" = paste0(
                                         "There were ",
                                         n7,
                                         " hauls successfully completed where the Marport sensor properly caclulated net mensuration using sequential outlier rejection, smoothed mean, and MKII offset adjustments (see AFSC Proc. Report Lauth & Kotwicki 2014).")
                            ))
  }

  # When NET_HEIGHT_METHOD != 6 (aka NA)
  # TOLEDO: Will not work if there are no other tows with the same wire out!
  if (nrow(dat0[!is.na(dat0$NET_HEIGHT), ]) != nrow(dat0)) {
    scope_where_height_missing <- dat0$WIRE_OUT[is.na(dat0$NET_HEIGHT)]
    # out$tows_without_height <- length(scope_where_height_missing)
    dat0$NET_HEIGHT_METHOD[which(is.na(dat0$NET_HEIGHT))] <- 4
    for (i in 1:length(unique(scope_where_height_missing))) {
      dat0$NET_HEIGHT[which(is.na(dat0$NET_HEIGHT) &
                              dat0$WIRE_OUT == unique(scope_where_height_missing)[i])] <-
        mean(dat0$NET_HEIGHT[dat0$WIRE_OUT == unique(scope_where_height_missing)[i]], na.rm = TRUE)
    }

    out <- rbind.data.frame(out,
                            data.frame("method_col" = "NET_HEIGHT_METHOD",
                                       "method_code" = 4,
                                       "n" = length(scope_where_height_missing),
                                       "desc" = paste0(
                                         "There were ",
                                         length(scope_where_height_missing),
                                         " missing net height values estimated by averaging the net height of tows with the same wire out scope.")
                            ))

  }

  # 4* NET_SPREAD_METHOD Estimated from other hauls - when missing net spread, or spread and height Will make racebase.haul$net_mesured = "N". Estimated using GLM
  glm_param <- ""
  if (nrow(dat0[!is.na(dat0$NET_SPREAD), ]) != nrow(dat0)) {

    # Find the best model
    dat1 <- dat0[!is.na(dat0$NET_SPREAD),]
    dat1$INVSCOPE <- 1/dat1$WIRE_OUT
    glm1 = stats::glm(NET_SPREAD  ~ INVSCOPE + NET_HEIGHT + NET_HEIGHT*INVSCOPE,
                      data=c(dat1), family="gaussian")
    glm2 = stats::glm(NET_SPREAD  ~ INVSCOPE + NET_HEIGHT,
                      data=c(dat1), family="gaussian")
    glm3 = stats::glm(NET_SPREAD  ~ INVSCOPE + NET_HEIGHT*INVSCOPE,
                      data=c(dat1), family="gaussian")
    glm4 = stats::glm(NET_SPREAD  ~ NET_HEIGHT + NET_HEIGHT*INVSCOPE,
                      data=c(dat1), family="gaussian")
    glm5 = stats::glm(NET_SPREAD  ~ INVSCOPE,
                      data=c(dat1), family="gaussian")
    glm6 = stats::glm(NET_SPREAD  ~ NET_HEIGHT,
                      data=c(dat1), family="gaussian")
    glm7 = stats::glm(NET_SPREAD  ~ NET_HEIGHT*INVSCOPE,
                      data=c(dat1), family="gaussian")

    best_model <- data.frame(stats::AIC(glm1, glm2, glm3, glm4, glm5, glm6, glm7))
    best_model<-best_model[best_model$AIC <= min(best_model$AIC)+3,] # +3 because basically the same if within 3 of min
    best_model<-best_model[best_model$df <= min(best_model$df),]
    best_model<-best_model[best_model$AIC <= min(best_model$AIC),]

    glm0 <- get(x = rownames(best_model)[1])
    glm0_sum <- summary(glm0)

    # Predict data
    dat0$NET_SPREAD_METHODS[is.na(dat0$NET_SPREAD)] <- 4

    dat1 <- dat0[is.na(dat0$NET_SPREAD),]
    dat1$INVSCOPE <- 1/dat1$WIRE_OUT
    n4 <- nrow(dat1)

    dat0$NET_SPREAD[is.na(dat0$NET_SPREAD)] <-
      stats::predict.glm(object = glm0,
                         newdata = dat1,
                         type="response")

    # Collect coeficents
    glm0_param <- data.frame(glm0_sum$coefficients)
    names(glm0_param) <- c("est0","se","tvalue","prob")
    glm0_param$var <- rownames(glm0_param)
    glm0_param$est <- round(x = glm0_param$est0, digits = 3)
    glm0_param$case <- NA
    for (i in 1:nrow(glm0_param)) {
      if (glm0_param$prob[i] < 0.001) {
        glm0_param$case[i] <- "very signifcant (P < 0.001)"
        # } else if (glm0_param$prob[i] < 0.001) {
        #   glm0_param$case[i] <- "mostly significant (P < 0.001)"
      } else if (glm0_param$prob[i] < 0.01) {
        glm0_param$case[i] <- "significant (P < 0.001)"
      } else if (glm0_param$prob[i] < 0.05) {
        glm0_param$case[i] <- "somewhat significant (P < 0.001)"
      }
    }
    glm0_param$var0 <- tolower(row.names(glm0_param))
    glm0_param$var0 <- gsub(pattern = "(",
                            replacement = "",
                            x = glm0_param$var0,
                            fixed = TRUE)
    glm0_param$var0 <- gsub(pattern = ")",
                            replacement = "",
                            x = glm0_param$var0,
                            fixed = TRUE)
    glm0_param$var0 <- gsub(pattern = ":",
                            replacement = " x ",
                            x = glm0_param$var0,
                            fixed = TRUE)
    glm0_param$var0 <- gsub(pattern = "invscope",
                            replacement = "inversed scope",
                            x = glm0_param$var0,
                            fixed = TRUE)
    glm0_param$var0 <- gsub(pattern = "_",
                            replacement = " ",
                            x = glm0_param$var0,
                            fixed = TRUE)



    str0 <- c()
    if (length(unique(glm0_param$case)) == 1) {
      str0 <- paste0("both predictor variables and their interaction were significant (P < 0.001)")
    } else {
      for (i in 1:length(unique(glm0_param$case))) {
        str0 <- c(str0, paste0(
          text_list(glm0_param$var[glm0_param$case == unique(glm0_param$case)[i]]),
          " were ",
          unique(glm0_param$case)[i]
        ))
      }
      str0 <- text_list(str0)
    }


    fm <- as.character(glm0$formula)
    fm <- paste0(fm[2], " ",
                 fm[1], " ",
                 glm0_param$est[glm0_param$var == "(Intercept)"], " + ",
                 fm[3])

    fm <- gsub(pattern = "NET_SPREAD",
               replacement = "w",
               x = fm, fixed = TRUE)
    if (sum(glm0_param$var == "INVSCOPE:NET_HEIGHT")>0) {
      fm <- gsub(pattern = "NET_HEIGHT * INVSCOPE",
                 replacement = paste0(glm0_param$est[glm0_param$var == "INVSCOPE:NET_HEIGHT"],
                                      " * (h/s)"),
                 x = fm, fixed = TRUE)
    }
    if (sum(glm0_param$var == "NET_HEIGHT")>0) {
      fm <- gsub(pattern = "NET_HEIGHT",
                 replacement = paste0(glm0_param$est[glm0_param$var == "NET_HEIGHT"],
                                      " * h"),
                 x = fm, fixed = TRUE)
    }
    if (sum(glm0_param$var == "INVSCOPE")>0) {
      fm <- gsub(pattern = "INVSCOPE",
                 replacement = paste0(glm0_param$est[glm0_param$var == "INVSCOPE"],
                                      "/s"),
                 x = fm, fixed = TRUE)
    }
    fm <- gsub(pattern = " + -",
               replacement = " - ",
               x = fm, fixed = TRUE)

    out <- rbind.data.frame(out,
                            data.frame("method_col" = "NET_SPREAD_METHOD",
                                       "method_code" = 4,
                                       "n" = n4,
                                       "desc" = paste0(fm,
                                                       ": For ",
                                                       n4,
                                                       " hauls, the net width was estimated using a generalized linear model. The ",
                                                       str0)
                            ))


    # # if the interaction between INVSLOPE and HEIGHT are not significant
    # if ((glm1_sum$coefficients[4,4]>0.05)) {
    #   # Find the best model
    #   glm2 = glm(SPREAD ~ INVSCOPE + HEIGHT,
    #              data=c(dat),family="gaussian")
    #   glm2_sum <- summary(glm2)
    #   glm0<-glm2
    #   eq <- "w ~ 1/s + h: inverse scope and net height were significant (P < 0.001) but not their interaction term"
    # }
    # str <- paste0(str, eq, "
    #
    #               ")
  }

  # 0* Unidentified method. Will make racebase.haul$net_mesured = "N".

  # 5 Estimated from warp angle - hopefully, will not come up and shouldn't be used


  out <- list(#"plot" = plot(dat$NET_SPREAD,1/dat$WIRE_OUT),
    "actions" = out,
    "glm_summary" = glm0_param,
    "dat" = dat0)

  return(out)
}


# OLD Calculate during hauls -------------------------------------------------------
#
# #' Calculate Net Spread for tows missing net width using a glm.
# #'
# #' @details The Marport Deep Sea Technologies Inc. net mensuration system was used during the deployment of each tow to record net spread and net height. Net width was measured as the horizontal distance between two sensors attached immediately forward of the junction of the upper breastline and the dandyline, and net height was measured from the headrope center to the seafloor. A custom-made AFSC bottom contact sensor (accelerometer) attached to the center of the footrope was used to determine tow duration based on footrope contact with the seafloor. Mean calc_net_spread values for estimating area swept for the tow duration were calculated according to the methods described by Lauth and Kotwicki (2014).
# #' In race_data, this will manifest as...
# #' \describe{
# #'  \item{"net_mensuration_code"}{Net Mensuration Method}
# #'  \item{"0*"}{Unidentified method. Will make racebase.haul$net_measured = "N"}
# #'  \item{"1"}{Scanmar net mensuration - don't use, historical}
# #'  \item{"2"}{NetMind net mensuration - don't use, historical}
# #'  \item{"3"}{Furuno net mensuration - don't use, historical}
# #'  \item{"4*"}{Estimated from other hauls - when missing net spread, or spread and height. Will make racebase.haul$net_measured = "N". Estimated using GLM}
# #'  \item{"5*"}{Estimated from warp angle - hopefully, will not come up and shouldn't be used}
# #'  \item{"6"}{Marport net mensuration - shouldn't be there, indicates raw data}
# #'  \item{"7*"}{Marport with sequential outlier rejection, smoothed mean, and adjusted for MKII offset (see AFSC Proc. Report Lauth & Kotwicki 2014). Will make racebase.haul$net_measured = "Y".}
# #' }
# #'
# #' @param dat data.frame. This can be data from GIDES or race_data.hauls, including these columns: WIRE_OUT, NET_HEIGHT, NET_SPREAD
# #'
# #' @export
# #' @return a list of equations
# #'
# #' @examples
# #' # Here is an example using 202101 Alaska Night Data from race_data.hauls:
# #' path <- system.file("exdata/calc_net_spread/VESTY202101_RACE_DATA.HAULS.csv",
# #'                     package = "GAPsurvey")
# #' dat <- read.csv(file=path, header=TRUE, sep=",", stringsAsFactors = FALSE)
# #' dat <- dat[dat$PERFORMANCE >= 0 & dat$HAUL_TYPE == 3,]
# #' dat[dat$NET_SPREAD_METHOD != 7, "NET_SPREAD"] <- NA # a normal net width
# #' dat[dat$NET_HEIGHT_METHOD != 6, "NET_HEIGHT"] <- NA # a normal net height
# #' calc_net_spread(dat)
# calc_net_spread <- function(dat) {
#
#   dat0 <- dat[,names(dat) %in%
#                 c("HAUL", "NET_SPREAD",	"NET_HEIGHT",	"WIRE_OUT")]
#   dat0$NET_SPREAD_METHODS <- 7 # No Issues
#   dat0$NET_HEIGHT_METHOD <- 6 # No Issues
#   dat0$WIRE_OUT_METHODS <- 5 # No Issues
#
#   out <- data.frame("method_col" = c(),
#                     "method_code" = c(),
#                     "n" = c(),
#                     "desc" = c())
#   # 7: NET_SPREAD_METHOD Marport with sequential outlier rejection, smoothed mean, and adjusted for MKII offset (see AFSC Proc. Report Lauth & Kotwicki 2014). Will make racebase.haul$net_mesured = "Y".
#   # if (nrow(dat0[complete.cases(dat0), ])>0) {
#   #   n7 <- nrow(dat0[complete.cases(dat0), ])
#
#   if (sum(!is.na(dat0$NET_SPREAD), na.rm = TRUE)>0) {
#     n7 <- sum(!is.na(dat0$NET_SPREAD), na.rm = TRUE)
#     out <- rbind.data.frame(out,
#                             data.frame("method_col" = "NET_SPREAD_METHOD",
#                                        "method_code" = 7,
#                                        "n" = n7,
#                                        "desc" = paste0(
#                                          "There were ",
#                                          n7,
#                                          " hauls successfully completed where the Marport sensor properly caclulated net mensuration using sequential outlier rejection, smoothed mean, and MKII offset adjustments (see AFSC Proc. Report Lauth & Kotwicki 2014).")
#                             ))
#   }
#
#   # When NET_HEIGHT_METHOD != 6 (aka NA)
#   # TOLEDO: Will not work if there are no other tows with the same wire out!
#   if (nrow(dat0[!is.na(dat0$NET_HEIGHT), ]) != nrow(dat0)) {
#     scope_where_height_missing <- dat0$WIRE_OUT[is.na(dat0$NET_HEIGHT)]
#     # out$tows_without_height <- length(scope_where_height_missing)
#     dat0$NET_HEIGHT_METHOD[which(is.na(dat0$NET_HEIGHT))] <- 4
#     for (i in 1:length(unique(scope_where_height_missing))) {
#       dat0$NET_HEIGHT[which(is.na(dat0$NET_HEIGHT) &
#                               dat0$WIRE_OUT == unique(scope_where_height_missing)[i])] <-
#         mean(dat0$NET_HEIGHT[dat0$WIRE_OUT == unique(scope_where_height_missing)[i]], na.rm = TRUE)
#     }
#
#     out <- rbind.data.frame(out,
#                             data.frame("method_col" = "NET_HEIGHT_METHOD",
#                                        "method_code" = 4,
#                                        "n" = length(scope_where_height_missing),
#                                        "desc" = paste0(
#                                          "There were ",
#                                          length(scope_where_height_missing),
#                                          " missing net height values estimated by averaging the net height of tows with the same wire out scope.")
#                             ))
#
#   }
#
#   # 4* NET_SPREAD_METHOD Estimated from other hauls - when missing net spread, or spread and height Will make racebase.haul$net_mesured = "N". Estimated using GLM
#   glm_param <- ""
#   if (nrow(dat0[!is.na(dat0$NET_SPREAD), ]) != nrow(dat0)) {
#
#     # Find the best model
#     dat1 <- dat0[!is.na(dat0$NET_SPREAD),]
#     dat1$INVSCOPE <- 1/dat1$WIRE_OUT
#     glm1 = stats::glm(NET_SPREAD  ~ INVSCOPE + NET_HEIGHT + NET_HEIGHT*INVSCOPE,
#                       data=c(dat1), family="gaussian")
#     glm2 = stats::glm(NET_SPREAD  ~ INVSCOPE + NET_HEIGHT,
#                       data=c(dat1), family="gaussian")
#     glm3 = stats::glm(NET_SPREAD  ~ INVSCOPE + NET_HEIGHT*INVSCOPE,
#                       data=c(dat1), family="gaussian")
#     glm4 = stats::glm(NET_SPREAD  ~ NET_HEIGHT + NET_HEIGHT*INVSCOPE,
#                       data=c(dat1), family="gaussian")
#     glm5 = stats::glm(NET_SPREAD  ~ INVSCOPE,
#                       data=c(dat1), family="gaussian")
#     glm6 = stats::glm(NET_SPREAD  ~ NET_HEIGHT,
#                       data=c(dat1), family="gaussian")
#     glm7 = stats::glm(NET_SPREAD  ~ NET_HEIGHT*INVSCOPE,
#                       data=c(dat1), family="gaussian")
#
#     best_model <- data.frame(stats::AIC(glm1, glm2, glm3, glm4, glm5, glm6, glm7))
#     best_model<-best_model[best_model$AIC <= min(best_model$AIC)+3,] # +3 because basically the same if within 3 of min
#     best_model<-best_model[best_model$df <= min(best_model$df),]
#     best_model<-best_model[best_model$AIC <= min(best_model$AIC),]
#
#     glm0 <- get(x = rownames(best_model)[1])
#     glm0_sum <- summary(glm0)
#
#     # Predict data
#     dat0$NET_SPREAD_METHODS[is.na(dat0$NET_SPREAD)] <- 4
#
#     dat1 <- dat0[is.na(dat0$NET_SPREAD),]
#     dat1$INVSCOPE <- 1/dat1$WIRE_OUT
#     n4 <- nrow(dat1)
#
#     dat0$NET_SPREAD[is.na(dat0$NET_SPREAD)] <-
#       stats::predict.glm(object = glm0,
#                          newdata = dat1,
#                          type="response")
#
#     # Collect coeficents
#     glm0_param <- data.frame(glm0_sum$coefficients)
#     names(glm0_param) <- c("est0","se","tvalue","prob")
#     glm0_param$var <- rownames(glm0_param)
#     glm0_param$est <- round(x = glm0_param$est0, digits = 3)
#     glm0_param$case <- NA
#     for (i in 1:nrow(glm0_param)) {
#       if (glm0_param$prob[i] < 0.001) {
#         glm0_param$case[i] <- "very signifcant (P < 0.001)"
#         # } else if (glm0_param$prob[i] < 0.001) {
#         #   glm0_param$case[i] <- "mostly significant (P < 0.001)"
#       } else if (glm0_param$prob[i] < 0.01) {
#         glm0_param$case[i] <- "significant (P < 0.001)"
#       } else if (glm0_param$prob[i] < 0.05) {
#         glm0_param$case[i] <- "somewhat significant (P < 0.001)"
#       }
#     }
#     glm0_param$var0 <- tolower(row.names(glm0_param))
#     glm0_param$var0 <- gsub(pattern = "(",
#                             replacement = "",
#                             x = glm0_param$var0,
#                             fixed = TRUE)
#     glm0_param$var0 <- gsub(pattern = ")",
#                             replacement = "",
#                             x = glm0_param$var0,
#                             fixed = TRUE)
#     glm0_param$var0 <- gsub(pattern = ":",
#                             replacement = " x ",
#                             x = glm0_param$var0,
#                             fixed = TRUE)
#     glm0_param$var0 <- gsub(pattern = "invscope",
#                             replacement = "inversed scope",
#                             x = glm0_param$var0,
#                             fixed = TRUE)
#     glm0_param$var0 <- gsub(pattern = "_",
#                             replacement = " ",
#                             x = glm0_param$var0,
#                             fixed = TRUE)
#
#
#
#     str0 <- c()
#     if (length(unique(glm0_param$case)) == 1) {
#       str0 <- paste0("both predictor variables and their interaction were significant (P < 0.001)")
#     } else {
#       for (i in 1:length(unique(glm0_param$case))) {
#         str0 <- c(str0, paste0(
#           text_list(glm0_param$var[glm0_param$case == unique(glm0_param$case)[i]]),
#           " were ",
#           unique(glm0_param$case)[i]
#         ))
#       }
#       str0 <- text_list(str0)
#     }
#
#
#     fm <- as.character(glm0$formula)
#     fm <- paste0(fm[2], " ",
#                  fm[1], " ",
#                  glm0_param$est[glm0_param$var == "(Intercept)"], " + ",
#                  fm[3])
#
#     fm <- gsub(pattern = "NET_SPREAD",
#                replacement = "w",
#                x = fm, fixed = TRUE)
#     if (sum(glm0_param$var == "INVSCOPE:NET_HEIGHT")>0) {
#       fm <- gsub(pattern = "NET_HEIGHT * INVSCOPE",
#                  replacement = paste0(glm0_param$est[glm0_param$var == "INVSCOPE:NET_HEIGHT"],
#                                       " * (h/s)"),
#                  x = fm, fixed = TRUE)
#     }
#     if (sum(glm0_param$var == "NET_HEIGHT")>0) {
#       fm <- gsub(pattern = "NET_HEIGHT",
#                  replacement = paste0(glm0_param$est[glm0_param$var == "NET_HEIGHT"],
#                                       " * h"),
#                  x = fm, fixed = TRUE)
#     }
#     if (sum(glm0_param$var == "INVSCOPE")>0) {
#       fm <- gsub(pattern = "INVSCOPE",
#                  replacement = paste0(glm0_param$est[glm0_param$var == "INVSCOPE"],
#                                       "/s"),
#                  x = fm, fixed = TRUE)
#     }
#     fm <- gsub(pattern = " + -",
#                replacement = " - ",
#                x = fm, fixed = TRUE)
#
#     out <- rbind.data.frame(out,
#                             data.frame("method_col" = "NET_SPREAD_METHOD",
#                                        "method_code" = 4,
#                                        "n" = n4,
#                                        "desc" = paste0(fm,
#                                                        ": For ",
#                                                        n4,
#                                                        " hauls, the net width was estimated using a generalized linear model. The ",
#                                                        str0)
#                             ))
#
#
#     # # if the interaction between INVSLOPE and HEIGHT are not significant
#     # if ((glm1_sum$coefficients[4,4]>0.05)) {
#     #   # Find the best model
#     #   glm2 = glm(SPREAD ~ INVSCOPE + HEIGHT,
#     #              data=c(dat),family="gaussian")
#     #   glm2_sum <- summary(glm2)
#     #   glm0<-glm2
#     #   eq <- "w ~ 1/s + h: inverse scope and net height were significant (P < 0.001) but not their interaction term"
#     # }
#     # str <- paste0(str, eq, "
#     #
#     #               ")
#   }
#
#   # 0* Unidentified method. Will make racebase.haul$net_mesured = "N".
#
#   # 5 Estimated from warp angle - hopefully, will not come up and shouldn't be used
#
#
#   out <- list(#"plot" = plot(dat$NET_SPREAD,1/dat$WIRE_OUT),
#     "actions" = out,
#     "glm_summary" = glm0_param,
#     "dat" = dat0)
#
#   return(out)
# }
