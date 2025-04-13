#' Inline rptR
#' @export
inline_rpt <- function(x){
  paste0(rnd(x$R), " (95% CI: [", rnd(x$CI_emp[1]), ", ", rnd(x$CI_emp[2]), "])")
}

#' Inline emtrend
#'
#' Extract statistics from emmGrid returned by \code{emmeans::emtrend} for
#' inline reporting.
#'
#' @param tbl An \code{\link[emmeans:emtrends]{emmeans::emtrends}} table.
#' @export

inline_trend <- function(tbl, term, at = NULL, label = TRUE, digits = 2,
                         lab_lower = TRUE){
  if(inherits(tbl, "emmGrid")){
    tbl <- summary(tbl, infer = TRUE)
  }
  stat <- id_stat(tbl)

  r <- tbl[tbl[,1] == term,]
  if (!is.null(at)){
    r <- r[which(r[,2] == at),]
  }
  if (label){
    label <- paste0("emtrend (",term, "): ")
    if (lab_lower){
      label <- tolower(label)
    }
  }
  paste0(label, rnd(r[,grep("trend", colnames(tbl))], digits = digits),
         "; ", switch(stat, t.ratio = "t",
                      z.ratio = "z"), "~", rnd(r$df,1), "~ = ",
         rnd(r[[stat]], digits), ", ",
         gtsummary::style_pvalue(r$p.value, prepend_p = TRUE, digits = digits))
}

#' Inline joint test
#'
#' #' Extract statistics from \code{emmeans::joint_tests} table for
#' inline reporting.
#'
#' @param tbl An \code{\link[emmeans:joint_tests]{emmeans::joint_tests}} table.
#' @export

inline_jnt <- function(tbl, term, at = NULL, digits = 3){
  check_packages("gtsummary")
  if (!is.null(at)){
    tbl <- tbl[tbl[,2] == at,]
  }
  r <- tbl[tbl$`model term` == term,]
  paste0("F~", r$df1, ",", rnd(r$df2), "~ = ", r$F.ratio, "; ",
         gtsummary::style_pvalue(r$p.value, prepend_p = TRUE, digits = digits))
}

#' Inline ANOVA
#' @importFrom utils tail
#' @export

inline_anova <- function(tbl, term, at=NULL, digits=3){
  check_packages("gtsummary")
  r <- tbl[which(rownames(tbl) == term),]
  paste0("F~", r$Df, ",", rnd(tail(tbl$Df,1)), "~ = ", rnd(r$F), "; ",
         gtsummary::style_pvalue(r$`Pr(>F)`, prepend_p = TRUE, digits = digits))
}

#' Inline contrast
#'
#' #' Extract statistics from emmGrid returned by \code{emmeans::contrast} for
#' inline reporting.
#'
#' @param x An \code{\link[emmeans:contrast]{emmeans::contrast}} table.
#' @export

inline_contrast <- function(x, contrast=NULL, what=c("estimate", "ratio"),
                            by = NULL, subset = NULL, at = NULL, digits = 3,
                            label = TRUE, qval = FALSE){
  check_packages(c("emmeans","gtsummary"))
  what <- match.arg(what, c("estimate", "ratio"))
  if (inherits(x, "emmGrid")){
    sum <- summary(x, infer = TRUE)
  } else{ sum <- x}
  stat <- id_stat(sum)
  cls <- id_conf_limits(sum)
  if(qval){
    xx <- emmeans::test(x, by=NULL, adjust = "fdr")
    sum$q.value <- xx$p.value
  }
  if (!is.null(subset)){
    sum <- subset(sum, subset)
  }
  if (!is.null(at)){
    sum <- sum[sum[,2] == at,]
  }
  if (is.null(contrast)){
    contrast <- 1
  }
  if (!is.null(by)){
    sum <- split(sum, sum[,by])
    sum <- sum[[at]]
  }
  rownames(sum) <- sum$contrast
  xx <- sum[contrast,]
  out <- paste0(rnd(xx[,what], digits),
                " (95% CI: ", rnd(xx[[cls[1]]], digits), ", ",
                rnd(xx[[cls[2]]], digits), ")", "; ",
                gtsummary::style_pvalue(xx$p.value, digits = digits,
                                        prepend_p = TRUE))
  if (qval){
    out <- paste0(out, "; ", gsub("p", "q",
                                  gtsummary::style_pvalue(xx$q.value,
                                                          digits = digits,
                                                          prepend_p = TRUE)))
  }
  if (label){
    paste0(contrast, ": ", out)
  } else{out}
}

#' Inline R2
#' @export
inline_r2 <- function(m, digits = 2){
  check_packages("MuMIn")
  if (inherits(m,"lm")){
    paste0("multiple R^2^ = ", rnd(summary(m)$r.squared, digits = digits),
           "; adjusted R^2^ = ", rnd(summary(m)$adj.r.squared, digits = digits))
  } else{
    r2 <- rnd(MuMIn::r.squaredGLMM(m)[[1]],2)
    paste0("marginal R^2^ = ", r2)
  }
}

#' Convenience function to format confidence limits for inline reporting
#' functions.
#' @noRd
id_conf_limits <- function(tbl){
  if ("lower.CL" %in% colnames(tbl)){
    c(lcl = "lower.CL", ucl = "upper.CL")
  } else {
    c(lcl = "asymp.LCL", ucl = "asymp.UCL")
  }
}

#' Convenience function to return stat for inline reporting functions.
#' @noRd
id_stat <- function(tbl){
  if (any(grepl("t.ratio", colnames(tbl)))){
    "t.ratio"
  } else if (any(grepl("z.ratio", colnames(tbl)))){
    "z.ratio"
  }
}
