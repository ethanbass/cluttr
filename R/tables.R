#' Pretty ANOVA
#' @export
pretty_anova <- function(x, labels = NULL){
  check_packages(c("emmeans","flextable"))
  x <- emmeans::joint_tests(x)
  if (!is.null(labels)){
    x <- relabel_model_terms(x, labels=labels)
  }
  x |> flextable::flextable() |>
    flextable::set_formatter(values = list("p.value" = p_val_format)) |>
    flextable::colformat_double(j = c("F.ratio"), digits = 2) |>
    flextable::set_header_labels("model term" = "Term",
                      "F.ratio" = "F",
                      "df1" = "DF1",
                      "df2" = "DF2",
                      "p.value" = "p-value") |>
    flextable::bold(i = ~p.value < .05, j="p.value") |>
    flextable::set_table_properties(layout = "autofit")
}

#' @noRd
relabel_model_terms<-function(jnt, labels){
  for (i in seq_along(labels)){
    jnt$`model term` <- gsub(names(labels)[i], labels[i], jnt$`model term`)
  }
  jnt
}

# format_emm_table <- function(tab){
#   class(tab)
#   check_packages("flextable")
#   if (inherits(tab, "emmGrid")){
#     tab <- summary(tab)
#   }
#   flextable::flextable(tab) |>
#     flextable::colformat_double(j = c("emmean", "SE", "lower.CL","upper.CL"),
#                                 digits = 3) |>
#     flextable::set_table_properties(layout = "autofit")
# }


#' @noRd
format_emm_table <- function(tab){
  check_packages("flextable")
  if (inherits(tab, "emmGrid")){
    tab <- summary(tab)
  }
  flextable::flextable(tab) |>
    flextable::colformat_double(j = c("emmean", "SE", "lower.CL","upper.CL"),
                                digits = 3) |>
    flextable::set_table_properties(layout = "autofit")
}

#' @noRd
format_contrast_table <- function(tab){
  check_packages("flextable")
  if (inherits(tab, "emmGrid")){
    tab <- summary(tab)
  }
  flextable::flextable(tab) |>
    flextable::colformat_double(j = c("estimate", "SE", "t.ratio","p.value"),
                                digits = 3) |>
    flextable::set_table_properties(layout = "autofit")
}
