#' Plot semEff
#' @param mod An \code{\link[semEff:semEff]{semEff}} model.
#' @param legend Logical. Whether to include the legend. Defaults to \code{FALSE}.
#' @param cols A vector of two colors to color the arrows. The first color is used
#' to denote a positive effect and the second color is used to denote a negative
#' effect.
#' @author Ethan Bass
plot.semEff <- function(mod, legend = FALSE, cols = c("blue", "red")){
  check_packages(c("semEff","DiagrammeR"))
  tab <- semEff::getEffTable(mod)
  tab$response_label <- firstup(gsub("\\.", " ", tab$response))
  tab$predictor_label <- firstup(gsub("\\.", " ", tab$predictor))

  # Filter for direct and indirect effects only (exclude total and mediators)
  path_edges <- subset(tab, effect_type %in% c("direct", "indirect"))

  # Function to determine significance markers
  get_significance <- function(lower_ci, upper_ci) {
    # If CI doesn't include zero, it's significant
    if (lower_ci > 0 || upper_ci < 0) {
      # Determine significance level based on effect size and CI width
      ci_width <- abs(upper_ci - lower_ci)
      effect_size <- abs(lower_ci + upper_ci) / 2

      if (ci_width < effect_size * 0.3) {
        return("***")  # Very significant
      } else if (ci_width < effect_size * 0.5) {
        return("**")   # Moderately significant
      } else {
        return("*")    # Barely significant
      }
    } else {
      return("")  # Not significant
    }
  }

  # Create DOT language specification
  dot_code <- "digraph path_model {\n  graph [rankdir = LR, overlap = false, fontsize = 10]\n  node [shape = box, fontname = Helvetica, fontsize = 12]\n"

  # Add nodes
  nodes <- data.frame(
    id = unique(c(path_edges$predictor, path_edges$response)),
    stringsAsFactors = FALSE
  )
  nodes$label <- firstup(gsub("\\.", " ", nodes$id))

  for (i in 1:nrow(nodes)) {
    node_id <- nodes$id[i]
    node_label <- nodes$label[i]
    dot_code <- paste0(dot_code, "  \"", node_id, "\" [label = \"", node_label, "\"]\n")
  }


  # Add edges
  for (i in 1:nrow(path_edges)) {
    from_id <- path_edges$predictor[i]
    to_id <- path_edges$response[i]
    effect <- path_edges$effect[i]
    effect_type <- path_edges$effect_type[i]
    sig_mark <- get_significance(path_edges$lower_ci[i], path_edges$upper_ci[i])

    # Format effect label
    effect_label <- sprintf("%.2f%s", effect, sig_mark)
    if (effect > 0) effect_label <- paste0("+", effect_label)

    # Set color based on sign of effect
    color <- ifelse(effect > 0, cols[1],cols[2])

    # Set line style based on effect type
    style <- ifelse(effect_type == "direct", "solid", "dashed")

    # Set line width based on absolute effect size
    width <- 0.5 + min(2.5, abs(effect) * 5)

    # Add edge to DOT code - use quotes around node IDs for safety
    dot_code <- paste0(dot_code, "  \"", from_id, "\" -> \"", to_id,
                       "\" [label = \"", effect_label,
                       "\", color = \"", color,
                       "\", style = \"", style,
                       "\", penwidth = ", width, "]\n")
  }

  if (legend){
    dot_code <- paste0(dot_code, "  subgraph cluster_legend {\n",
                       "    label = \"Legend\"\n",
                       "    node [shape = plaintext]\n",
                       "    legend [label = \"*** p < 0.001\\n** p < 0.01\\n* p < 0.05\\nBlue = positive effect\\nRed = negative effect\\nSolid = direct effect\\nDashed = indirect effect\\nLine width = effect magnitude\"]\n",
                       "  }\n")
  }
  # Close the DOT code
  dot_code <- paste0(dot_code, "}")

  diagram <- DiagrammeR::grViz(dot_code)
  diagram
}

#' Export semEff diagram
#' @param diagram A diagram (output from \code{\link{plot.semEff}}).
#' @param filename Path to write the image.
export_diagram <- function(diagram, filename) {
  check_packages(c("rsvg","DiagrammeRsvg"))
  DiagrammeRsvg::export_svg(diagram) |>
    charToRaw() |>
    rsvg::rsvg_png(filename)
}
