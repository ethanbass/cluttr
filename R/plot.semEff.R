#' Plot semEff
#'
#' A function for plotting path diagrams from \code{\link[semEff:semEff]{semEff}}
#' objects with  \code{\link[DiagrammeR:DiagrammeR]{DiagrammeR}}
#'
#' @param mod An \code{\link[semEff:semEff]{semEff}} model.
#' @param legend Logical. Whether to include the legend. Defaults to \code{FALSE}.
#' @param cols A vector of two colors to color the arrows. The first color is used
#' to denote a positive effect and the second color is used to denote a negative
#' effect.
#' @param hide_paths Hide non-significant paths in the specified category:
#' \code{none} (default), \code{indirect} or \code{all}.
#' @param hide_labels Hide labels for non-significant paths in the specified
#' category: \code{none} (default), \code{indirect} or \code{all}.
#' @examplesIf interactive()
#' shipley.sem.eff <- semEff(shipley.sem.boot)
#' plot(shipley.sem.eff)
#' @author Ethan Bass
#' @export

plot.semEff <- function (mod, legend = FALSE, cols = c("blue", "red"),
                         hide_paths = c("none","indirect","all"),
                         hide_labels = c("none","indirect","all")){
  check_packages(c("semEff", "DiagrammeR"))
  hide_paths <- match.arg(hide_paths, c("none","indirect","all"))
  hide_labels <- match.arg(hide_labels, c("none","indirect","all"))
  tab <- semEff::getEffTable(mod)
  tab$response_label <- firstup(gsub("\\.", " ", tab$response))
  tab$predictor_label <- firstup(gsub("\\.", " ", tab$predictor))
  path_edges <- subset(tab, effect_type %in% c("direct", "indirect"))

  # Add significance indicator to the data frame
  path_edges$significant <- sapply(1:nrow(path_edges), function(i) {
    lower_ci <- path_edges$lower_ci[i]
    upper_ci <- path_edges$upper_ci[i]
    return(lower_ci > 0 || upper_ci < 0)  # TRUE if significant
  })

  # Store all paths before filtering
  all_path_edges <- path_edges

  # Filter paths based on hide_paths option
  if (hide_paths == "all") {
    # Hide all non-significant paths
    path_edges <- subset(path_edges, significant == TRUE)
  } else if (hide_paths == "indirect") {
    # Hide only non-significant indirect paths
    path_edges <- subset(path_edges, significant == TRUE | effect_type == "direct")
  }
  # If hide_paths is "none" or anything else, keep all paths

  get_significance <- function(lower_ci, upper_ci) {
    if (lower_ci > 0 || upper_ci < 0) {
      ci_width <- abs(upper_ci - lower_ci)
      effect_size <- abs(lower_ci + upper_ci)/2
      if (ci_width < effect_size * 0.3) {
        return("***")
      }
      else if (ci_width < effect_size * 0.5) {
        return("**")
      }
      else {
        return("*")
      }
    }
    else {
      return("")
    }
  }

  dot_code <- "digraph path_model {\n  graph [rankdir = LR, overlap = false, fontsize = 10]\n  node [shape = box, fontname = Helvetica, fontsize = 12]\n"

  # Get all nodes that are in the filtered path_edges
  all_nodes <- unique(c(path_edges$predictor, path_edges$response))
  nodes <- data.frame(id = all_nodes, stringsAsFactors = FALSE)
  nodes$label <- firstup(gsub("\\.", " ", nodes$id))

  for (i in 1:nrow(nodes)) {
    node_id <- nodes$id[i]
    node_label <- nodes$label[i]
    dot_code <- paste0(dot_code, "  \"", node_id, "\" [label = \"",
                       node_label, "\"]\n")
  }

  # Draw paths based on the filtered path_edges
  for (i in 1:nrow(path_edges)) {
    from_id <- path_edges$predictor[i]
    to_id <- path_edges$response[i]
    effect <- path_edges$effect[i]
    effect_type <- path_edges$effect_type[i]
    is_significant <- path_edges$significant[i]

    # Determine if we should show the label
    show_label <- TRUE
    if (hide_labels == "all") {
      show_label <- is_significant
    } else if (hide_labels == "indirect") {
      show_label <- is_significant || effect_type == "direct"
    }

    sig_mark <- get_significance(path_edges$lower_ci[i],
                                 path_edges$upper_ci[i])

    # Create label based on show_label decision
    if (show_label) {
      effect_label <- sprintf("%.2f%s", effect, sig_mark)
      if (effect > 0)
        effect_label <- paste0("+", effect_label)
    } else {
      effect_label <- ""  # Empty label for paths we want to hide the label for
    }

    color <- ifelse(effect > 0, cols[1], cols[2])
    style <- ifelse(effect_type == "direct", "solid", "dashed")
    width <- 0.5 + min(2.5, abs(effect) * 5)

    dot_code <- paste0(dot_code, "  \"", from_id, "\" -> \"",
                       to_id, "\" [label = \"", effect_label, "\", color = \"",
                       color, "\", style = \"", style, "\", penwidth = ",
                       width, "]\n")
  }

  if (legend) {
    legend_text <- "*** p < 0.001\\n** p < 0.01\\n* p < 0.05\\nBlue = positive effect\\nRed = negative effect\\nSolid = direct effect\\nDashed = indirect effect\\nLine width = effect magnitude"

    # Add path filtering information to legend
    if (hide_paths == "all") {
      legend_text <- paste0(legend_text, "\\nAll non-significant paths hidden")
    } else if (hide_paths == "indirect") {
      legend_text <- paste0(legend_text, "\\nNon-significant indirect paths hidden")
    }

    # Add label filtering information to legend (if different from path filtering)
    if (hide_labels != hide_paths) {
      if (hide_labels == "all") {
        legend_text <- paste0(legend_text, "\\nLabels for non-significant paths hidden")
      } else if (hide_labels == "indirect") {
        legend_text <- paste0(legend_text, "\\nLabels for non-significant indirect paths hidden")
      }
    }

    dot_code <- paste0(dot_code, "  subgraph cluster_legend {\n",
                       "    label = \"Legend\"\n", "    node [shape = plaintext]\n",
                       "    legend [label = \"", legend_text, "\"]\n",
                       "  }\n")
  }

  dot_code <- paste0(dot_code, "}")
  diagram <- DiagrammeR::grViz(dot_code)
  diagram
}


#' Export semEff path diagram
#'
#' A convenience function for exporting path diagram produced by
#' \code{\link{plot.semEff}}.
#' @importFrom DiagrammeRsvg export_svg
#' @importFrom rsvg rsvg_png
#' @param diagram A diagram (output from \code{\link{plot.semEff}}).
#' @param filename Path to write the image.
export_diagram <- function(diagram, filename) {
  check_packages(c("rsvg","DiagrammeRsvg"))
  DiagrammeRsvg::export_svg(diagram) |>
    charToRaw() |>
    rsvg::rsvg_png(filename)
}
