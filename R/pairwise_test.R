#' Compute p-values for pairwise comparisons for continuous and categorical variables by group.
#'
#' @param data A data frame containing VMAC variables.
#' @param variable A vector of continuous or categorical variables.
#' @param group A categorical variable by which the pariwise comparisons will be made.
#' @param continuous An integer value indicating the minimum number of unique values for \code{variable} to be considered continuous.
#' @param p.adj Method for adjusting p values (see p.adjust).
#' @param datatable A logical value indicating whether to return a formatted datatable (TRUE) or an unformatted dataframe (FALSE).
#' @return A dataframe containing p-values of pairwise comparisons.
#' @export

pairwise_test <- function(data, variable, group = "diagnosis.factor.base", continuous = 5, p.adj = "none", datatable = FALSE, ...) {
  variable <- unique(setdiff(variable, group))
  grp <- data[, group]
  grp.levels <- levels(grp)
  n.grp.levels <- length(grp.levels)
  output.df <- NULL

  get_variable_type <- function(x) {
    l <- length(unique(x));
    return(
      ifelse(
        l < continuous,
        "categorical",
        "continuous"
      )
    )
  }

  type.var = apply(as.matrix(data[, variable]), MARGIN = 2, FUN = get_variable_type)

  for (i in 1:length(variable)) {
    var <- data[, variable[i]]
    if (type.var[i] == "continuous") {
      pairwise.test <- pairwise.wilcox.test(x = as.numeric(var), g = grp, paired = FALSE, p.adj = p.adj)$p.value
    } else if (type.var[i] == "categorical") {
      nlev.var <- nlevels(var)
      contingency.table <- as.matrix(table(var, grp))
      if (nlev.var == 2) {
        pairwise.test <- pairwise.prop.test(t(contingency.table), p.adj = p.adj)$p.value
      } else {
        pairwise.test <- matrix(data = NA, nrow = n.grp.levels - 1, ncol = n.grp.levels - 1)
        pairwise.test[!upper.tri(pairwise.test)] >= fifer::chisq.post.hoc(t(contingency.table), control = p.adj)$adj.p
      }
    }

    output.df <- rbind(output.df, pairwise.test)
  }

  p.cutoff <- 0.05 / (n.grp.levels * (n.grp.levels - 1) / 2)

  output.df <- as.data.frame(
    dplyr::as_tibble(output.df, rownames = "Level") %>%
      dplyr::mutate(
        Variable = rep(variable, each = n.grp.levels - 1)
      ) %>%
      dplyr::select(
        Variable, Level, everything()
      )
  )

  if (datatable == TRUE) {
    return(
      DT::datatable(output.df,
                    options = list(
                      pageLength = 5 * (n.grp.levels - 1)
                    )) %>%
        DT::formatRound(setdiff(names(output.df), c("Variable", "Level")), 6) %>%
        DT::formatStyle(setdiff(names(output.df), c("Variable", "Level")),
                        color = styleInterval(p.cutoff, c("red", "black")))
    )
  } else {
    return(output.df)
  }
}
