#' Compute p-values for pairwise comparisons for continuous and categorical variables by group.
#'
#' @param data A data frame containing VMAC variables.
#' @param variable A vector of continuous or categorical variables.
#' @param group A categorical variable by which the pariwise comparisons will be made.
#' @param continuous An integer value indicating the minimum number of unique values for \code{variable} to be considered continuous.
#' @param padj Method for adjusting p values (see p.adjust).
#' @return \code{data} with added AD signature variables.
#' @export

pairwise_test <- function(data, variable, group = "diagnosis.factor.base", continuous = 5, padj = "none", ...) {
  variable <- unique(setdiff(variable, group))
  grp <- data[, group]
  grp.levels <- levels(grp)
  n.grp.levels <- length(grp.levels)
  my.latex.matrix.char <- my.latex.matrix.num <- NULL

  check.n.unique <- function(x) {
    l <- length(unique(x));
    return(
      ifelse(
        l < continuous,
        "categorical",
        "continuous"
      )
    )
  }

  type.var = apply(as.matrix(data[, variable]), MARGIN = 2, FUN = check.n.unique)

  for (i in 1:length(variable)) {
    var <- data[, variable[i]]
    if (type.var[i] == "continuous") {
      pairwise.test <- pairwise.wilcox.test(x = as.numeric(var), g = grp, paired = FALSE, p.adj = padj)$p.value
    } else if (type.var[i] == "categorical") {
      nlev.var <- nlevels(var)
      my.table = as.matrix(table(var, grp))
      if (nlev.var==2) {
        pairwise.test=pairwise.prop.test(t(my.table), p.adj=padj)$p.value
      }else {
        pairwise.test=matrix(NA, n.grp.levels-1, n.grp.levels-1)
        pairwise.test[!upper.tri(pairwise.test)]=chisq.post.hoc(t(my.table))$raw.p
      }
      #    my.latex.matrix.char=rbind(my.latex.matrix, matrix(format.pval(my.test, digits=6, eps=0.000001), nrow=n.grp.levels-1) )
      #     my.latex.matrix.num=rbind(my.latex.matrix.num, my.test)
    }
    my.latex.matrix.char=rbind(my.latex.matrix.char, matrix(format.pval(pairwise.test, digits=6, eps=0.000001), nrow=n.grp.levels-1) )
    my.latex.matrix.num=rbind(my.latex.matrix.num, pairwise.test)

  }

  p.cutoff = 0.05/(n.grp.levels*( n.grp.levels-1)/2)

  if (wantcolor){
    cellTex <- matrix(rep("", nrow(my.latex.matrix.num)*ncol(my.latex.matrix.num)), nrow=nrow(my.latex.matrix.num))
    cellTex <- apply(my.latex.matrix.num, 2, FUN=function(x) ifelse(!is.na(x)&as.numeric(x)<p.cutoff, "color{red}", ""))
  } else cellTex <- NULL

  if (short.label == "name") {
    my.rgroup=variable
  } else {
    my.rgroup=ifelse(label(data[, variable])=="", variable, label(data[, variable]))
  }

  my.latex.matrix=apply(my.latex.matrix, 2, formatPvalVMAC)

  latex( my.latex.matrix.char, title="",  file="",where="htdp", landscape=FALSE, na.blank=TRUE,
         cellTexCmds=cellTex,
         colheads=grp.levels[-n.grp.levels],
         rowname=rep(grp.levels[-1],length(variable)),
         rgroup=my.rgroup,
         n.rgroup=rep(n.grp.levels-1, length(variable)),
         caption=paste("Pairwise Comparison by", group, "; After adjusting for multiple comparison, p-value less then", round(p.cutoff, 3), " was colored red."),
         longtable=TRUE, ...)

}
