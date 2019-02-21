my.LatexTable <-function(my.descr.var, my.bygrp, my.bygrp.lab=NULL, my.dat, 
                         my.overall=TRUE, 
                         my.test=TRUE, 
                         short.label="name",
                         printWarning=TRUE, 
                         my.size="tiny", show=TRUE, my.caption=NULL, fun=latex, ...){
  my.dat<-my.dat[!is.na(my.dat[, my.bygrp]), ]  
  my.descr.var.1 = unique(setdiff(my.descr.var, my.bygrp))
  descrForm <- as.formula(paste0(paste(my.descr.var.1, collapse= " + "),  "~", my.bygrp))
  
  summary = my.summaryM(descrForm,
    data       = my.dat,
    test       = my.test,
    overall    = my.overall,
    continuous = 6
  )

  if(short.label=="name") {
    summary$results$.ALL.$labels=my.descr.var.1
  } else if(is.numeric(short.label)) {
    summary$results$.ALL.$labels=substr(summary$results$.ALL.$labels, 1, short.label)
  }
  
  my.bygrp.lab =ifelse(length(my.bygrp.lab)==0, my.bygrp, my.bygrp.lab)
  
  if(show)  {
    cat('\\LTcapwidth=\\textwidth \n')
    if (printWarning) {
      cat("\\textcolor{magenta}{Due to small cell counts, some of the chi-squared values and p-values may be incorrect.}\\\\ \n")
    }
    fun(summary,
      file        = "",
#      what        = '%',
      where       = "!htbp",
      caption     = ifelse(length(my.caption)==0, paste0("All variables: Descriptive statistics by ", my.bygrp.lab), my.caption),
      prn         = TRUE,
      npct        = "both",
      exclude1    = FALSE,
      prmsd       = TRUE,
      long        = TRUE,
      digits      = 3,
      pdig        = 4,
      round       = 2,
      size        = my.size,
      outer.size  = "tiny",
      #outer.size  = "footnotesize",
      lines.page   = 1000,
      middle.bold = TRUE,
      longtable   = TRUE,
      center      = "centering",
      label       = paste0('tbl:descr.', my.bygrp), ...
    )
  }
  #summary
}
my.summaryM<-function (formula, groups = NULL, data = NULL, subset, na.action = na.retain, 
          overall = FALSE, continuous = 10, na.include = FALSE, quant = c(0.025, 
                                                                          0.05, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.95, 
                                                                          0.975), nmin = 100, test = FALSE, conTest = conTestkw, 
          catTest = catTestchisq, ordTest = ordTestpo) 
{
  marg <- length(data) && ".marginal." %in% names(data)
  if (marg) 
    formula <- update(formula, . ~ . + .marginal.)
  formula <- Formula(formula)
  Y <- if (!missing(subset) && length(subset)) 
    model.frame(formula, data = data, subset = subset, na.action = na.action)
  else model.frame(formula, data = data, na.action = na.action)
  X <- model.part(formula, data = Y, rhs = 1)
  Y <- model.part(formula, data = Y, lhs = 1)
  getlab <- function(x, default) {
    lab <- attr(x, "label")
    if (!length(lab) || lab == "") 
      default
    else lab
  }
  if (marg) {
    xm <- X$.marginal.
    X$.marginal. <- NULL
  }
  else xm <- rep("", nrow(X))
  if (length(X)) {
    xname <- names(X)
    if (length(xname) == 1 && !length(groups)) 
      groups <- xname
    if (!length(groups) && length(xname) > 1) {
      warnings("Must specify groups when > 1 right hand side variable is present.\ngroups taken as first right hand variable.")
      groups <- xname[1]
    }
    svar <- if (length(xname) == 1) 
      factor(rep(".ALL.", nrow(X)))
    else do.call("interaction", list(X[setdiff(xname, groups)], 
                                     sep = " "))
    group <- X[[groups]]
    glabel <- getlab(group, groups)
  }
  else {
    svar <- factor(rep(".ALL.", nrow(Y)))
    group <- rep("", nrow(Y))
    groups <- group.freq <- NULL
    glabel <- ""
  }
  quants <- unique(c(quant, 0.025, 0.05, 0.125, 0.25, 0.375, 
                     0.5, 0.625, 0.75, 0.875, 0.95, 0.975))
  nv <- ncol(Y)
  nameY <- names(Y)
  R <- list()
  for (strat in levels(svar)) {
    instrat <- svar == strat
    n <- integer(nv)
    type <- n
    comp <- dat <- vector("list", nv)
    names(comp) <- names(dat) <- nameY
    labels <- Units <- vector("character", nv)
    if (test) {
      testresults <- vector("list", nv)
      names(testresults) <- names(comp)
    }
    gr <- group[instrat]
    xms <- xm[instrat]
    if (all(xms != "")) 
      xms <- rep("", length(xms))
    group.freq <- table(gr)
    group.freq <- group.freq[group.freq > 0]
    if (overall) 
      group.freq <- c(Combined = sum(group.freq), group.freq)
    for (i in 1:nv) {
      w <- Y[instrat, i]
      if (length(attr(w, "label"))) 
        labels[i] <- attr(w, "label")
      if (length(attr(w, "units"))) 
        Units[i] <- attr(w, "units")
      if (!inherits(w, "mChoice")) {
        if (!is.factor(w) && !is.logical(w) && length(unique(w[!is.na(w)])) < 
            continuous) 
          w <- as.factor(w)
        s <- !is.na(w)
        if (na.include && !all(s) && length(levels(w))) {
          w <- na.include(w)
          levels(w)[is.na(levels(w))] <- "NA"
          s <- rep(TRUE, length(s))
        }
        n[i] <- sum(s & xms == "")
        w <- w[s]
        g <- gr[s, drop = TRUE]
        if (is.factor(w) || is.logical(w)) {
          tab <- table(w, g)
          if (test) {
            if (is.ordered(w)) 
              testresults[[i]] <- ordTest(g, w)
            else testresults[[i]] <- catTest(tab)
          }
          if (nrow(tab) == 1) {
            b <- casefold(dimnames(tab)[[1]], upper = TRUE)
            pres <- c("1", "Y", "YES", "PRESENT")
            abse <- c("0", "N", "NO", "ABSENT")
            jj <- match(b, pres, nomatch = 0)
            if (jj > 0) 
              bc <- abse[jj]
            else {
              jj <- match(b, abse, nomatch = 0)
              if (jj > 0) 
                bc <- pres[jj]
            }
            if (jj) {
              tab <- rbind(tab, rep(0, ncol(tab)))
              dimnames(tab)[[1]][2] <- bc
            }
          }
          if (overall) 
            tab <- cbind(Combined = apply(tab, 1, 
                                               sum), tab)
          comp[[i]] <- tab
          type[i] <- 1
        }
        else {
          sfn <- function(x, quant) {
            y <- c(quantile(x, quant), Mean = mean(x), 
                   SD = sqrt(var(x)), N = sum(!is.na(x)))
            names(y) <- c(paste0(formatC(100 * quant, 
                                         format = "fg", width = 1, digits = 15), 
                                 "%"), "Mean", "SD", "N")
            y
          }
          qu <- tapply(w, g, sfn, simplify = TRUE, quants)
          if (test) 
            testresults[[i]] <- conTest(g, w)
          if (overall) 
            qu$Combined <- sfn(w, quants)
          comp[[i]] <- matrix(unlist(qu), ncol = length(quants) + 
                                3, byrow = TRUE, dimnames = list(names(qu), 
                                                                 c(format(quants), "Mean", "SD", "N")))
          if (any(group.freq <= nmin)) 
            dat[[i]] <- lapply(split(w, g), nmin = nmin, 
                               function(x, nmin) if (length(x) <= nmin) 
                                 x
                               else NULL)
          type[i] <- 2
        }
      }
      else {
        w <- as.numeric(w) == 1
        n[i] <- sum(!is.na(apply(w, 1, sum)) & xms == 
                      "")
        g <- as.factor(gr)
        ncat <- ncol(w)
        tab <- matrix(NA, nrow = ncat, ncol = length(levels(g)), 
                      dimnames = list(dimnames(w)[[2]], levels(g)))
        if (test) {
          pval <- numeric(ncat)
          names(pval) <- dimnames(w)[[2]]
          d.f. <- stat <- pval
        }
        for (j in 1:ncat) {
          tab[j, ] <- tapply(w[, j], g, sum, simplify = TRUE, 
                             na.rm = TRUE)
          if (test) {
            tabj <- rbind(table(g) - tab[j, ], tab[j, 
                                                   ])
            st <- catTest(tabj)
            pval[j] <- st$P
            stat[j] <- st$stat
            d.f.[j] <- st$df
          }
        }
        if (test) 
          testresults[[i]] <- list(P = pval, stat = stat, 
                                   df = d.f., testname = st$testname, namefun = st$namefun, 
                                   statname = st$statname, latexstat = st$latexstat, 
                                   plotmathstat = st$plotmathstat)
        if (overall) 
          tab <- cbind(Combined = apply(tab, 1, 
                                        sum), tab)
        comp[[i]] <- tab
        type[i] <- 3
      }
    }
    labels <- ifelse(nchar(labels), labels, names(comp))
    R[[strat]] <- list(stats = comp, type = type, group.freq = group.freq, 
                       labels = labels, units = Units, quant = quant, data = dat, 
                       N = sum(!is.na(gr) & xms == ""), n = n, testresults = if (test) testresults)
  }
  structure(list(results = R, group.name = groups, group.label = glabel, 
                 call = call, formula = formula), class = "summaryM")
}

my.latex<-function (object, title = first.word(deparse(substitute(object))), 
          file = paste(title, ".tex", sep = ""), append = FALSE, label = title, 
          rowlabel = title, rowlabel.just = "l", cgroup = NULL, n.cgroup = NULL, 
          rgroup = NULL, n.rgroup = NULL, cgroupTexCmd = "bfseries", 
          rgroupTexCmd = "bfseries", rownamesTexCmd = NULL, colnamesTexCmd = NULL, 
          cellTexCmds = NULL, rowname, cgroup.just = rep("c", length(n.cgroup)), 
          colheads = NULL, extracolheads = NULL, extracolsize = "scriptsize", 
          dcolumn = FALSE, numeric.dollar = !dcolumn, cdot = FALSE, 
          longtable = FALSE, draft.longtable = TRUE, ctable = FALSE, 
          booktabs = FALSE, table.env = TRUE, here = FALSE, lines.page = 40, 
          caption = NULL, caption.lot = NULL, caption.loc = c("top", 
                                                              "bottom"), star = FALSE, double.slash = FALSE, vbar = FALSE, 
          collabel.just = rep("c", nc), na.blank = TRUE, insert.bottom = NULL, 
          insert.bottom.width = NULL, insert.top = NULL, first.hline.double = !(booktabs | 
                                                                                  ctable), where = "!tbp", size = NULL, center = c("center", 
                                                                                                                                   "centering", "centerline", "none"), landscape = FALSE, 
          multicol = TRUE, math.row.names = FALSE, math.col.names = FALSE, 
          hyperref = NULL, ...) 
{
  if (length(hyperref)) 
    hyperref <- sprintf("\\hyperref[%s]{", hyperref)
  center <- match.arg(center)
  caption.loc <- match.arg(caption.loc)
  cx <- format.df(object, dcolumn = dcolumn, na.blank = na.blank, 
                  numeric.dollar = numeric.dollar, cdot = cdot, math.row.names = math.row.names, 
                  math.col.names = math.col.names, double.slash = double.slash, 
                  ...)
  if (missing(rowname)) 
    rowname <- dimnames(cx)[[1]]
  nocolheads <- length(colheads) == 1 && is.logical(colheads) && 
    !colheads
  if (!length(colheads)) 
    colheads <- dimnames(cx)[[2]]
  col.just <- attr(cx, "col.just")
  nc <- ncol(cx)
  nr <- nrow(cx)
  if (length(cgroup)) {
    k <- length(cgroup)
    if (!length(n.cgroup)) 
      n.cgroup <- rep(nc/k, k)
    if (sum(n.cgroup) != nc) 
      stop("sum of n.cgroup must equal number of columns")
    if (length(n.cgroup) != length(cgroup)) 
      stop("cgroup and n.cgroup must have same lengths")
  }
  if (!length(rowname)) 
    rgroup <- NULL
  if (!length(n.rgroup) && length(rgroup)) 
    n.rgroup <- rep(nr/length(rgroup), length(rgroup))
  if (length(n.rgroup) && sum(n.rgroup) != nr) 
    stop("sum of n.rgroup must equal number of rows in object")
  if (length(rgroup) && length(n.rgroup) && (length(rgroup) != 
                                               length(n.rgroup))) 
    stop("lengths of rgroup and n.rgroup must match")
  if (length(rgroup) && rowlabel.just == "l") 
    rowname <- paste("~~", rowname, sep = "")
  sl <- ifelse(double.slash, "\\\\", "\\")
  if (ctable) {
    eol <- paste(sl, "NN\n", sep = "")
    eog <- ""
  }
  else if (longtable && length(n.rgroup)) {
    eol <- paste(sl, "tabularnewline*\n", sep = "")
    eog <- paste(sl, "tabularnewline\n", sep = "")
  }
  else {
    eol <- paste(sl, "tabularnewline\n", sep = "")
    eog <- paste(sl, "tabularnewline\n", sep = "")
  }
  if (booktabs) {
    toprule <- paste(sl, "toprule\n", sep = "")
    midrule <- paste(sl, "midrule\n", sep = "")
    bottomrule <- paste(sl, "bottomrule\n", sep = "")
  }
  else if (ctable) {
    toprule <- paste(sl, "FL\n", sep = "")
    midrule <- paste(sl, "ML\n", sep = "")
    bottomrule <- paste(sl, "LL\n", sep = "")
  }
  else {
    toprule <- if (first.hline.double) 
      paste(sl, "hline", sl, "hline\n", sep = "")
    else paste(sl, "hline\n", sep = "")
    midrule <- bottomrule <- paste(sl, "hline\n", sep = "")
  }
  if (length(cellTexCmds) & !(all(dim(cx) == dim(cellTexCmds)) & 
                                length(dim(cx)) == length(dim(cellTexCmds)))) {
    msg <- "The dimensions of cellTexCmds must be:"
    msg1 <- paste(dim(cx), collapse = " x ")
    msg <- paste(msg, msg1)
    msg <- paste(msg, ", but you gave me: ")
    msg1 <- paste(dim(cellTexCmds), collapse = " x ")
    msg <- paste(msg, msg1, sep = "")
    stop(msg)
  }
  if (length(cgroup) & length(cellTexCmds)) {
    my.index <- split(1:NCOL(cellTexCmds), rep(cumsum(n.cgroup), 
                                               times = n.cgroup))
    new.index <- NULL
    new.col <- dim(cx)[2] + 1
    for (i in my.index) new.index <- c(new.index, i, new.col)
    new.index <- new.index[-length(new.index)]
    cellTexCmds <- cbind(cellTexCmds, "")[, new.index]
  }
  if (length(cellTexCmds) | length(rownamesTexCmd)) {
    if (!length(rownamesTexCmd) & length(rowname)) 
      rownamesTexCmd <- rep("", nr)
    if (!length(cellTexCmds)) {
      cellTexCmds <- rep("", dim(cx)[1] * dim(cx)[2])
      dim(cellTexCmds) <- dim(cx)
    }
    rcellTexCmds <- cbind(rownamesTexCmd, cellTexCmds)
    thisDim <- dim(rcellTexCmds)
    rcellTexCmds <- paste(sl, rcellTexCmds, sep = "")
    rcellTexCmds[rcellTexCmds == sl] <- ""
    dim(rcellTexCmds) <- thisDim
  }
  else {
    rcellTexCmds <- NULL
  }
  if (length(cgroup)) {
    last.col <- cumsum(n.cgroup)
    first.col <- c(1, 1 + last.col[-length(last.col)])
    cgroup.cols <- cbind(first.col, last.col)
    col.subs <- split(seq(length.out = nc), rep.int(seq_along(n.cgroup), 
                                                    times = n.cgroup))
    cxi <- list()
    for (i in seq(along = col.subs)) cxi[[i]] <- cx[, col.subs[[i]], 
                                                    drop = FALSE]
    cxx <- cxi[[1]]
    col.justxx <- col.just[col.subs[[1]]]
    collabel.justxx <- collabel.just[col.subs[[1]]]
    colheadsxx <- colheads[col.subs[[1]]]
    extracolheadsxx <- extracolheads[col.subs[[1]]]
    cgroupxx <- cgroup[1]
    n.cgroupxx <- n.cgroup[1]
    for (i in seq(along = col.subs)[-1]) {
      cxx <- cbind(cxx, "", cxi[[i]])
      col.justxx <- c(col.justxx, "c", col.just[col.subs[[i]]])
      collabel.justxx <- c(collabel.justxx, "c", collabel.just[col.subs[[i]]])
      cgroupxx <- c(cgroupxx, "", cgroup[i])
      n.cgroupxx <- c(n.cgroupxx, 1, n.cgroup[i])
      colheadsxx <- c(colheadsxx, "", colheads[col.subs[[i]]])
      if (length(extracolheads)) {
        extracolheadsxx <- c(extracolheadsxx, "", extracolheads[col.subs[[i]]])
      }
    }
    cgroup.colsxx <- cgroup.cols + 0:(nrow(cgroup.cols) - 
                                        1)
    cx <- cxx
    col.just <- col.justxx
    collabel.just <- collabel.justxx
    n.cgroup <- n.cgroupxx
    cgroup.cols <- cgroup.colsxx[cgroup != "", , drop = FALSE]
    cgroup <- cgroupxx
    colheads <- colheadsxx
    extracolheads <- extracolheadsxx
    nc <- ncol(cx)
  }
  cline <- NULL
  if (length(rowname)) {
    cx <- cbind(rowname, cx)
    col.just <- c(rowlabel.just, col.just)
    if (length(extracolheads)) 
      extracolheads <- c("", extracolheads)
    collabel.just <- c(rowlabel.just, collabel.just)
    if (length(cgroup) == 0L) 
      colheads <- c(rowlabel, colheads)
    else {
      colheads <- c("", colheads)
      cgroup <- c(rowlabel, cgroup)
      rlj <- ifelse(rowlabel.just == "l", "l", "c")
      cgroup.just <- c(rlj, cgroup.just)
      n.cgroup <- c(1, n.cgroup)
      cgroup.cols <- 1 + cgroup.cols
      cline <- paste(sl, "cline{", cgroup.cols[, 1], "-", 
                     cgroup.cols[, 2], "}", sep = "", collapse = " ")
    }
    nc <- 1 + nc
  }
  vbar <- ifelse(vbar, "|", "")
  if (!append) 
    cat("", file = file)
  cat("%", deparse(sys.call()), "%\n", file = file, append = file != 
        "", sep = "")
  if (dcolumn) {
    decimal.point <- ifelse(cdot, paste(sl, "cdot", sep = ""), 
                            ".")
    cat(sl, "newcolumntype{.}{D{.}{", decimal.point, "}{-1}}\n", 
        sep = "", file = file, append = file != "")
  }
{
  tabular.cols <- paste(vbar, col.just, sep = "")
  if (!length(n.cgroup)) 
    tabular.cols <- c(tabular.cols, vbar)
  else {
    vv2 <- cumsum(n.cgroup)
    tabular.cols[vv2] <- paste(tabular.cols[vv2], vbar, 
                               sep = "")
  }
  tabular.cols <- paste(tabular.cols, collapse = "")
}
intop <- function() {
  if (!length(insert.top)) 
    return(NULL)
  paste(if (center == "none") 
    "\n\\vspace{1ex}\n\n", paste("\\textbf{", insert.top, 
                                 "}", sep = ""), if (center %in% c("centerline", 
                                                                   "centering")) 
                                   "\\\\", if (center == "none") 
                                     "\n\\vspace{1ex}\n\n", sep = "")
}
if (length(caption) && !ctable) {
  caption <- paste(sl, "caption", if (length(caption.lot)) 
    paste("[", caption.lot, "]", sep = ""), "{", caption, 
    if (!longtable) 
      paste(sl, "label{", label, "}", sep = ""), "}", 
    sep = "")
  table.env <- TRUE
}
if (ctable) {
  latex.begin <- latexBuild(if (length(size)) 
    paste("{", sl, size, sep = ""), "{", intop(), "", 
    paste(sl, "ctable[", sep = ""), "", if (length(caption) && 
                                              caption.loc == "bottom") 
      "botcap,", "", if (length(caption)) 
        paste("caption={", caption, "},", sep = ""), 
    "", if (length(caption.lot)) 
      paste("cap={", caption.lot, "},", sep = ""), 
    "", if (length(caption)) 
      paste("label=", label, ",", sep = ""), "", if (star) 
        "star, ", "", if (!landscape) 
          paste("pos=", where, ",", sep = ""), "", if (landscape) 
            "sideways", "", paste("]{", tabular.cols, "}", 
                                  sep = ""), "", if (length(insert.bottom)) 
                                    paste("{", paste(sl, "tnote[]{", sedit(insert.bottom, 
                                                                           "\\\\", " "), "}", sep = "", collapse = ""), 
                                          "}", sep = ""), "", if (!length(insert.bottom)) 
                                            "{}", "", paste("{", toprule, sep = ""), "{")
  latex.end <- attr(latex.begin, "close")
}
else if (!longtable) {
  latex.begin <- latexBuild(if (landscape) 
    paste(sl, "begin{landscape}", sep = ""), "landscape", 
    if (table.env) 
      paste(sl, "begin{table}", if (here) 
        "[H]"
        else paste("[", where, "]", sep = ""), "\n", 
        sep = ""), "table", if (length(size)) 
          paste("{", sl, size, "\n", sep = ""), "{", if (caption.loc == 
                                                           "top" && length(caption)) 
            paste(caption, "\n"), "", intop(), "", if (center == 
                                                         "center") 
              paste(sl, "begin{center}\n", sep = ""), "center", 
    if (center == "centering") 
      paste("{", sl, "centering\n", sep = ""), "{", 
    if (center == "centerline") 
      paste(sl, "centerline{", sep = ""), "{", hyperref, 
    "{", paste(sl, "begin{tabular}{", tabular.cols, 
               "}\n", toprule, sep = ""), "tabular", insert = list(if (!table.env && 
                                                                         length(insert.bottom)) list("tabular", "after", 
                                                                                                     insert.bottom), if (table.env) list("table", 
                                                                                                                                         "before", insert.bottom), if (caption.loc == 
                                                                                                                                                                         "bottom" && length(caption)) list("tabular", 
                                                                                                                                                                                                           "after", caption)))
  latex.end <- attr(latex.begin, "close")
}
else {
  latex.begin <- latexBuild(if (!draft.longtable) 
    paste(sl, "let", sl, "LTmulticolumn=", sl, "multicolumn", 
          sep = ""), "", paste(sl, "setlongtables", sep = ""), 
    "", if (landscape) 
      paste(sl, "begin{landscape}", sep = ""), "landscape", 
    if (length(size)) 
      paste("{", sl, size, "\n", sep = ""), "{", intop(), 
    "", paste(sl, "begin{longtable}{", tabular.cols, 
              "}", sep = ""), "longtable", if (caption.loc == 
                                                 "top" && length(caption)) 
                paste(caption, eog), "", toprule, "", insert = list(if (caption.loc == 
                                                                          "bottom" && length(caption)) list("longtable", 
                                                                                                            "after", caption)))
  latex.end <- attr(latex.begin, "close")
  if (!length(caption)) 
    latex.end <- paste(latex.end, "\\addtocounter{table}{-1}", 
                       sep = "\n")
}
cat(latex.begin, file = file, append = file != "")
cgroupheader <- NULL
if (length(cgroup)) {
  cvbar <- paste(cgroup.just, vbar, sep = "")
  cvbar[1] <- paste(vbar, cvbar[1], sep = "")
  cvbar[-length(cvbar)] <- paste(cvbar[-length(cvbar)], 
                                 vbar, sep = "")
  slmc <- paste(sl, "multicolumn{", sep = "")
  if (length(cgroupTexCmd)) 
    labs <- paste(sl, cgroupTexCmd, " ", cgroup, sep = "")
  else labs <- cgroup
  if (multicol) 
    labs <- paste(slmc, n.cgroup, "}{", cvbar, "}{", 
                  labs, "}", sep = "")
  cgroupheader <- paste(labs, collapse = "&")
  if (!length(cline)) {
    inr <- as.numeric(length(rowname))
    cline <- paste(sl, "cline{", 1 + inr, "-", nc, "}", 
                   sep = "")
  }
  cgroupheader <- paste(cgroupheader, eol, cline, "\n", 
                        sep = "")
  cat(cgroupheader, file = file, append = file != "")
}
{
  cvbar <- paste(collabel.just, vbar, sep = "")
  cvbar[1] <- paste(vbar, cvbar[1], sep = "")
  if (length(n.cgroup)) {
    vv2 <- cumsum(n.cgroup[-length(n.cgroup)])
    cvbar[vv2] <- paste(cvbar[vv2], vbar, sep = "")
  }
  slmc1 <- paste(sl, "multicolumn{1}{", sep = "")
  labs <- colheads
  if (length(colnamesTexCmd)) 
    labs <- paste(sl, colnamesTexCmd, " ", labs, sep = "")
  if (nocolheads) 
    colheads <- labs <- NULL
  header <- NULL
  if (length(labs)) {
    if (!length(extracolheads)) {
      heads <- get2rowHeads(labs)
      colheads <- heads[[1]]
      if (any(heads[[2]] != "")) 
        extracolheads <- heads[[2]]
    }
    if (multicol) 
      colheads <- paste(slmc1, cvbar, "}{", colheads, 
                        "}", sep = "")
    header <- if (length(colheads)) 
      paste(colheads, collapse = "&")
    if (length(extracolheads)) {
      extracolheads <- ifelse(extracolheads == "" | 
                                extracolsize == "", extracolheads, paste("{", 
                                                                         sl, extracolsize, " ", extracolheads, "}", 
                                                                         sep = ""))
      if (multicol) 
        extracolheads <- ifelse(extracolheads == "", 
                                extracolheads, paste(slmc1, cvbar, "}{", 
                                                     extracolheads, "}", sep = ""))
      else extracolheads <- ifelse(extracolheads == 
                                     "", extracolheads, paste(extracolheads, sep = ""))
      header <- if (length(header)) 
        paste(header, eol, paste(extracolheads, collapse = "&"), 
              sep = "")
    }
    if (length(header)) 
      cat(header, eog, file = file, sep = "", append = file != 
            "")
    if (ctable) 
      cat(midrule, file = file, append = file != "")
    else cat(midrule, file = file, append = file != 
               "")
  }
}
if (longtable) {
  if (!length(caption)) 
    cat(sl, "endhead\n", midrule, sl, "endfoot\n", sep = "", 
        file = file, append = file != "")
  else {
    cat(sl, "endfirsthead", sep = "", file = file, append = file != 
          "")
    cat(sl, "caption[]{\\em (continued)} ", eol, sep = "", 
        file = file, append = file != "")
    cat(midrule, sep = "", file = file, append = file != 
          "")
    if (length(cgroupheader)) 
      cat(cgroupheader, file = file, append = file != 
            "")
    if (length(header)) 
      cat(header, file = file, sep = "&", append = file != 
            "")
    cat(eog, midrule, sl, "endhead", "\n", midrule, 
        sep = "", file = file, append = file != "")
    if (length(insert.bottom)) {
      if (length(insert.bottom.width) == 0) {
        insert.bottom.width = paste0(sl, "linewidth")
      }
      cat(paste(sl, "multicolumn{", nc, "}{", "p{", 
                insert.bottom.width, "}}{", insert.bottom, 
                "}", eol, sep = "", collapse = "\n"), sep = "", 
          file = file, append = file != "")
    }
    cat(sl, "endfoot\n", sep = "", file = file, append = file != 
          "")
    cat(sl, "label{", label, "}\n", sep = "", file = file, 
        append = file != "")
  }
}
{
  if (length(n.rgroup)) {
    rg.end <- cumsum(n.rgroup)
    rg.start <- rg.end - n.rgroup + 1
    if (!length(rgroup)) {
      rgroup <- rep("", length(n.rgroup))
    }
    else {
      if (length(rgroupTexCmd)) {
        rgroup <- paste("{", sl, rgroupTexCmd, " ", 
                        rgroup, "}", sep = "")
      }
      else rgroup <- paste("{", rgroup, "}", sep = "")
    }
    seq.rgroup <- seq(along = n.rgroup)
  }
  else {
    seq.rgroup <- 1
    rg.end <- nr
    rg.start <- 1
  }
  linecnt <- 0
  for (j in seq.rgroup) {
    if (length(n.rgroup)) {
      if (longtable && linecnt > 0 && (linecnt + n.rgroup[j] + 
                                         (n.rgroup[j] > 1)) > lines.page) {
        cat(sl, "newpage\n", sep = "", file = file, 
            append = file != "")
        linecnt <- 0
      }
      cat(rgroup[j], rep("", nc - 1), sep = "&", file = file, 
          append = file != "")
      cat(eol, sep = "", file = file, append = file != 
            "")
      linecnt <- linecnt + 1
    }
    for (i in rg.start[j]:rg.end[j]) {
      if (!length(n.rgroup)) {
        if (longtable && linecnt > 0 && (linecnt + 
                                           1 > lines.page)) {
          cat(sl, "newpage\n", sep = "", file = file, 
              append = file != "")
          linecnt <- 0
        }
      }
      if (length(rcellTexCmds)) {
        num.cols <- ncol(cx)
        for (colNum in 1:num.cols) {
          cat(rcellTexCmds[i, colNum], " ", cx[i, 
                                               colNum], file = file, append = file != 
                "")
          if (colNum < num.cols) 
            cat("&", file = file, append = file != 
                  "")
        }
      }
      else {
        cat(cx[i, ], file = file, sep = "&", append = file != 
              "")
      }
      cat(if (i == rg.end[j] || (!ctable && !length(n.rgroup))) 
        eog
        else if (i < rg.end[j]) 
          eol, sep = "", file = file, append = file != 
          "")
      linecnt <- linecnt + 1
    }
    if (length(n.rgroup) > j) 
      cat(midrule, sep = "", file = file, append = file != 
            "")
    else cat(bottomrule, sep = "", file = file, append = file != 
               "")
  }
}
cat(latex.end, file = file, sep = "\n", append = file != 
      "")
sty <- c("longtable"[longtable], "here"[here], "dcolumn"[dcolumn], 
         "ctable"[ctable], "booktabs"[booktabs], if (landscape && 
                                                       !ctable) "lscape")
structure(list(file = file, style = sty), class = "latex")
}

