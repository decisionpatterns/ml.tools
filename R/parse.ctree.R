print.SplittingNode <- function(x, n = 1, ...) {
    cat(paste(paste(rep(" ", n - 1), collapse = ""), x$nodeID, ") ", sep=""))
    print(x$psplit, left = TRUE)
    cat(paste("; criterion = ", round(x$criterion$maxcriterion, 3), 
              ", statistic = ", round(max(x$criterion$statistic), 3), "\n", 
              collapse = "", sep = ""))
    print(x$left, n + 2)
    cat(paste(paste(rep(" ", n - 1), collapse = ""), x$nodeID, ") ", sep=""))
    print(x$psplit, left = FALSE)
    cat("\n")
    print(x$right, n + 2)
}

print.orderedSplit <- function(x, left = TRUE, ...) {
    if (!is.null(attr(x$splitpoint, "levels"))) {
        sp <- attr(x$splitpoint, "levels")[x$splitpoint]
    } else {
        sp <- x$splitpoint
    }
    if (!is.null(x$toleft)) left <- as.logical(x$toleft) == left
    if (left) {
        exp <- paste(x$variableName, "<=", sp)
    } else {
        exp <- paste(x$variableName, ">", sp)
    }
  
    return(exp)
}

print.nominalSplit <- function(x, left = TRUE, ...) {

    levels <- attr(x$splitpoint, "levels")

    ### is > 0 for levels available in this node
    tab <- x$table

    if (left) {
        lev <- levels[as.logical(x$splitpoint) & (tab > 0)]
    } else {
        lev <- levels[!as.logical(x$splitpoint) & (tab > 0)]
    }

    var <- x$variable
    op  <- ifelse( length(lev) == 1, "==", "%in%" )
    vals <-  
  
    txt <- paste("{", paste(lev, collapse = ", "), "}", collapse = "", sep = "")
    
    exp <- paste(x$variableName, "==", txt)
    
  
  
    return(exp)
}
