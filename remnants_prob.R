
`iidspace` <- function (x, ntrials, probs = NULL){
  temp = list()
  for (i in 1:ntrials) {
    temp[[i]] <- x
  }
  res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
  if (is.null(probs)) {
    res$probs <- rep(1/dim(res)[1], dim(res)[1])
  }
  else {
    if (!identical(length(x), length(probs))) {
      stop("'probs' is not the same length as 'outcomes'")
    }
    if (any(probs < 0)) {
      stop("'probs' contains negative values")
    }
    probs <- probs/sum(probs)
    ptemp = list()
    for (i in 1:ntrials) {
      ptemp[[i]] <- probs
    }
    pdf <- expand.grid(ptemp, KEEP.OUT.ATTRS = FALSE)
    pres <- apply(pdf, 1, prod)
    res$probs <- pres
  }
  names(res) <- c(paste(rep("X", ntrials), 1:ntrials, sep = ""), 
                  "probs")
  return(res)
}


`is.probspace` <- function (x){
  if (any(class(x) == "ps")) 
    return(TRUE)
  if (!is.data.frame(x) | is.null(x$probs)) 
    return(FALSE)
  if (any(x$probs < 0)) 
    return(FALSE)
  return(TRUE)
}



`probspace` <- function (x, ...)
  UseMethod("probspace")



`probspace.default` <- function (x, probs, ...){
  y <- data.frame(x)
  if (missing(probs)) {
    y$probs <- rep(1, dim(y)[1])/dim(y)[1]
  }
  else {
    if (any(probs < 0)) {
      stop("'probs' contains negative values")
    }
    y$probs <- probs/sum(probs)
  }
  return(y)
}


`probspace.list` <- function (x, probs, ...){
  y <- x
  if (missing(probs)) {
    probs <- rep(1, length(y))/length(y)
  }
  else {
    if (any(probs < 0)) {
      stop("'probs' contains negative values")
    }
    probs <- probs/sum(probs)
  }
  res <- list(outcomes = y, probs = probs)
  class(res) <- c("ps", "list")
  return(res)
}


`Prob` <- function (x, ...)
  UseMethod("Prob")

`prob` <- function (x, ...){
  message("'prob' is deprecated; use 'Prob' instead.")
  Prob(x, ...)
}

`Prob.default` <- function (x, event = NULL, given = NULL, ...){
  if (is.null(x$probs)) {
    message("'space' is missing a probs column")
    stop("see ?probspace")
  }
  if (missing(event)) {
    r <- TRUE
  }
  else {
    e <- substitute(event)
    r <- eval(e, x, parent.frame())
    if (!is.logical(r)) 
      stop("'event' must evaluate to logical")
    r <- r & !is.na(r)
    if (!isTRUE(all.equal(sum(x$probs), 1))) 
      warning("'space' does not have probability 1.")
  }
  A <- x[r, ]
  if (missing(given)) {
    p <- sum(A$probs)
  }
  else {
    f <- substitute(given)
    g <- eval(f, x, enclos = parent.frame())
    if (!is.logical(g)) {
      if (!is.data.frame(given)) 
        stop("'given' must be data.frame or evaluate to logical")
      B <- given
    }
    else {
      if (missing(event)) 
        stop("'event' must be specified when 'given' is an expression")
      g <- g & !is.na(g)
      B <- x[g, ]
    }
    if (sum(B$probs <= 0)) 
      stop("prob(given) must be positive")
    p <- sum(intersect(A, B)$probs)/sum(B$probs)
  }
  return(p)
}


`Prob.ps` <- function (x, event = NULL, given = NULL, ...){
  if (is.null(x$probs)) {
    message("'space' is missing a probs component")
    stop("see ?probspace")
  }
  if (missing(event)) {
    A <- x
  }
  else {
    e <- substitute(event)
    r <- sapply(x$outcomes, function(t) {
      eval(e, t, enclos=parent.frame())
    })
    if (!is.logical(r)) 
      stop("'event' must evaluate to logical")
    r <- r & !is.na(r)
    if (!isTRUE(all.equal(sum(x$probs), 1))) 
      warning("'space' does not have probability 1.")
    A <- list(outcomes = x$outcomes[r], probs = x$probs[r])
  }
  if (missing(given)) {
    p <- sum(A$probs)
  }
  else {
    f <- substitute(given)
    g <- sapply(x$outcomes, function(t) {
      eval(f, t, enclos=parent.frame())
    })
    if (!is.logical(g)) {
      if (!is.probspace(given)) 
        stop("'given' must be a probspace or evaluate to logical")
      B <- given
    }
    else {
      if (missing(event)) 
        stop("'event' must be specified when 'given' is an expression")
      g <- g & !is.na(g)
      B <- list(outcomes = x$outcomes[g], probs = x$probs[g])
    }
    if (sum(B$probs <= 0)) 
      stop("prob(given) must be positive")
    p <- sum(intersect(A, B)$probs)/sum(B$probs)
  }
  return(p)
}


`marginal` <- function (space, vars = NULL){
  if (!is.data.frame(space) | is.null(space$probs)) {
    message("'space' is not a proper probability space")
    stop("see ?probspace")
  }
  if (is.null(vars)) 
    vars <- names(space)[names(space) != "probs"]
  if (!is.character(vars)) {
    stop("'vars' must be a character vector")
  }
  if (length(vars) > 1) {
    res <- aggregate(space$probs, by = as.list(space[, vars]), 
                     FUN = sum)
  }
  else {
    res <- aggregate(space$probs, by = list(space[, vars]), 
                     FUN = sum)
  }
  names(res) <- c(vars, "probs")
  return(res)
}

