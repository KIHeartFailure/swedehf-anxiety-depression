# MICE for cmprsk::crr

crr_mids <- function(time, event, xvars, data, subset = NULL, ...) {
  call <- match.call()
  if (!is.mids(data)) {
    stop("The data must have class mids")
  }
  analyses <- as.list(1:data$m)
  for (i in 1:data$m) {
    data.i <- mice::complete(data, i)
    if (!is.null(subset)) {
      data.i <- data.i %>% filter(!!subset)
    }
    analyses[[i]] <- crr(
      ftime = data.i[[time]], fstatus = data.i[[event]],
      cov1 = data.i[, xvars],
      failcode = 1, cencode = 0, variance = TRUE
    )
  }
  object <- list(
    call = call, call1 = data$call, nmis = data$nmis,
    analyses = analyses
  )
  oldClass(object) <- c("mira", "matrix")
  return(object)
}

vcov.crr <- function(object, ...) object$var # or getS3method('vcov','coxph')
coef.crr <- function(object, ...) object$coef

tidy.crr <- function(x, ...) {
  co <- coef(x)
  data.frame(
    term = names(co),
    estimate = unname(co),
    std.error = sqrt(diag(x$var)),
    stringsAsFactors = FALSE
  )
}

glance.crr <- function(x, ...) { }
