withr_tempfile <- function(env = parent.frame()) {
  f <- tempfile()
  do.call(on.exit, list(substitute(unlink(f), list(f = f)), add = TRUE), envir = env)
  f
}
