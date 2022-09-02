#-------------------------------------------------------------------------------
# Miscellaneous helper functions
#-------------------------------------------------------------------------------
modernacolors <- function() {
  cols <- list(
    modernalightred = c(.96, .82, .84),
    modernalightgray = c(.85, .85, .85),
    modernalightorange = c(.98, .92, .84),
    modernalightgreen = c(.91, .95, .86),
    modernalightblue = c(.82, .90, .98),
    modernalightpurple = c(.93, .8, .94),
    modernared = c(.82, .2, .24),
    modernaorange = c(.93, .6, .25),
    modernablue = c(.33, .60, .88),
    modernagreen = c(.54, .74, .34),
    modernapurple = c(.54, .14, .55),
    modernadarkred = c(.41, .09, .12),
    modernadarkgray = c(.25, .25, .25),
    modernadarkblue = c(.07, .15, .25)
  )
  
  x <- sapply(lapply(cols, function(X) round(X * 255)), function(Y) paste0(Y, collapse = " "))
  cols <- lapply(strsplit(x, " "), function(X) rgb(X[1], X[2], X[3], maxColorValue=255))
  list2env(cols, envir = .GlobalEnv)
}

# function to check residual 
assumption_check <- function(lm_fit){
  qq <- 
    ggplot(data = tibble("residuals" = residuals(lm_fit)), aes(sample = residuals))+
    stat_qq() +
    stat_qq_line() +
    ggtitle("QQ plot") +
    theme_bw()
  
  res_vs_fitted <-
    ggplot(data = data.frame(residuals = residuals(lm_fit), `fitted.values` = fitted(lm_fit)), 
           aes(x = `fitted.values`, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "darkcyan") +
    labs(x = "fitted values") +
    ggtitle("Residual vs fitted") +
    theme_minimal()
  
  cowplot::plot_grid(plotlist = list(qq, res_vs_fitted), nrow = 1, ncol = 2)
}

