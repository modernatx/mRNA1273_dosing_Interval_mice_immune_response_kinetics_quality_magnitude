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
    modernadarkblue = c(.07, .15, .25),
    modernadarkblue2 = c(.07, .15, .65)
  )
  
  x <- sapply(lapply(cols, function(X) round(X * 255)), function(Y) paste0(Y, collapse = " "))
  cols <- lapply(strsplit(x, " "), function(X) rgb(X[1], X[2], X[3], maxColorValue=255))
  list2env(cols, envir = .GlobalEnv)
}

# function to check residual 
assumption_check <- function(lm_fit, response){
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
  
  res_hist <-
    ggplot(data = data.frame(residuals = residuals(lm_fit)),aes( residuals)) +
    geom_histogram()+
    labs(x = "Residual") +
    ggtitle("Residual histogram") +
    theme_minimal()
  
  observed_fitted <-
    ggplot(data = data.frame(Fitted.values = predict(lm_fit, type = "response"), "Observed" = response )) +
    #gratia::observed_fitted_plot(lm_fit)+
    geom_point(aes( x = Fitted.values,
                    y = Observed))+
    theme_minimal()+
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkcyan")
  
  cowplot::plot_grid(plotlist = list(qq, res_vs_fitted,   res_hist,  observed_fitted  ), nrow = 2, ncol = 2)
}

