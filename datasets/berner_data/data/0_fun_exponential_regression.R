# An R fuction to fit and evaluate exponential regression models

# FIT EXPONENTIAL MODEL -----------------------------------------------------------------
fit.exp <- function(x,y,weight=NA){
  outlist <- list()
  df <- data.frame(x=round(x),y=y)
  #outlist$my.nlm <- nls(y~a*exp(x*b), start=list(a=50,b=0.002),df)
  outlist$my.nlm <- nls(y~a*exp(x*b), start=list(a=0.005,b=0.004),df)
  my.nlm.summary <- summary(outlist$my.nlm)
  outlist$my.nlm.summary <- my.nlm.summary
  # model predictions
  outlist$obs.pred <- data.frame(obs=df$y)
  outlist$obs.pred$pred <- predict(object=outlist$my.nlm, newdata=df$x)
  
  # model predictions used to depict line
  outlist$preds.line <- data.frame(x=seq(min(df$x),max(df$x),0.25))
  outlist$preds.line$modeled <- predict(object=outlist$my.nlm, newdata=outlist$preds)
  
  # calc R2
  resids <- outlist$my.nlm.summary$residuals
  resid.ss <- sum((df$y-predict(outlist$my.nlm))^2)
  total.ss <- (nrow(df)-1)*var(df$y)
  r2 <- round(1-resid.ss/total.ss,2)
  #adj.r2 <- r2-(1-r2)*(1/(nrow(df)-1-1))
  rmse  <- sqrt(mean(resids^2))
  
  # model coefs
  outlist$coefs <- data.frame(
  a = round(my.nlm.summary$coefficients[1,1],3), a.se = round(my.nlm.summary$coefficients[1,2],3),
  b = round(my.nlm.summary$coefficients[2,1],5), b.se = round(my.nlm.summary$coefficients[2,2],5),
  r2 = r2, p = round(my.nlm.summary$coefficients[2,4],3), 
  df = paste(my.nlm.summary$df[1], my.nlm.summary$df[2], sep=','),
  rmse = round(rmse,2))
outlist
}

# END SCRIPT -----------------------------------------------------------------