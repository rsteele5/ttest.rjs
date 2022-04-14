Rttest = function(x, y, paired=F, alpha=0.05){
  #### Input Error Checking ####
  if(!is.vector(x)) {stop("`x` must be vector")}
  if(!is.vector(y)) {stop("`y` must be vector")}
  if(!is.logical(paired)) {stop("`paired` must be a logical")}
  if(!is.numeric(alpha)) {stop("`alpha` must be numeric")}
  else if (alpha>1 | alpha<0) {
    stop(paste("`alpha` = ", alpha,"\n`alpha` must be between 0 and 1"))
  }

  #### T-Test Analysis and Execution ####
  # Variance Test (Currently variance in populations are assumed to be the same)
  tt = t.test(x,y, paired=paired , conf.level=1-alpha)

  #### Output data ####
  pop=rep(c("x", "y"), c(length(x),length(y))) # population identifiers
  obj = list(data=data.frame(Population=pop,Value=c(x,y)),
             alpha=alpha,
             ci=as.vector(tt$conf.int), # stripped of "conf.int" attr
             pvalue=tt$p.value)
  class(obj) = "Rttest"
  obj
}


plot.Rttest = function(x, pch=21,bg="Blue", cex =3){
  plot(1,1)
}

print.Rttest = function(x){
  print(c(x,1,1))
}
