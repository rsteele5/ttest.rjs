Rttest = function(x, y, paired=F, alpha=0.05){
  #### Input Error Checking ####
  stopifnot("`x` must be vector"= is.vector(x),
            "`y` must be vector"= is.vector(y),
            "`paired` must be a logical"= is.logical(paired),
            "`alpha` must be numeric"= is.numeric(alpha),
            "`alpha` must be between 0 and 1"= (alpha<=1|alpha>=0))

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
