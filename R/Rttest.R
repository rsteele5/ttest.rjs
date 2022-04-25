#' Rttest
#'
#' This function preforms several types of t-tests tailored towards user input.
#'
#' @param x First sample vector
#' @param y Second sample vector
#' @param paired Whether the samples are paired
#' @param alpha determines range of the confidence interval's distance from 100\%
#'
#' @importFrom stats t.test
#'
#' @return An Rttest class obj containing a list comprised of a data.frame of
#' the given samples (x,y), alpha value, confidence interval of type "conf.level",
#' and p value.
#' @export
#'
#' @examples
#' x <-rnorm(30,5,2)
#' y<- rnorm(40, 3,2)
#' alpha <- 0.1
#' obj <- Rttest(x = x, y = y, alpha = alpha)
#'
Rttest = function(x, y, paired=F, alpha=0.05){
  #### Input Error Checking ####
  stopifnot("`x` must be vector"= is.vector(x),
            "`y` must be vector"= is.vector(y),
            "`x` must contain at least 1 element"= length(x)>0,
            "`y` must contain at least 1 element"= length(y)>0,
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

#' @method print Rttest
#' @importFrom kableExtra scroll_box kable_styling kbl
#'
print.Rttest = function(x){
  #convert the data into distinct X and Y vectors
  data = x[["data"]]
  xv = data[data[1] == 'x',2]
  yv = data[data[1] == 'y',2]
  # Append NAs to the shorter vector making them the same size because
  # data.frames normally require columns to be the same length
  max.len = max(length(xv), length(yv))
  xv = c(xv, rep(NA, max.len - length(xv)))
  yv = c(yv, rep(NA, max.len - length(yv)))
  options(knitr.kable.NA = '') # NAs will not be included in kable output
  # Print only the data using kable in a presentable way
  scroll_box(kable_styling(kbl(
    data.frame(X = xv, Y = yv), caption = "Population Samples", padding = 30)),
    width = "300px", height = "400px")
}
