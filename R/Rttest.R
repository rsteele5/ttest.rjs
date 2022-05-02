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
#' y <- rnorm(40,3,2)
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
            "`alpha` must be numeric"= is.number(alpha),
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



#' Print.Rttest
#'
#' Displays the 2 sample using kable.
#'
#' @param x Rttest object
#' @param width The width in pixels(px). (defualt: NULL)
#' @param height The height in pixels(px). This can be set to NULL for and
#' unbounded height. (defualt: 400)
#' @param ... unused
#'
#' @importFrom kableExtra scroll_box kable_styling kbl
#' @export
#'
print.Rttest = function(x, width=NULL, height=400,...){
  #### Input Error Checking ####
  stopifnot("`width` must be number"= is.number(width)|is.null(width),
            "`height` must be number"= is.number(height)|is.null(height))
  data = has.Rttest.data(x)
  #### Data Conversion ####
  xv = data[data[1] == 'x',2]
  yv = data[data[1] == 'y',2]
  # Append NAs to the shorter vector making them the same size because
  # data.frames normally require columns to be the same length
  max.len = max(length(xv), length(yv))
  xv = c(xv, rep(NA, max.len - length(xv)))
  yv = c(yv, rep(NA, max.len - length(yv)))
  # create pixel measurements if needed
  if(is.number(width)) { width = paste(width, "px", sep="") }
  if(is.number(height)) { height = paste(height, "px", sep="") }
  #### Kable Output ####
  options(knitr.kable.NA = '') # NAs will not be included in kable output
  # Print only the data using kable in a presentable way
  scroll_box(kable_styling(kbl(
    data.frame(X = xv, Y = yv), caption = "Population Samples", padding = 30)),
    width = width, height = height)
}

#' Plot Diagnostics for an Rttest Object
#'
#' A customizable box plot of the two samples contained within the Rttest object
#' using ggplot2
#'
#' @param x Rttest object.
#' @param main main title for the plot.
#' @param sub sub-title for the plot.
#' @param xlab a label for the first population sample (default: "x") *TODO*
#' @param ylab a label for the second population sample (default: "y") *TODO*
#' @param poplab a label to denote what population the samples were taken from
#' (default: "Population")
#' @param vlab a label to denote the values of the samples (default: "Value")
#' @param ... unused
#' @importFrom ggplot2 ggplot aes geom_boxplot labs
#' @export
#'
#' @examples
#' x <-rnorm(30,5,2)
#' y <- rnorm(40,3,2)
#' alpha <- 0.1
#' obj <- Rttest(x = x, y = y, alpha = alpha)
#'
#' plot(obj)
#' plot(obj, main="My Custom Title", sub="Which also has a sub-title",
#'      poplab="Birds", vlab="wingspan",
#'      xlab="Red", ylab="Black")
#'
plot.Rttest = function(x, main="Comparison of Value Distripution per Sample",
                       sub=NULL, poplab="Population", vlab="Value",
                       xlab=NULL, ylab=NULL, ...){
  #### Global Variable Instantiation ####
  Population = NULL
  Value = NULL
  #### Input Error Checking ####
  stopifnot("`main` must be string"= is.stringORnull(main),
            "`sub` must be string"= is.stringORnull(sub),
            "`xlab` must be string"= is.stringORnull(xlab),
            "`ylab` must be string"= is.stringORnull(ylab),
            "`poplab` must be string"= is.string(poplab),
            "`vlab` must be string"= is.string(vlab))
  data = has.Rttest.data(x)
  #### Generate and return plot ####
  rtp = ggplot(data, aes(x=Population, y=Value, fill=Population)) +
    geom_boxplot() + labs(title=main, subtitle=sub, x=poplab, y=vlab, fill=poplab)
  # TODO:implement the modification of x and y labels to plot
  rtp
}

#### Helper Functions ####
is.number = function(x){
  is.numeric(x) & length(x) == 1
}
is.string = function(x){
  is.character(x) & length(x) == 1
}
is.stringORnull = function(x){
  is.string(x)|is.null(x)
}
has.Rttest.data = function(x){
  stopifnot("Rttest no longer contains the \'data\' data.frame"= "data" %in% names(x))
  data = x[["data"]]
  stopifnot("Rttest data variable is no longer a data.frame"= is.data.frame(data),
            "Rttest data.frame has been modified inappropriately"=
              length(colnames(data)) == 2 &
              colnames(data)[1] == "Population" &
              colnames(data)[2] == "Value")
  data
}
