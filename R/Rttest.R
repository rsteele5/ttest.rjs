#' Rttest
#'
#' This function preforms several types of t-tests tailored towards user input.
#'
#' @param x First sample vector
#' @param y Second sample vector
#' @param alpha determines range of the confidence interval's distance from 100\%
#' @param paired Whether the samples are paired
#'
#' @importFrom stats t.test var.test
#'
#' @return An Rttest class obj containing a list comprised of a data.frame of
#' the given samples (x,y), alpha value, confidence interval of type "conf.level",
#' and p value.
#' @export
#'
#' @examples
#' x <- rnorm(30,5,2)
#' y <- rnorm(40,3,2)
#' a <- 0.1
#' obj <- Rttest(x, y, a)
#'
Rttest = function(x, y, alpha=0.05, paired=F){
  #### Input Error Checking ####
  stopifnot("`x` must be vector"= is.vector(x),
            "`y` must be vector"= is.vector(y),
            "`x` must contain at least 1 element"= length(x)>0,
            "`y` must contain at least 1 element"= length(y)>0,
            "`paired` must be a logical"= is.logical(paired),
            "`alpha` must be numeric"= is.number(alpha),
            "`alpha` must be between 0 and 1"= (alpha<=1|alpha>=0))

  #### T-Test Analysis and Execution ####
  # Test the Variance with an F-test
  ft = var.test(x, y, alternative = "two.sided")
  # Determine the type of test
  type = "Welch"
  if (paired) {type = "Paired"}
  else if (ft$p.value > alpha) {type = "T-test"}
  # Perform the t-test with the derived info
  tt = t.test(x,y, paired=paired, conf.level=1-alpha,
              # if p value of the F-test is greater than alpha then there
              # is no significant difference between the variances (TRUE)
              # else there is a significant difference (FALSE)
              var.equal = ft$p.value > alpha)

  #### Output data ####
  pop=rep(c("x", "y"), c(length(x),length(y))) # population identifiers
  obj = list(test_type=type,
             data=data.frame(Population=pop,Value=c(x,y)),
             alpha=alpha,
             ci=as.vector(tt$conf.int), # stripped of "conf.int" attr
             pvalue=tt$p.value)
  class(obj) = "Rttest"
  obj
}



#' Print.Rttest
#'
#' Displays the confidence interval and type of test preformed. If `html` is set
#' to `True` then the 2 samples x and y are also printed using kable.
#'
#' @param x Rttest object.
#' @param html a logical determining whether or not to utilize kable for sample
#' output.
#' @param width The width in pixels(px). (defualt: NULL)
#' @param height The height in pixels(px). This can be set to NULL for and
#' unbounded height. (defualt: 400)
#' @param ... unused
#'
#' @importFrom kableExtra scroll_box kable_styling kbl
#' @export
#'
#' @examples
#' x <- rnorm(30,5,2)
#' y <- rnorm(40,3,2)
#' a <- 0.1
#' obj <- Rttest(x, y, a)
#'
#' print(obj)
#' print(obj, TRUE, 300)
#' print(obj, TRUE, 400, 200)
#'
print.Rttest = function(x, html=F, width=NULL, height=400, ...){
  #### Input Error Checking ####
  stopifnot("`width` must be number"= is.number(width)|is.null(width),
            "`height` must be number"= is.number(height)|is.null(height),
            "`html` must be a logical"= is.logical(html))
  data = has.Rttest.data(x)
  #### Data Conversion ####
  print(x["ci"])
  print(x["test_type"])
  if(html){
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
}

#' Plot Diagnostics for an Rttest Object
#'
#' A customizable box plot of the two samples contained within the Rttest object
#' using ggplot2
#'
#' @param x Rttest object.
#' @param main main title for the plot.
#' @param sub sub-title for the plot.
#' @param poplab a label to denote what population the samples were taken from
#' (default: "Population")
#' @param vlab a label to denote the values of the samples (default: "Value")
#' @param ... unused
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_line geom_point labs
#' @export
#'
#' @examples
#' x <- rnorm(30,5,2)
#' y <- rnorm(40,3,2)
#' a <- 0.1
#' obj <- Rttest(x, y, a)
#'
#' plot(obj)
#' plot(obj, main="My Custom Title", sub="Which also has a sub-title",
#'      poplab="My Population", vlab="My Value Type")
#'
plot.Rttest = function(x, main="Comparison of Value Distripution per Sample",
                       sub=NULL, poplab="Population", vlab="Value", ...){
  #### Global Variable Instantiation ####
  Population = NULL
  Value = NULL
  pairs = NULL
  #### Input Error Checking ####
  stopifnot("`main` must be string"= is.stringORnull(main),
            "`sub` must be string"= is.stringORnull(sub),
            "`poplab` must be string"= is.string(poplab),
            "`vlab` must be string"= is.string(vlab))
  data = has.Rttest.data(x)
  #### Generate and return plot ####
  # If paired link pairs together
  if(x$test_type == "Paired"){
    xl = nrow(data[data$Population == "x",])
    yl = nrow(data[data$Population == "y",])
    data$pairs = c(1:xl,1:yl)
  }

  # create 2 sample base plot with labels
  rpt = ggplot(data, aes(x=Population, y=Value)) +
        geom_boxplot(aes(fill=Population)) +
        labs(title=main, subtitle=sub, x=poplab, y=vlab, fill=poplab)

  # If paired connect pairs to show "before and after" link and update title
  if(x$test_type == "Paired"){
    rpt = rpt + geom_line(aes(group = pairs)) +
          geom_point(size = 2) +
          labs(title=paste("Paired ",main))
  }

  rpt
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
