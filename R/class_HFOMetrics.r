.HFOMetrics<- setClass(
    "HFOMetrics",
    slots = list(
      hfoPow= "matrix",
      testHfo = "numeric",
      lengthHfo = "numeric",
      startHfo = "numeric",
      maxHfo = "numeric"
    )
)


HFOMetrics<- function(hfoPow,testHfo,lengthHfo,startHfo,maxHfo) {

  .HFOMetrics(
    hfoPow=hfoPow,
    testHfo = testHfo,
    lengthHfo = lengthHfo,
    startHfo = startHfo,
    maxHfo = maxHfo
  )
}



#' Print the HFOMetrics object
#' @param object A HFOMetrics object
#' @rdname show-HFOMetrics-method
#' @export
setMethod("show", "HFOMetrics", function(object) {
  cat("\nHFOMetrics object\n")
    slots <- c("hfoPow","testHfo","lengthHfo","startHfo","maxHfo")
  printSlots(object, slots = slots)
  cat("Use '$attr' to access the data\n")
  invisible(object)
})


#' Get the number of rows or columns of a HFOMetrics object
#'
#' @param x A HFOMetrics object
#'
#' @rdname dim-HFOMetrics-method
setMethod("nrow", "HFOMetrics", function(x) {
  nrow(x@hfoPow)
})

#' @rdname dim-HFOMetrics-method
setMethod("ncol", "HFOMetrics", function(x) {
  ncol(x@hfoPow)
})


#' Subset a HFOMetrics object
#'
#' @param x A HFOMetrics object
#' @param i A logical vector or a numeric vector of indices to subset the electrodes
#' @param j A logical vector or a numeric vector of indices to subset the time windows
#' @param ... Additional arguments (not used)
#' @param drop Additional arguments (not used)
#'
#' @rdname subset-HFOMetrics-method
setMethod("[", "HFOMetrics", function(x, i, j, ..., drop = FALSE) {

  if (!missing(i)){
    i <- checkIndex(i, x$electrodes)
  }else{
    i <- TRUE
  }

  if(missing(j)){
    j <- TRUE
  }

  hfoPow_subset <- x@hfoPow[i, j, drop = FALSE]
  testHfo_subset = x@testHfo[i]
  lengthHfo_subset = x@lengthHfo[i]
  startHfo_subset = x@startHfo[i]
  maxHfo_subset = x@maxHfo[i]

    .HFOMetrics(
      hfoPow_subset=hfoPow,
      testHfo=testHfo_subset ,
      lengthHfo=lengthHfo_subset,
      startHfo=startHfo_subset,
      maxHfo=maxHfo_subset
   )
})

setMethod("$", "HFOMetrics", function(x, name) {
  slot(x, name)
})

setMethod("$<-", "HFOMetrics", function(x, name, value) {
  slot(x, name) <- value
  invisible(x)
})
