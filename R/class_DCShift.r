.DCShift<- setClass(
    "DCShift",
    slots = list(
      lowPassTs= "matrix",
      testDce = "numeric",
      lengthDce = "numeric",
      startDce = "numeric",
      maxDce = "numeric"
    )
)


DCShift<- function(lowPassTs,testDce,lengthDce,startDce,maxDce) {

  .DCShift(
    lowPassTs=lowPassTs,
    testDce = testDce,
    lengthDce = lengthDce,
    startDce = startDce,
    maxDce = maxDce
  )
}



#' Print the DCShift object
#' @param object A DCShift object
#' @rdname show-DCShift-method
#' @export
setMethod("show", "DCShift", function(object) {
  cat("\nDCShift object\n")
    slots <- c("lowPassTs","testDce","lengthDce","startDce","maxDce")
  printSlots(object, slots = slots)
  cat("Use '$attr' to access the data\n")
  invisible(object)
})


#' Get the number of rows or columns of a DCShift object
#'
#' @param x A DCShift object
#'
#' @rdname dim-DCShift-method
setMethod("nrow", "DCShift", function(x) {
  nrow(x@lowPassTs)
})

#' @rdname dim-DCShift-method
setMethod("ncol", "DCShift", function(x) {
  ncol(x@lowPassTs)
})


#' Subset a DCShift object
#'
#' @param x A DCShift object
#' @param i A logical vector or a numeric vector of indices to subset the electrodes
#' @param j A logical vector or a numeric vector of indices to subset the time windows
#' @param ... Additional arguments (not used)
#' @param drop Additional arguments (not used)
#'
#' @rdname subset-DCShift-method
setMethod("[", "DCShift", function(x, i, j, ..., drop = FALSE) {

  if (!missing(i)){
    i <- checkIndex(i, x$electrodes)
  }else{
    i <- TRUE
  }

  if(missing(j)){
    j <- TRUE
  }

  lowPassTs_subset <- x@lowPassTs[i, j, drop = FALSE]
  testDce_subset = x@testDce[i]
  lengthDce_subset = x@lengthDce[i]
  startDce_subset = x@startDce[i]
  maxDce_subset = x@maxDce[i]

    .DCShift(
      lowPassTs_subset=lowPassTs,
      testDce=testDce_subset ,
      lengthDce=lengthDce_subset,
      startDce=startDce_subset,
      maxDce=maxDce_subset
   )
})

setMethod("$", "DCShift", function(x, name) {
  slot(x, name)
})

setMethod("$<-", "DCShift", function(x, name, value) {
  slot(x, name) <- value
  invisible(x)
})
