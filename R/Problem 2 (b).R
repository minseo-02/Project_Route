quick_sort <- function(x, order = "asc") {
  
  if (length(x) <= 1) return(x)
  
  a <- x[1]
  b <- x[ceiling(length(x) / 2)]
  c <- x[length(x)]
  pivot <- median(c(a, b, c))
  
  if (order == "asc") {
    left  <- x[x < pivot]
    mid   <- x[x == pivot]
    right <- x[x > pivot]
    
    return(c(quick_sort(left, "asc"), mid, quick_sort(right, "asc")))
    
  } else if (order == "desc") {
    left  <- x[x > pivot]
    mid   <- x[x == pivot]
    right <- x[x < pivot]
    
    return(c(quick_sort(left, "desc"), mid, quick_sort(right, "desc")))
    
  } else {
    stop("order must be either 'asc' or 'desc'")
  }
}

quick_sort(x, order = "asc")





