set.seed(1)
x = runif(10)

# 오름차순

bubble_sort_asc = function(vec) {
  
  n = length(vec)
  
  for (i in 1:(n-1)) 
    {
    for (j in 1:(n-i)) 
      {  
      if (vec[j] > vec[j + 1]) 
        {
        temp = vec[j]
        vec[j] = vec[j + 1]
        vec[j + 1] = temp
      }
    }
  }
  return(vec)
}

bubble_sort_asc(x)

# 내림차순

bubble_sort_desc = function(vec) {
  
  n = length(vec)
  
  for (i in 1:(n-1)) 
  {
    for (j in 1:(n-i)) 
    {  
      if (vec[j] < vec[j + 1]) 
      {
        temp = vec[j]
        vec[j] = vec[j + 1]
        vec[j + 1] = temp
      }
    }
  }
  return(vec)
}

bubble_sort_desc(x)


# 옵션으로 적용

bubble_sort = function(vec, order = "asc") {
  
  n = length(vec)
  
  for (i in 1:(n-1)) 
  {
    for (j in 1:(n-i)) 
    {  
      if ((order == "asc" && vec[j] > vec[j + 1]) ||
          (order == "desc" && vec[j] < vec[j + 1]))
      {
        temp = vec[j]
        vec[j] = vec[j + 1]
        vec[j + 1] = temp
      }
    }
  }
  return(vec)
}




