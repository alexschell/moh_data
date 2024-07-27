# Function to flatten nested list
# From: https://stackoverflow.com/a/11919032

flatten<-function(x) {
  dumnames<-unlist(getnames(x,T))
  dumnames<-gsub("(*.)\\.1","\\1",dumnames)
  repeat {
    x <- do.call(.Primitive("c"), x)
    if(!any(vapply(x, is.list, logical(1)))){
      names(x)<-dumnames
      return(x)
    }
  }
}

getnames<-function(x,recursive){
  
  nametree <- function(x, parent_name, depth) {
    if (length(x) == 0) 
      return(character(0))
    x_names <- names(x)
    if (is.null(x_names)){ 
      x_names <- seq_along(x)
      x_names <- paste(parent_name, x_names, sep = "")
    }else{ 
      x_names[x_names==""] <- seq_along(x)[x_names==""]
      x_names <- paste(parent_name, x_names, sep = "")
    }
    if (!is.list(x) || (!recursive && depth >= 1L)) 
      return(x_names)
    x_names <- paste(x_names, ".", sep = "")
    lapply(seq_len(length(x)), function(i) nametree(x[[i]], 
                                                    x_names[i], depth + 1L))
  }
  nametree(x, "", 0L)
}
