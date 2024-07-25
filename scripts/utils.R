# Duplication related -------------------------------------------------

duplicates = function(...) duplicated(...) | duplicated(..., fromLast = TRUE)

duplicates_df = function(...) new_duplicated(...) | new_duplicated(..., fromLast = TRUE)

# `duplicated` method for data frames with support for `incomparables`
# https://stevenmortimer.com/the-unfinished-duplicated-function/
new_duplicated <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  
  if(!identical(incomparables, FALSE)) {
    n <- ncol(x)
    nmx <- names(x)
    nmincomparables <- names(incomparables)
    lincomparables <- length(incomparables)
    if(is.null(nmincomparables)) {
      if(lincomparables < n) {
        # pad any incomparables lists with the default value if list is shorter than the number columns in the supplied data.frame
        tmp <- c(incomparables, as.list(rep_len(FALSE, n - lincomparables)))
        names(tmp) <- nmx
        incomparables <- tmp 
      }
      if(lincomparables > n) {
        # if the list is unnamed and there are more elements in the list than there are columns, then only first n elements
        warning(paste("more columns in 'incomparables' than x, only using the first", n, "elements"))
        incomparables <- incomparables[1:n]
      }
    } else {
      # for named lists, find match, else default value
      tmp <- as.list(rep_len(FALSE, n))
      names(tmp) <- nmx
      i <- match(nmincomparables, nmx, 0L)
      if(any(i <= 0L))
        warning("not all columns named in 'incomparables' exist")
      tmp[ i[i > 0L] ] <- incomparables[i > 0L]
      incomparables <- tmp[nmx]
    }
    
    # first determine duplicates, then override when an incomparable value is found in a row since the existence of even 1 incomparable value in a row means it cannot be a duplicate
    res <- duplicated(do.call("paste", c(x, sep="\r")), fromLast = fromLast)
    
    #for better performance only bother with the columns that have incomparable values not set to the default: !identical(x, FALSE)
    run_incomp_check <- sapply(incomparables, FUN=function(x){!identical(x, FALSE)})
    if (sum(run_incomp_check) > 0L){
      incomp_check <- mapply(FUN=function(column,incomparables){match(column, incomparables)}, x[run_incomp_check], incomparables[run_incomp_check])
      # any rows with an incomparable match means, TRUE, it can override the duplicated result
      overwrite <- apply(data.frame(incomp_check), 1, function(x){any(!is.na(x))})
      res[overwrite] <- FALSE
    }
    
    return(res)
  } else if(length(x) != 1L) {
    duplicated(do.call("paste", c(x, sep="\r")), fromLast = fromLast)
  } else {
    duplicated(x[[1L]], fromLast = fromLast, ...)
  }
  
}

find_duplicates = function(df, key, incomparables = c(), ...) {
  
  if (length(key) == 1) {
    df = df[duplicates(df[[key]], incomparables = incomparables), ]
  } else if (length(key) > 1) {
    df = df[duplicates_df(df[key], incomparables = incomparables), ]
  } else {
    return(NULL)
  }
  
  dists = lapply(unique(df[[key]]), function(k) {
    d = pairwise_distance(df[df[[key]] == k, ], ...)
    d = data.frame(key = k, d)
    names(d)[-1] = paste0("dist_", names(d)[-1])
    d
  })
  
  out = merge(dplyr::bind_rows(dists), df[, c("id", key)], by.x = "key", by.y = key)
  out = merge(out, out[, c("id", "key")], by="key")
  out = out[out$id.x != out$id.y, ]
  names(out)[names(out) %in% c("id.x", "id.y")] = c("id", "duplicate_id")
  
  out
  
}

pairwise_distance = function(df, cols = NULL) {
  
  if (is.null(cols)) cols = names(df)
  col_classes = sapply(df[cols], class)
  
  out = list()
  
  for (k in cols) {
    if (all(!is.na(df[[k]]))) {
      if (col_classes[k] %in% c("character")) {
        out[[k]] = stringdist::stringdist(df[[k]][1], df[[k]][2]) / max(nchar(df[[k]][1:2]))
      } else if (col_classes[k] %in% c("numeric", "integer", "Date", "logical")) {
        out[[k]] = abs(as.numeric(df[[k]][1] - df[[k]][2]))
      }
    }
  }
  
  out
  
}


# Misc ----------------------------------------------------------------

# Two-step join for unique matches
w_join = function(df1, df2, col1, col2) {
  
  col1_matches = lapply(df1[[col1]], function(x) df2$id[which(df2[[col1]] == x)])
  col2_matches = lapply(df1[[col2]], function(x) df2$id[which(df2[[col2]] == x)])
  
  mapply(
    function(x, y) {
      if (length(x) == 1) {
        # unique match on col1
        x
      } else if (length(x) > 1 & length(intersect(x, y)) == 1) {
        # unique match on col1 & col2
        intersect(x, y)
      } else {
        # no match or non-unique match
        NA_integer_
      }
    },
    col1_matches,
    col2_matches
  )
  
}

# Check digit (for ID number)
check_luhn = function(x) {
  out = nchar(x) == 9
  out[!is.na(out) & out] = grepl("^[0-9]{9}$", x[!is.na(out) & out])
  tmp = sapply(
    strsplit(x[!is.na(out) & out], ""),
    function(u) {
      u = as.numeric(u)
      v = u[1:8] * rep(1:2, 4)
      v[v >= 10] = 1 + (v[v >= 10] %% 10)
      u[9] == -sum(v) %% 10
    }
  )
  out[!is.na(out) & out] = tmp
  out
}

# String distance
dstring = function(x, y) stringdist::stringdist(x, y) / pmax(nchar(x), nchar(y))

# Clean up names for joining
strip_names = function(x) {
  x = arabicStemR::removeDiacritics(x)
  x = arabicStemR::fixAlifs(x)
  gsub(" ", "", x)
}

# Clean up names
clean_name = function(x) {
  x = arabicStemR::fixAlifs(x)
  x = arabicStemR::removeDiacritics(x)
  x = gsub("(?<=عبد) ", "", x, perl = TRUE)
  x = gsub("(?<=ابو) ", "", x, perl = TRUE)
  x = gsub(" (?=الله)", "", x, perl = TRUE)
  x = gsub(" (?=الدين)", "", x, perl = TRUE)
  x
}
