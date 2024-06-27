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
