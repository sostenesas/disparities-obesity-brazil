assert_has_cols <- function(df, cols, df_name = deparse(substitute(df))) {
  miss <- setdiff(cols, names(df))
  if (length(miss)) stop(sprintf("%s: faltam colunas: %s", df_name, paste(miss, collapse = ", ")), call. = FALSE)
  TRUE
}

assert_no_na <- function(df, cols, df_name = deparse(substitute(df))) {
  for (cl in cols) {
    if (anyNA(df[[cl]])) stop(sprintf("%s: NA encontrado em %s", df_name, cl), call. = FALSE)
  }
  TRUE
}

assert_in_range <- function(x, min, max, x_name = deparse(substitute(x))) {
  if (any(is.na(x))) stop(sprintf("%s: possui NA (validação range)", x_name), call. = FALSE)
  if (any(x < min | x > max)) stop(sprintf("%s: fora do intervalo [%s, %s]", x_name, min, max), call. = FALSE)
  TRUE
}
