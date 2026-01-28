snapshot_path <- function(name) {
  dir.create("tests/snapshots", recursive = TRUE, showWarnings = FALSE)
  file.path("tests/snapshots", paste0(name, ".rds"))
}

snapshot_expect_equal <- function(object, name, tolerance = 1e-8) {
  path <- snapshot_path(name)

  if (!file.exists(path)) {
    saveRDS(object, path)
    message("Snapshot criado: ", path)
    return(TRUE)
  }

  old <- readRDS(path)
  equal <- isTRUE(all.equal(old, object, tolerance = tolerance, check.attributes = FALSE))

  if (!equal) {
    new_path <- file.path("tests/snapshots", paste0(name, "_NEW.rds"))
    saveRDS(object, new_path)
    stop(
      sprintf("Snapshot mudou: %s\nNovo salvo em: %s\nSe mudança for esperada, substitua o snapshot antigo.", path, new_path),
      call. = FALSE
    )
  }
  TRUE
}
