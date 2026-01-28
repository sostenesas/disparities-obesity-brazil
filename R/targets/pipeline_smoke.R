library(dplyr)
source("R/functions/validate.R")
source("R/functions/snapshot.R")

tar_smoke_targets <- function() {
  list(
    tar_target(
      smoke_raw,
      {
        set.seed(1)
        tibble::tibble(
          ano = sample(c(2013L, 2019L), 200, replace = TRUE),
          uf = sample(state.abb, 200, replace = TRUE),
          sexo = sample(c("M", "F"), 200, replace = TRUE),
          idade = sample(18:80, 200, replace = TRUE),
          imc = pmax(12, pmin(55, rnorm(200, mean = 27, sd = 5)))
        )
      }
    ),

    tar_target(
      smoke_checks_input,
      {
        assert_has_cols(smoke_raw, c("ano","uf","sexo","idade","imc"))
        assert_no_na(smoke_raw, c("ano","uf","sexo","idade","imc"))
        assert_in_range(smoke_raw$imc, 10, 70)
        TRUE
      }
    ),

    tar_target(
      smoke_summary,
      smoke_raw %>%
        mutate(obesidade = imc >= 30) %>%
        group_by(ano) %>%
        summarise(prop_obes = mean(obesidade), .groups = "drop")
    ),

    tar_target(
      smoke_snapshot,
      {
        snap <- smoke_summary %>%
          mutate(prop_obes = round(prop_obes, 6)) %>%
          arrange(ano)

        snapshot_expect_equal(snap, "smoke_summary")
        TRUE
      }
    )
  )
}
