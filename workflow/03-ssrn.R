source(here::here("workflow/02-railway_od.R"))
library(sf)
library(dplyr)
library(scanstatistics)
drake::loadd(list = c("kanto_jr_adjacent_matrix", "kanto_jr_allpassernger_matrix"))
plan_kanto4_hatunetu <-
  drake::drake_plan(
  df_kanto4 =
    readr::read_rds(here::here("data/kanto_jr_800mbuffer_hatunetu.rds")) %>%
    mutate(st_name = as.character(st_name)) %>%
    mutate(st_name = forcats::fct_relevel(st_name,
                                          rownames(kanto_jr_adjacent_matrix))) %>%
    arrange(st_name) %>%
    mutate(st_name = as.character(st_name)),
  counts =
    floor(df_kanto4$w.Fever) %>%
    purrr::set_names(df_kanto4$st_name),
  baselines =
    df_kanto4$e.hatu %>%
    dplyr::if_else(. == 0, 0.000001, .) %>%
    purrr::set_names(df_kanto4$st_name))

plan_scanstats <- drake::drake_plan(
  # Create network window ---------------------------------------------------
  zones =
    ssrn:::network_window(kanto_jr_adjacent_matrix,
                          kanto_jr_allpassernger_matrix,
                          type = "connected_B",
                          cluster_max = 46),
  # Scan --------------------------------------------------------------------
  poisson_result =
    scan_eb_poisson(counts = counts,
                    zones = zones,
                    baselines = baselines,
                    n_mcsim = 10000,
                    max_only = FALSE),
  # Cluster -----------------------------------------------------------------
  top = top_clusters(poisson_result,
                      zones,
                      k = 100,
                      overlapping = FALSE))
plan_ssrn <-
  drake::bind_plans(plan_kanto4_hatunetu, plan_scanstats)
drake::make(plan_ssrn)
