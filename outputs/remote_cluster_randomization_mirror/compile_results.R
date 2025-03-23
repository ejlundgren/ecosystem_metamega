library("data.table")

paths <- list.files("summaries/", full.names = T)

summaries <- lapply(paths, readRDS)

names(summaries) <- paths

summaries <- rbindlist(summaries, idcol = "remote_path")

saveRDS(summaries, "final_summarized_model_results.Rds")

print("Task complete")