process_biospecimen_availability <- function(path = "~/box/VMAC BIOSTAT/DATA/MAP/rawData/VMAP_biospecimens_collected_20210719.xlsx", epoch = 1:5) {
  biospecimen.list <- NULL

  for (epoch.i in epoch) {
    biospecimen.list[[epoch.i]] <- readxl::read_xlsx(path, sheet = epoch.i)
    biospecimen.list[[epoch.i]]$epoch <- epoch.i
  }

  biospecimen.df <- dplyr::bind_rows(biospecimen.list)

  biospecimen.df$plasma <- ifelse(biospecimen.df$plasma == "y", TRUE, FALSE)
  biospecimen.df$serum <- ifelse(biospecimen.df$serum == "y", TRUE, FALSE)
  biospecimen.df$DNA <- ifelse(biospecimen.df$DNA == "y", TRUE, FALSE)
  biospecimen.df$PAXGene <- ifelse(biospecimen.df$PAXGene == "y", TRUE, FALSE)
  biospecimen.df$csf <- ifelse(biospecimen.df$csf == "y", TRUE, FALSE)

  names(biospecimen.df) <- c("map.id", "plasma.availability", "serum.availability",
                             "dna.availability", "paxgene.availability", "csf.availability", "epoch")

  biospecimen.df <- format_id(biospecimen.df)

  biospecimen.df <- as.data.frame(biospecimen.df)

  return(biospecimen.df)
}
