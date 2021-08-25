process_biospecimen_availability <- function(path = "~/box/VMAC BIOSTAT/DATA/MAP/rawData/VMAP_biospecimens_collected_20210719.xlsx", epoch = 1:5) {
  biospecimen.list <- NULL

  for (epoch.i in epoch) {
    biospecimen.list[[epoch.i]] <- readxl::read_xlsx(path, sheet = epoch.i)
    biospecimen.list[[epoch.i]]$epoch <- epoch.i
  }

  biospecimen.df <- dplyr::bind_rows(biospecimen.list) %>%
    format_id()

  biospecimen.df$plasma <- ifelse(biospecimen.df$plasma == "y", "Yes", "No")
  biospecimen.df$serum <- ifelse(biospecimen.df$serum == "y", "Yes", "No")
  biospecimen.df$DNA <- ifelse(biospecimen.df$DNA == "y", "Yes", "No")
  biospecimen.df$PAXGene <- ifelse(biospecimen.df$PAXGene == "y", "Yes", "No")
  biospecimen.df$csf <- ifelse(biospecimen.df$csf == "y", "Yes", "No")

  names(biospecimen.df) <- c("map.id", "plasma.availability", "serum.availability",
                             "dna.availability", "paxgene.availability", "csf.availability",
                             "epoch")

  biospecimen.df <- biospecimen.df %>%
    mutate(
      plasma.availability = factor(plasma.availability, levels = c("No", "Yes")),
      serum.availability = factor(serum.availability, levels = c("No", "Yes")),
      dna.availability = factor(dna.availability, levels = c("No", "Yes")),
      paxgene.availability = factor(paxgene.availability, levels = c("No", "Yes")),
      csf.availability = factor(csf.availability, levels = c("No", "Yes")),
    ) %>%
    as.data.frame()

  return(biospecimen.df)
}
