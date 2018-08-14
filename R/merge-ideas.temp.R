#' A preprocessing function to replace large negative values with NA
#'
#' @param dat A dataframe containing VMAC variables
#' @param missing.values A vector containing the VMAC values to be recoded as NA.
#' @return The input dataframe with the specified values replaced with NA.
#' @export

#dat %>%
#  filter(map_id > 0) %>%
#  arrange(map_id) %>%
#  missingtoNA(., missing.values = rep(-1111, 9)*1:9) %>% # ???do?
#  mutate(
#    map_id = str_pad(
#      string = map_id,
#      width = 3,
#      side = "left",
#      pad = 0
#    )
#  )


#all: missingtoNA
#map_id: remove missing and negatives; remove duplicates; convert to three-digit string
#arrange: `map_id`
