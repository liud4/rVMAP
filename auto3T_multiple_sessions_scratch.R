# test$data <- test$data[, c(1:189, 191, 190, 192:2234)]

indices.df <- dplyr::bind_cols(start = as.integer(grep("map\\_id", names(test$data))[-length(grep("map\\_id", names(test$data)))]),
                 end = as.integer(grep("map\\_id", names(test$data))[-1] - 1),
                 instrument = c('freesurfer_2005',
                                'freesurfer_2009',
                                'freesurfer_lobes',
                                'freesurfer_hippocampal_subfields',
                                'freesurfer_brain_volume',
                                'ma',
                                'wml',
                                'vbmqa',
                                'fslfirst',
                                'eve',
                                'asl_scan',
                                'dtiqa',
                                'veasl'
                 )
)

id.df <- dplyr::bind_cols(session_id = test$data$session_id, purrr::map_dfc(indices$start, function(x) test$data[!is.na(x), x]))
epoch.df <- test$data[, grep("epoch", names(test$data), v = T)[-length(grep("epoch", names(test$data), v = T))]]

# all map IDs
purrr::map_chr(1:16, function(x) max(df[x, -1], na.rm = TRUE))

reference.df <- dplyr::bind_cols(
  map_id = purrr::map_chr(1:16, function(x) max(id.df[x, -1], na.rm = TRUE)),
  epoch = purrr::map_int(1:16, function(x) min(epoch.df[x, ], na.rm = TRUE)),
  session_id = test$data$session_id
) %>%
  rowwise() %>%
  arrange(
    epoch, map_id
  )

# which MAP IDs multiple sessions in more than 1 epoch:
# 232: epoch 1 and epoch 2
# 300: epoch 1 and epoch 3
table(reference.df$map_id, reference.df$epoch)[rowSums(table(reference.df$map_id, reference.df$epoch)) > 1, ]
