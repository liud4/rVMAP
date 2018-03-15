# INSTEAD OF THIS, WRITE CODE SO THAT WE MAKE SURE THESE ARE ALL SPECIFIED AT THE BEGINNING.

# install and load packages
library("rVMAC")
load.pkg(c("Hmisc", "REDCapR"))

# define important directories
box.dir  <- file.path("~", "box") # should be in .Rprofile for each individual
vmac.dir <- file.path(box.dir, "VMAC BIOSTAT")
func.dir <- file.path(vmac.dir, "Useful Code")
data.dir <- file.path(vmac.dir, "DATA", "MAP")
data.raw.dir <- file.path(data.dir, "rawData")

# define time zone
Sys.setenv(TZ = "US/Central")

# define REDCap API URI
redcap.api.uri <- "https://redcap.vanderbilt.edu/api/"

# initialize list
MAPfreeze.list <- list(
  general = list(
    timestamp = format(Sys.time(), "%Y%m%d %H:%M", tz = "US/Central", usetz = FALSE),
    versions = c(
      R = R.version$version.string,
      REDCapR = paste0(packageVersion('REDCapR')),
      Hmisc = paste0(packageVersion('Hmisc')),
      REDCap = NA
    ),
    sysinfo = Sys.info()[c("sysname", "release", "nodename", "login", "user", "effective_user")],
    redcap.api.uri = redcap.api.uri
  ),
  epoch_0 = list(
    project = c(
      "MAP Eligibility",
      "MAP APOE"
    ),
    shortname = c(
      "eligibility",
      "apoe"
    ),
    token = c(
      "E62C4DFF1A0522D71A71547DB72097F5", # eligibility
      "CDCA25D3338A87CFD34F520B607CF2C4"  # apoe
    )
  ),
  epoch_1 = list(
    project = c(
      "MAP Enrollment",
      "MAP Cardiac MRI Analysis Epoch 1",
      "MAP Manual 3T Brain MRI Epoch 1",
      "MAP Manual 3T Brain MRI Breath Hold",
      "MAP Automated 3T Brain MRI Epoch 1",
      "MAP Automated 3T Brain MRI Breath Hold",
      "MAP Clinical CSF Epoch 1",
      "MAP Biomarkers Epoch 1",
      "MAP SRT Error Analysis Epoch 1",
      "MAP Addendum Epoch 1"
    ),
    shortname = c(
      "main",
      "cardiac.mri",
      "man3T",
      "man3T.bh",
      "auto3T",
      "auto3T.bh",
      "csf",
      "biomarkers",
      "srt",
      "addendum"
    ),
    token = c(
      "72681869457A4425A0E84501DC436550", # main
      "46F5516A50912E2EEAD8F4D0F25BD15A", # cardiac.mri
      "216A14EE098A971131F2014F982B379C", # man3T
      "2096DD854CF1C3FC57301FA8A6DC2E15", # man3T.bh
      "332C5EAA2B746BDD2CD3C02D104102D2", # auto3T
      "0ED21B06DCACA1BCCDE029F5E037093F", # auto3T.bh
      "1C00F8E4C7D174BA40C26C121C9F4A8D", # csf
      "9F9D6B900397C0A69EB2F7BE45E5C31B", # biomarkers
      "6CC401C1DAB7EADCA86CEBFC46279B57", # srt
      "81B1146D409C237EBAA0CE2913039BFC"  # addendum
    )
  ),
  epoch_2 = list(
    project = c(
      "MAP 18 Month",
      "MAP Cardiac MRI Analysis Epoch 2",
      "MAP Manual 3T Brain MRI Epoch 2",
      "MAP Automated 3T Brain MRI Epoch 2",
      "MAP Clinical CSF Epoch 2",
      "MAP Biomarkers Epoch 2"
    ),
    shortname = c(
      "main",
      "cardiac.mri",
      "man3T",
      "auto3T",
      "csf",
      "biomarkers"
    ),
    token = c(
      "542DCB71DADCAD1D29D075C4C4BEF206", # main
      "8A807C1831B4C15AF76DAFF89602D6D4", # cardiac.mri
      "A1D72073329E163FBA3ED839B4F8B613", # man3T
      "C47CA13E94BB3178E67E370473727AF8", # auto3T
      "8514E359947F7C7C35F44CE4306961DC", # csf
      "B8E6376BF00BD9D1505A7A44FD60D757"  # biomarkers
    )
  ),
  epoch_3 = list(
    project = c(
      "MAP 36 Month",
      "MAP Manual 3T Brain MRI Epoch 3",
      "MAP Automated 3T Brain MRI Epoch 3",
      "MAP Clinical CSF Epoch 3",
      "MAP Biomarkers Epoch 3"
    ),
    shortname = c(
      "main",
      "man3T",
      "auto3T",
      "csf",
      "biomarkers"
    ),
    token = c(
      "FC49B57BEBBFC7737888D2A6BF72A8FE", # main
      "6265D570148DAD5733AF74FBD68CEEBF", # man3T
      "23BE2F52914D1AB2F43082971B4FD8FD", # auto3T
      "04485C3158724FBF5CD17B8A9DFBC6F6", # csf
      "EE81833004CE2180E58343A699B9F517"  # biomarkers
    )
  ),
  epoch_4 = list(
    project = c(
      "MAP 60 Month",
      "MAP Manual 3T Brain MRI Epoch 4",
      "MAP Automated 3T Brain MRI Epoch 4"
    ),
    shortname = c(
      "main",
      "man3T",
      "auto3T"
    ),
    token = c(
      "2D737F024F909E0F80AAEDFEE1FB5944", # main
      "144B2C87251150590473B3A05D852C22", # man3T
      "C90BE8393C8C8146BD6F3B91CF7196A5"  # auto3T
    )
  )
)

###

for (index.epoch in 0:4){
  epoch <- paste0("epoch_", index.epoch)

  list.names <- MAPfreeze.list[[epoch]][["shortname"]]

  MAPfreeze.list[[epoch]][["data"]] <-
    MAPfreeze.list[[epoch]][["metadata"]] <-
    vector("list", length(list.names))

  names(MAPfreeze.list[[epoch]][["data"]]) <-
    names(MAPfreeze.list[[epoch]][["metadata"]]) <-
    names(MAPfreeze.list[[epoch]][["token"]]) <-
    list.names

  for (index.token in 1:length(MAPfreeze.list[[epoch]][["token"]])){

    current.shortname <- MAPfreeze.list[[epoch]][["shortname"]][index.token]
    current.token <- MAPfreeze.list[[epoch]][["token"]][index.token]

    df <- meta.df <- NULL

    # grab data
    df <- redcap_read_oneshot(
      redcap_uri = redcap.api.uri,
      token = current.token,
      raw_or_label = "raw",
      verbose = FALSE
    )$data

    # grab metadata
    meta.df <- redcap_metadata_read(
      redcap_uri = redcap.api.uri,
      token = current.token,
      verbose = FALSE
    )$data

    # write data
    MAPfreeze.list[[epoch]][["data"]][[current.shortname]] <- df
    MAPfreeze.list[[epoch]][["metadata"]][[current.shortname]] <- meta.df

    # clean up
    remove(df, meta.df)
  }
}

MAPfreeze.list$general$versions[["REDCap"]] <- redcap_version(
  redcap_uri = redcap.api.uri,
  token = MAPfreeze.list$epoch_1$token[1]
)

saveRDS(
  object = MAPfreeze.list,
  file = file.path(
    data.raw.dir,
    paste0(
      "MAPfreeze", "_", format(Sys.time(), "%Y%m%d"), ".rds"
    )
  )
)
