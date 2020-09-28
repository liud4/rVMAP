ma.df <- dplyr::bind_cols(
  variable = c(
    "ma.grey.matter",
    "ma.left.hemisphere",
    "ma.right.hemisphere",
    "ma.frontal.lobe.vol",
    "ma.occipital.lobe.vol",
    "ma.parietal.lobe.vol",
    "ma.temporal.lobe.vol",
    "ma.hippocampus.vol",
    "ma.inf.lat.vent.vol"
  ),
  label = c(
    "Total Grey Matter",
    "Left Hemisphere Grey Matter",
    "Right Hemisphere Grey Matter",
    "Frontal Lobe Grey Matter",
    "Occipital Lobe Grey Matter",
    "Parietal Lobe Grey Matter",
    "Temporal Lobe Grey Matter",
    "Hippocampus Volume",
    "Inferior Lateral Ventricle Volume"
  ),
  unit = c(
    "mm³",
    "mm³",
    "mm³",
    "mm³",
    "mm³",
    "mm³",
    "mm³",
    "mm³",
    "mm³"
  )
)

wmh.df <- dplyr::bind_cols(
 variable = c(
   "wml.volume",
   "wml.volume.left.hemisphere",
   "wml.volume.right.hemisphere",
   "wml.volume.frontal.lobe",
   "wml.volume.parietal.lobe",
   "wml.volume.temporal.lobe",
   "wml.volume.occipital.lobe"
 ),
 label = c(
   "WMH Volume",
   "WMH Left Hemisphere Volume",
   "WMH Right Hemisphere Volume",
   "WMH Frontal Lobe Volume",
   "WMH Parietal Lobe Volume",
   "WMH Temporal Lobe Volume",
   "WMH Occipital Lobe Volume"
 ),
 unit = c(
   "cm³",
   "cm³",
   "cm³",
   "cm³",
   "cm³",
   "cm³",
   "cm³"
 )
)

wmh.log.df <- dplyr::bind_cols(
  variable = paste0(wmh.df$variable, ".plus.1.log"),
  label = wmh.df$label,
  unit = paste0("log ", wmh.df$unit)
)

np.df <- dplyr::bind_cols(
  variable = c(
    "np.moca",
    "np.bnt",
    "np.anim",
    "np.tmta",
    "np.digsymb",
    "np.executive.composite",
    "np.tower",
    "np.inhibit",
    "np.fas",
    "np.tmtb",
    "np.tmtb.log",
    "np.hvot",
    "np.memory.composite",
    "np.biber.t1to5",
    "np.biberb",
    "np.biber.ld",
    "np.biber.discrim",
    "np.cvlt1to5",
    "np.cvltb",
    "np.cvlt.ldfr",
    "np.cvltrecog.discrim"
  ),
  label = c(
    "Montreal Cognitive Assessment Test",
    "Boston Naming Test (30 Item)",
    "Animal Naming",
    "Number Sequencing",
    "Coding",
    "Executive Function Composite",
    "DKEFS Tower Test",
    "Color-Word Inhibition",
    "FAS, Total",
    "Letter-Number Switching",
    "Letter-Number Switching",
    "Hooper Visual Organization Test",
    "Memory Composite",
    "BFLT Trial 1-5 Learning",
    "BFLT Interference Trial",
    "BFLT Long Delayed Free Recall",
    "BFLT Recognition, Total",
    "CVLT-II Trial 1-5 Learning",
    "CVLT-II Interference Trial",
    "CVLT-II Long Delayed Free Recall",
    "CVLT-II Recognition"
  ),
  unit = c(
    "Total score",
    "Total score",
    "Total score",
    "Time to completion (s)",
    "Total score",
    "Total (z-score)",
    "Total score",
    "Time to completion (s)",
    "Total score",
    "Time to completion (s)",
    "Log time to completion (s)",
    "Total score",
    "Total (z-score)",
    "Total score",
    "Total score",
    "Total score",
    "Total score",
    "Total score",
    "Total score",
    "Total score",
    "Total score"
  )
)

AD.sig.df <- dplyr::bind_cols(
  variable = c(
    "AD.sig.schwarz",
    "AD.sig.mcevoy"
  ),
  label = c(
    "AD Signature",
    "AD Signature"
  ),
  unit = c(
    NA,
    NA
  )
)

asl.df <- dplyr::bind_cols(
  variable = c(
    "asl.rest.grey.matter.hct",
    "asl.rest.frontal.lobe.hct",
    "asl.rest.occipital.lobe.hct",
    "asl.rest.temporal.lobe.hct",
    "asl.rest.parietal.lobe.hct",
    "asl.reac.grey.matter.hct",
    "asl.reac.frontal.lobe.hct",
    "asl.reac.occipital.lobe.hct",
    "asl.reac.temporal.lobe.hct",
    "asl.reac.parietal.lobe.hct"
  ),
  label = c(
    "Whole Brain CBF",
    "Frontal Lobe CBF",
    "Occipital Lobe CBF",
    "Temporal Lobe CBF",
    "Parietal Lobe CBF",
    "Whole Brain CVR",
    "Frontal Lobe CVR",
    "Occipital Lobe CVR",
    "Temporal Lobe CVR",
    "Parietal Lobe CVR"
  ),
  unit = c(
    "mL/100 g/min",
    "mL/100 g/min",
    "mL/100 g/min",
    "mL/100 g/min",
    "mL/100 g/min",
    "% change",
    "% change",
    "% change",
    "% change",
    "% change"
  )
)

oef.df <- dplyr::bind_cols(
  variable = c(
    "oef.oef",
    "oef.cmro2",
    "oef.oef.hct",
    "oef.cmro2.hct"
  ),
  label = c(
    "Oxygen Extraction Fraction",
    "Cerebral Metabolic Rate of Oxygen",
    "Oxygen Extraction Fraction",
    "Cerebral Metabolic Rate of Oxygen"
  ),
  unit = c(
    "%",
    "µmol/100 g/min",
    "%",
    "µmol/100 g/min"
  )
)

####

figure_labels.df <- dplyr::bind_rows(
  ma.df, wmh.df, wmh.log.df, np.df, AD.sig.df, asl.df, oef.df
)

figure_labels.df$combined_label <- paste0(figure_labels.df$label, ", ", figure_labels.df$unit)
figure_labels.df[figure_labels.df$variable %in% c(np.df$variable, AD.sig.df$variable), "combined_label"] <- figure_labels.df[figure_labels.df$variable %in% c(np.df$variable, AD.sig.df$variable), "label"]
