# 8 June 2016
## Updated variable names and labels

# 07 Oct 2016, LS:
## Adding labels for new pvs2 vars
## Adding processing for pvs2 factor vars --- removed 19 Oct

# 19-20 Oct 2016, LS:
## Removing processing for pvs2 factor vars--- we do not need them
## Removing code for session ID
## Modifying labels for scan dates

# 27 Feb 2017, JN
## Updating pvs coding scheme names

manual3T <- function(dat){
  # Returns dat with manual 3T imaging-related variables labeled
  
    # This code does not seem right to me. Why not xxx.man3T on RHS? -LS
    # I am commenting out 20 Oct 2016
      # session.id has different format in regular and Breath Hold
      #dat$session.id.man3T <- as.numeric(as.character(dat$session.id))
      #dat$session.id.2.man3T <- as.numeric(as.character(dat$session.id.2))
      #dat$session.id.3.man3T <- as.numeric(as.character(dat$session.id.3))
      #dat$session.id.4.man3T <- as.numeric(as.character(dat$session.id.4))
  
  dat <- within(dat, {
    label(session.id.2)="Session ID"
    label(scan.date.2)="Scan date"
    label(cow.primary)="primary data entry person"
    label(cow.secondary)="secondary data entry person"
    label(cow.acoa)="CoW AcoA"
    label(cow.a1.l)="CoW A1 Left"
    label(cow.a1.r)="CoW A1 Right"
    label(cow.pcoa.l)="CoW PcoA Left"
    label(cow.pcoa.r)="CoW PcoA Right"
    label(cow.p1.l)="CoW P1 Left"
    label(cow.p1.r)="CoW P1 Right"
    label(cow.variant)="CoW Variant"
    label(cow.variant.other)="CoW Variant - Other Description"
    label(cow.notes)="CoW Variant Notes"
    label(circle.of.willis.complete)="Complete?"
    label(session.id.3)="Session ID"
    label(scan.date.3)="Scan date"
    label(swi.usable)="Is SWI data usable?"
    label(swi.microbleeds.number)="Total number of microbleeds"
    label(swi.microbleeds.side)="Side of microbleeds"
    label(swi.microbleeds.pattern)="Predominant pattern of microbleeds"
    label(swi.microbleeds.location...0)="Location of microbleeds (choice=Brainstem)"
    label(swi.microbleeds.location...1)="Location of microbleeds (choice=Basal ganglia)"
    label(swi.microbleeds.location...2)="Location of microbleeds (choice=Caudate nucleus)"
    label(swi.microbleeds.location...3)="Location of microbleeds (choice=Cerebellum)"
    label(swi.microbleeds.location...4)="Location of microbleeds (choice=Frontal)"
    label(swi.microbleeds.location...5)="Location of microbleeds (choice=Frontal periventricular)"
    label(swi.microbleeds.location...6)="Location of microbleeds (choice=Lobar)"
    label(swi.microbleeds.location...7)="Location of microbleeds (choice=Occipital)"
    label(swi.microbleeds.location...8)="Location of microbleeds (choice=Occipital periventricular)"
    label(swi.microbleeds.location...9)="Location of microbleeds (choice=Thalamus)"
    label(swi.microbleeds.location...10)="Location of microbleeds (choice=Parietal periventricular)"
    label(swi.microbleeds.location...11)="Location of microbleeds (choice=Temporal)"
    label(swi.microbleeds.location...12)="Location of microbleeds (choice=Temporal periventricular)"
    label(swi.microbleeds.location....8888)="Location of microbleeds (choice=N/A)"
    label(swi.microbleeds.location....9999)="Location of microbleeds (choice=Missing)"
    label(swi.microbleeds.distribution...0)="Distribution of microbleeds (choice=Cortical)"
    label(swi.microbleeds.distribution...1)="Distribution of microbleeds (choice=Subcortical)"
    label(swi.microbleeds.distribution....8888)="Distribution of microbleeds (choice=N/A)"
    label(swi.microbleeds.distribution....9999)="Distribution of microbleeds (choice=Missing)"
    label(swi.microbleed.notes)="Notes"
    label(swi.complete)="Complete?"
    label(session.id.4)="Session ID"
    label(scan.date.4)="Scan date"
    label(lacunar.infarcts.usable)="Is Lacunar Infarcts Data usable?"
    label(lacunar.infarcts.number)="Number of Lacunar Infarcts "
    label(lacunar.infarcts.distribution...0)="Distribution of lacunar infarcts (choice=Cortical)"
    label(lacunar.infarcts.distribution...1)="Distribution of lacunar infarcts (choice=Subcortical)"
    label(lacunar.infarcts.distribution....8888)="Distribution of lacunar infarcts (choice=N/A)"
    label(lacunar.infarcts.distribution....9999)="Distribution of lacunar infarcts (choice=Missing)"
    label(lacunar.infarcts.note)="Lacunar Infarcts Notes"
    #label(session.id.5)="Session ID"
    #label(scan.date.5)="Scan Date"
    label(pvs.pat.usable)="Is PVS Data (Patankar) usable?"
    label(pvs.pat.centrum)="Perivascular Spaces Score Centrum (Patankar) "
    label(pvs.pat.mesencephalon)="Perivascular Spaces Score Mesencephalon (Patankar) "
    label(pvs.pat.subinsular)="Perivascular Spaces Score Subinsular (Patankar) "
    label(pvs.pat.basal.ganglia)="Perivascular Spaces Score Basal Ganglia (Patankar) "
    label(pvs.han.usable)="Is PVS Data (Hansen) usable?"
    label(pvs.han.centrum)="Perivascular Spaces Score Centrum (Hansen)"
    label(pvs.han.basal.ganglia)="Perivascular Spaces Score Basal Ganglia (Hansen)"
    label(pvs.han.total)="Perivascular Spaces Total Score (Hansen)"
    label(lacunar.infarcts.and.perivascular.space.complete)="Complete?"
    
    pvs.pat.basal.ganglia.factor <- ifelse(is.na(pvs.pat.basal.ganglia), NA, ifelse(pvs.pat.basal.ganglia <=2, 0,1))
    pvs.pat.basal.ganglia.factor <- factor(pvs.pat.basal.ganglia.factor,levels=c(0,1),labels=c("Low","High"))
    label(pvs.pat.basal.ganglia.factor) <- paste0(label(pvs.pat.basal.ganglia), ", dichotomized [0-2]vs.[3-5]")
    
    pvs.pat.centrum.factor <- ifelse(is.na(pvs.pat.centrum), NA, ifelse(pvs.pat.centrum<=1, 0,1))
    pvs.pat.centrum.factor <- factor(pvs.pat.centrum.factor,levels=c(0,1),labels=c("Low","High"))
    label(pvs.pat.centrum.factor) <- paste0(label(pvs.pat.centrum), ", dichotomized [0-1]vs.[2+]")
    
    pvs.han.basal.ganglia.factor <- ifelse(is.na(pvs.han.basal.ganglia), NA, ifelse(pvs.han.basal.ganglia<=1, 0, 1))
    pvs.han.basal.ganglia.factor <- factor(pvs.han.basal.ganglia.factor,
                                           levels= c(0, 1), labels= c("Low", "High")) 
    label(pvs.han.basal.ganglia.factor) <- paste0(label(pvs.han.basal.ganglia), ", dichotomized [0-1]vs.[2+]")
    
    pvs.han.centrum.factor <- ifelse(is.na(pvs.han.centrum<=1), NA, ifelse(pvs.han.centrum<=1,0,1))
    pvs.han.centrum.factor <- factor(pvs.han.centrum.factor,levels=c(0,1),labels=c("Low","High"))
    label(pvs.han.centrum.factor) <- paste0(label(pvs.han.centrum), ", dichotomized [0-1]vs.[2+]")
    
    pvs.han.total.factor <- ifelse(is.na(pvs.han.total), NA, ifelse(pvs.han.total<=3,0,1))
    pvs.han.total.factor <- factor(pvs.han.total.factor,levels=c(0,1),labels=c("Low","High"))
    label(pvs.han.total.factor) <- paste0(label(pvs.han.total), ", dichotomized [0-3]vs.[4+]")
    
    swi.microbleeds.number.orig <- swi.microbleeds.number
    swi.microbleeds.number <- as.character(swi.microbleeds.number)
    swi.microbleeds.number <- ifelse(swi.microbleeds.number == "", NA,
                                     swi.microbleeds.number)
    swi.microbleeds.number <- ifelse(swi.microbleeds.number == "-9999", NA, swi.microbleeds.number) 
    
    swi.microbleeds.number <- gsub("+", "", swi.microbleeds.number, fixed= TRUE)
    swi.microbleeds.number <- as.numeric(swi.microbleeds.number)
    label(swi.microbleeds.number) <- paste0(label(swi.microbleeds.number.orig), ", recoded")
    
    swi.microbleeds.number.factor <- ifelse(is.na(swi.microbleeds.number), NA, ifelse(swi.microbleeds.number<1,0,1))
    swi.microbleeds.number.factor <- factor(swi.microbleeds.number.factor,levels=c(0,1),labels=c("No","Yes"))
    label(swi.microbleeds.number.factor) <- "Microbleeds Present"
    
    lacunar.infarcts.number.factor=ifelse(is.na(lacunar.infarcts.number), NA, ifelse(lacunar.infarcts.number<1,0,1))
    lacunar.infarcts.number.factor<-factor(lacunar.infarcts.number.factor, levels=c(0,1),labels=c("No","Yes"))
    label(lacunar.infarcts.number.factor) <- 'Lacunar Infarcts Present'
    
    #Derive bHold versions
    bHold.pvs.han.basal.ganglia.factor <- ifelse(bHold.pvs.han.basal.ganglia >= 2, 1, 0)
    bHold.pvs.han.basal.ganglia.factor <- factor(bHold.pvs.han.basal.ganglia.factor,
                                             levels= c(0, 1), labels= c("Low", "High")) 
    
    bHold.pvs.han.centrum.factor <- ifelse(bHold.pvs.han.centrum<=1,0,1)
    bHold.pvs.han.centrum.factor <- factor(bHold.pvs.han.centrum.factor,levels=c(0,1),labels=c("Low","High"))
    
    bHold.pvs.han.total.factor <- ifelse(bHold.pvs.han.total<3,0,1)
    bHold.pvs.han.total.factor <- factor(bHold.pvs.han.total.factor,levels=c(0,1),labels=c("Low","High"))
    
    bHold.swi.microbleeds.number.orig <- bHold.swi.microbleeds.number
    bHold.swi.microbleeds.number <- as.character(bHold.swi.microbleeds.number)
    bHold.swi.microbleeds.number <- ifelse(bHold.swi.microbleeds.number == "", NA,
                                           bHold.swi.microbleeds.number)
    bHold.swi.microbleeds.number <- ifelse(bHold.swi.microbleeds.number == "-9999", NA, bHold.swi.microbleeds.number) 
    
    bHold.swi.microbleeds.number <- gsub("+", "", bHold.swi.microbleeds.number, fixed= TRUE)
    bHold.swi.microbleeds.dichotomous <- ifelse(bHold.swi.microbleeds.number>=1,1,0)
    bHold.swi.microbleeds.dichotomous <- factor(bHold.swi.microbleeds.dichotomous,levels=c(1,0),labels=c("Yes","No"))
    
    bHold.pvs.pat.basal.ganglia.factor <- 
      ifelse(is.na(bHold.pvs.pat.basal.ganglia),NA,ifelse(bHold.pvs.pat.basal.ganglia %in% c(0, 1, 2), 0,1))
    
    bHold.pvs.pat.basal.ganglia.factor <- factor(bHold.pvs.pat.basal.ganglia.factor,levels=c(0,1),labels=c("Low","High"))
    label(bHold.pvs.pat.basal.ganglia) <- label(bHold.pvs.pat.basal.ganglia.factor) <- paste0(label(bHold.pvs.pat.basal.ganglia), ", dichotomized [0-2]vs.[3-5]")
    
    bHold.pvs.pat.centrum.factor <- 
      ifelse(is.na(bHold.pvs.pat.centrum),NA,ifelse(bHold.pvs.pat.centrum %in% c(0, 1), 0,1))
    
    bHold.pvs.pat.centrum.factor <- factor(bHold.pvs.pat.centrum.factor,levels=c(0,1),labels=c("Low","High"))
    label(bHold.pvs.pat.centrum) <- label(bHold.pvs.pat.centrum.factor) <- paste0(label(bHold.pvs.pat.centrum), ", dichotomized [0-1]vs.[2+]")
    
    bHold.pvs.han.basal.ganglia.factor <- ifelse(bHold.pvs.han.basal.ganglia >= 2, 1, 0)
    bHold.pvs.han.basal.ganglia.factor <- factor(bHold.pvs.han.basal.ganglia.factor,
                                           levels= c(0, 1), labels= c("Low", "High")) 
    label(bHold.pvs.han.basal.ganglia) <- label(bHold.pvs.han.basal.ganglia.factor) <-paste0(label(bHold.pvs.han.basal.ganglia), ", dichotomized [0-1]vs.[2+]")
    
    bHold.pvs.han.centrum.factor <- ifelse(bHold.pvs.han.centrum<=1,0,1)
    bHold.pvs.han.centrum.factor <- factor(bHold.pvs.han.centrum.factor,levels=c(0,1),labels=c("Low","High"))
    label(bHold.pvs.han.centrum) <- label( bHold.pvs.han.centrum.factor) <-  paste0(label(bHold.pvs.han.centrum), ", dichotomized [0-1]vs.[2+]")
    
    
    bHold.pvs.han.total.factor <- ifelse(bHold.pvs.han.total<=3,0,1)
    bHold.pvs.han.total.factor <- factor(bHold.pvs.han.total.factor,levels=c(0,1),labels=c("Low","High"))
    label(bHold.pvs.han.total) <- label(bHold.pvs.han.total.factor) <-  paste0(label(bHold.pvs.han.total), ", dichotomized [0-2]vs.[3+]")
    
    ica.right.thickness.total <- vwi.right.ica.od - vwi.right.ica.id
    ica.left.thickness.total  <- vwi.left.ica.od  - vwi.left.ica.id
    aca.right.thickness.total <- vwi.right.aca.od - vwi.right.aca.id
    aca.left.thickness.total  <- vwi.left.aca.od  - vwi.left.aca.id
    mca.right.thickness.total <- vwi.right.mca.od - vwi.right.mca.id
    mca.left.thickness.total  <- vwi.left.mca.od  - vwi.left.mca.id
    vb.thickness.total        <- vwi.vb.od - vwi.vb.id
    
    vwi.ica.id                <- (vwi.right.ica.id+vwi.left.ica.id)/2
    vwi.aca.id                <- (vwi.right.aca.id+vwi.left.aca.id)/2
    vwi.mca.id                <- (vwi.right.mca.id+vwi.left.mca.id)/2
        
    label(ica.right.thickness.total) <- 'Right ICA total wall thickness, mm'
    label(ica.left.thickness.total)  <- 'Left ICA total wall thickness, mm'
    label(aca.right.thickness.total) <- 'Right ACA total wall thickness, mm'
    label(aca.left.thickness.total)  <- 'Left ACA total wall thickness, mm'
    label(mca.right.thickness.total) <- 'Right MCA total wall thickness, mm'
    label(mca.left.thickness.total)  <- 'Left MCA total wall thickness, mm'
    label(vb.thickness.total)        <- 'VB total wall thickness, mm'
    label(vwi.ica.id)                <- 'Mean ICA inner diameter, mm'
    label(vwi.aca.id)                <- 'Mean ACA inner diameter, mm'
    label(vwi.mca.id)                <- 'Mean MCA inner diameter, mm'
  })
  

    # 15 Dec 2016, LS:
    #   I'm commenting this out because the bHold versions of
    #     the variables do not need labels.
    #     We get rid of the bHold columns eventually---
    #       we just copy/paste some of the values
  #bHoldnames <- names(dat)[grep("^bHold\\.", names(dat))]
  ##bHoldnames <- bHoldnames[c(1:882,885:1237)]
  #regnames <- gsub("^bHold\\.", "", bHoldnames)
  #for(i in seq_along(bHoldnames)){
  #  label(dat[, bHoldnames[i]]) <- paste0("Breath Hold: ",
  #                                        label(dat[, regnames[i]]))}
  
  dat
}
