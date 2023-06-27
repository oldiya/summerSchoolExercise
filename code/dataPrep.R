#
# Title: data preparation
# 

# Load variable key explanation

    Raw_data_structure_CZ_JH1 <- readxl::read_excel("data/CZplots/Raw_data_structure_CZ_JH1.xlsx")

# Forest data ----

    # Tree information data 
    standtrees_unit_CZ <- readr::read_csv("data/CZplots/standtrees_unit_CZ.csv")
    length(unique(standtrees_unit_CZ$keyID)) # 100 plots 

    #Plot level data 
    plotdescdata_CZ <- readr::read_csv("data/CZplots/plotdescdata_CZ.csv")
    length(unique(plotdescdata_CZ$keyID)) # 106
    
    # soil data 
    datasoil_CZ <- readRDS("data/CZplots/datasoil_CZ.rds")
    length(unique(datasoil_CZ$keyID)) #106 
    
    # Estimations on species richness
    speciesrichnessdata_95_CZ <- readr::read_csv("data/CZplots/speciesrichnessdata_95_CZ.csv")
    length(unique(speciesrichnessdata_95_CZ$keyID))
    
 # Question 1 why tree level data has lees plots and plot level data and species are observed in the 106 plots   ------  
 
    # Species proportion per plot ----
    
    standtrees_unit_CZ_live <- standtrees_unit_CZ [standtrees_unit_CZ$alive == 1,]
    
    treePropSpp <- standtrees_unit_CZ_live |> 
                    dplyr::group_by(keyID, treesp) |> 
                      dplyr::summarise(volSppha = sum(vol_unit_ha))
    
    treePropTot <- standtrees_unit_CZ_live |> 
                      dplyr::group_by(keyID) |> 
                          dplyr::summarise(volAllha = sum(vol_unit_ha))
    
    
    treePropSpp <- merge(treePropSpp, treePropTot, by = "keyID")
    treePropSpp$propSpp <-  round(treePropSpp$volSppha * 100 / treePropSpp$volAllha, 0)
    
    treePropSpp <- treePropSpp[, c("keyID", "treesp", "propSpp")]
    treeSppProop <- treePropSpp |>
                      tidyr::pivot_wider(names_from = treesp, 
                                         values_from = propSpp)
    
    colnames(treeSppProop) <- c( "keyID", "A.pseudoplatanus", "F.sylvatica",
                                 "L.decidua", "Q.robur", "S.aucuparia", 
                                 "B.pendula", "P.abies", "P.sylvestris", 
                                 "F.excelsior",  "A.alba", "A.platanoides",  
                                 "T.cordata", "S.racemosa",   "U.glabra",
                                 "S.nigra", "P.alba", "U.minor", "S.caprea", 
                                 "C.betulus",    "P.nigra", "Q.petraea", 
                                 "S.torminalis", "A.campestre", "P.strobus", 
                                 "Q.rubra") 
    
    # IF we have an NA value, transform this into 0
    treeSppProop[is.na(treeSppProop)] <- 0
    
    
    # Species diversity per plot based on taxon level
    
    sppRichness <- speciesrichnessdata_95_CZ[, c("keyID", "taxon", "scaled_richness")]
    sppRichness <- sppRichness |>
      tidyr::pivot_wider(names_from = taxon, 
                         values_from = scaled_richness)
    
    colnames(sppRichness) <- c("keyID", "Tracheophyta_rich", "Birds_rich", 
                               "Bryophytes_rich",  "Fungi_rich", "Lichens_rich",
                               "Beetles_rich")
    
    # Gini index per plot ----
    # Structure index, calculated by diameter of the trees. Higher values 
    # indicate more structural heterogeneity; lower values indicate 
    # more  homogeneous stands
    # 
    ## [0-1] Higher values indicate more structural heterogeneity,
    ##       lower values indicate more homogeneous stands
    GiniCoefdbh <- data.frame(cbind(aggregate((standtrees_unit_CZ_live$treedb) ~ keyID,
                                               ineq::Gini, data = standtrees_unit_CZ_live)))
    
    colnames(GiniCoefdbh) <- c("keyID", "GiniDBH")
    
    # Shannon index tree species per plot ----
    
    VolTreesSpp <- data.frame(cbind(aggregate((standtrees_unit_CZ_live$vol_unit_ha) ~ species + keyID,
                                             sum, data = standtrees_unit_CZ_live)))
    
    colnames(VolTreesSpp) <- c("TreeSppS", "keyID","Vol")
    
    VolALLTrees <- data.frame(cbind(aggregate((standtrees_unit_CZ_live$vol_unit_ha) ~ keyID,
                                             sum, data = standtrees_unit_CZ_live)))
    
    colnames(VolALLTrees) <- c("keyID", "VolTot")
    
    ShannonIndexTable <- merge(VolTreesSpp, VolALLTrees, 
                               by = c("keyID"), all.x = T)
    
    ShannonIndexTable$giDIVG <- ShannonIndexTable$Vol/ShannonIndexTable$VolTot
    ShannonIndexTable$lngiDIVG <- log(ShannonIndexTable$Vol/ShannonIndexTable$VolTot)
    ShannonIndexTable$mult <- round(ShannonIndexTable$giDIVG * ShannonIndexTable$lngiDIVG,2)
    
    ShannonIndex <- data.frame(cbind(aggregate( mult ~ keyID,
                                               sum, data = ShannonIndexTable)))
    
    ShannonIndex$mult <-  (-1)*(ShannonIndex$mult)
    colnames(ShannonIndex) <- c("keyID", "ShannonIndexTreeSpp")
  
    
    # Dead volumes 
    standtrees_unit_CZ_dead <- standtrees_unit_CZ[standtrees_unit_CZ$alive == 0,]
    deadtrees <- standtrees_unit_CZ_dead |> 
                      dplyr::group_by(keyID) |> 
                      dplyr::summarise(volDeadha = sum(vol_unit_ha))
    
    # Question 2 deadwood is only for the measured year? or counts anything available standing or ground-----
  

# Species diversity observations ----

    #Overall Species richness values estimated from the data 
    speciesrichnessdata_95_CZ <- readr::read_csv("data/CZplots/speciesrichnessdata_95_CZ.csv")
    length(unique(speciesrichnessdata_95_CZ$keyID)) #106
    
    # Tracheophyta
    dataVP_CZ <- readr::read_csv("data/CZplots/dataVP_CZ.csv")
    length(unique(dataVP_CZ$keyID)) #106
    length(unique(dataVP_CZ$genspe)) #348
    ggplot2::ggplot(dataVP_CZ, ggplot2::aes(x=genspe)) +
        ggplot2::geom_bar() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
    Tracheophyta <- as.data.frame(table(dataVP_CZ$genspe))    
    Tracheophyta <- Tracheophyta[order(-Tracheophyta$Freq), ]                                                                                                      
    
    # Saproxylic Beetles
    dataSB_CZ <- readr::read_csv("data/CZplots/dataSB_CZ.csv")
    length(unique( dataSB_CZ$keyID)) #106
    length(unique(dataSB_CZ$genspe)) #171
    ggplot2::ggplot(dataSB_CZ, ggplot2::aes(x=genspe)) +
        ggplot2::geom_bar() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
    Beetles <- as.data.frame(table(dataSB_CZ$genspe))    
    Beetles <- Beetles[order(-Beetles$Freq), ]
   
    
    # Lichens
    dataLI_CZ <- readr::read_csv("data/CZplots/dataLI_CZ.csv")
    length(unique(dataLI_CZ$keyID)) #106
    length(unique(dataLI_CZ$genspe)) #119
    ggplot2::ggplot(dataLI_CZ, ggplot2::aes(x=genspe)) +
        ggplot2::geom_bar() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
    Lichens <- as.data.frame(table(dataLI_CZ$genspe))    
    Lichens <-  Lichens[order(-Lichens$Freq), ]
    
    # Basidiomycota / Fungus
    dataFU_CZ <- readr::read_csv("data/CZplots/dataFU_CZ.csv")
    length(unique(dataFU_CZ$keyID)) #106
    length(unique(dataFU_CZ$genspe)) #436
    ggplot2::ggplot(dataFU_CZ, ggplot2::aes(x=genspe)) +
        ggplot2::geom_bar() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
    
    dataFU_CZ$fungusObs <- 1
    fungus <- data.frame(cbind(aggregate(fungusObs ~ keyID,
                                                sum, data = dataFU_CZ)))
    fungusFreq <- as.data.frame(table(dataFU_CZ$genspe))    
    fungusFreq <- fungusFreq[order(-fungusFreq$Freq), ]
    
    # Bryophita
    dataBRYO_CZ <- readr::read_csv("data/CZplots/dataBRYO_CZ.csv")
    length(unique(dataBRYO_CZ$keyID)) #95
    length(unique(dataBRYO_CZ$genspe)) #21
    ggplot2::ggplot(dataBRYO_CZ, ggplot2::aes(x=genspe)) +
        ggplot2::geom_bar() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
    bryophitaFreq <- as.data.frame(table(dataBRYO_CZ$genspe))    
    bryophitaFreq <- bryophitaFreq[order(-bryophitaFreq$Freq), ]
    
    bryophitaPlot <- as.data.frame(table(dataBRYO_CZ$keyID))
    colnames(bryophitaPlot) <- c("keyID", "bryophitaNumObs")
    
    # Birds / Aves
    dataBIRD_CZ <- readr::read_csv("data/CZplots/dataBIRD_CZ.csv")
    length(unique(dataBIRD_CZ$keyID)) #106
    length(unique( dataBIRD_CZ$genspe)) #67
    ggplot2::ggplot(dataBIRD_CZ, ggplot2::aes(x=genspe)) +
        ggplot2::geom_bar() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
    birdFreq <- as.data.frame(table(dataBIRD_CZ$genspe))    
    birdFreq <- bryophitaFreq[order(-birdFreq$Freq), ]
    
    birdPlot <- as.data.frame(table(dataBIRD_CZ$keyID))
    colnames(birdPlot ) <- c("keyID", "birdNumObs")
    
    # Select dendrocopos major 
    
    dendrocoposTb <-  dataBIRD_CZ [dataBIRD_CZ$genspe == "Dendrocopos major", ]
    dendrocoposTb$dendrocoposMajor <- 1
    
    # Select phoenicurus
    certhiaTb <-  dataBIRD_CZ [dataBIRD_CZ$genspe == "Certhia familiaris", ]
    certhiaTb$certhia <- 1
    
    # Create the summary data------
    
    modelData <- plotdescdata_CZ[, c("keyID", "lon", "lat", "silsl2",
                                     "vertstr", "yeasam")]
    
    colnames(modelData) <- c("keyID", "longitud", "latitude",
                             "forestManagementType", "forestStructure",
                             "yearLastManagement")
    # Question 3 is yeasam year of the last management and from where this data was measured. Where can I find the variable meaning fro plot data -----
    
    # Add slope
    
    modelData <- merge(modelData, 
                       datasoil_CZ[, c("slope", "keyID")],
                       by = "keyID")
    
    # Add species proportion 
    modelData <- merge(modelData, treeSppProop,
                       by = "keyID")
    
    # Add a proxy for density 
    modelData <- merge(modelData, treePropTot,  by = "keyID")
    
    #Add Gini coefficient by dbh as a proxy for forest structural diversity
    modelData <- merge(modelData, GiniCoefdbh,  by = "keyID")
    
    # Add shannon of tree species 
    modelData <- merge(modelData, ShannonIndex,  by = "keyID")
    
    # Add species richness from other taxones
    modelData <- merge(modelData, sppRichness,  by = "keyID")    
    
    
    # Add presence / absence Dendrocopos major
    modelData <- merge(modelData,   
                       dendrocoposTb[, c("keyID", "dendrocoposMajor")], 
                       by = "keyID", all.x = TRUE)  
    modelData$dendrocoposMajor[is.na(modelData$dendrocoposMajor)] <- 0
    
    # Add presence / absence Certhia familiaris
    modelData <- merge(modelData,   
                       certhiaTb[, c("keyID", "certhia")], 
                       by = "keyID", all.x = TRUE)  
    modelData$certhia[is.na(modelData$certhia)] <- 0
    
    
    # Add number of observed  Bryophita per plot
    modelData <- merge(modelData, bryophitaPlot, 
                       by = "keyID", all.x = TRUE) 
    modelData$bryophitaNumObs[is.na(modelData$bryophitaNumObs)] <- 0
    
    # Add number of observed bird species 
    modelData <- merge(modelData, birdPlot, 
                       by = "keyID", all.x = TRUE) 
    modelData$bryophitaNumObs[is.na(modelData$birdNumObs)] <- 0
    
      
    
    # Change IDs of plots 
    modelData$PlotID <- 1:nrow(modelData)
    modelData$keyID <- NULL
    modelData$yearLastManagement <- NULL
    
    write.csv(modelData, "data/observations.csv")
      
      
    
    
    
