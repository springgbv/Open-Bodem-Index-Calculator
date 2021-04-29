#' Calculate indicator for plant parasitic nematodes
#'
#' This function calculates the indicator for the presence of plant parasitic nematodes. All nematodes present in a sample are used.
#' A subset of nematodes is weighted in the set regardless of their presence.
#' 
#' @param A_NEMA (data.table) Long data table with the counted nematodes of a parcel.
#' 
#' @import data.table
#' 
#' @export
ind_nematodes_list <- function(A_NEMA){
  nema.obic <- as.data.table(OBIC::nema.obic)
  
  geel = rood = species = standaard = b = v = count = nem_score = NULL
  
  checkmate::assert_data_table(A_NEMA)
  checkmate::assert_numeric(A_NEMA[,count])
  checkmate::assert_subset(x = A_NEMA[,species],choices = nema.obic[,species])
  
  # merge dd and nema.obic and remove non standard non counted nematodes from dd
  dd <- merge.data.table(nema.obic, A_NEMA, by = 'species', all.x = TRUE)
  dd <- dd[standaard == TRUE|!is.na(count)]
  
  # Check if all standard nematodes are present
  if(checkmate::anyMissing(dd[,count])){
    errorCondition('at least one of the "standard" nematodes seems to be a missing value, its assumed this nematode is counted and is equal to 0.')
  } 
  # Calculate score for each individual nematode species
  dd[,nem_score := OBIC::evaluate_logistic(dd[,count], b = dd[,b], x0 = dd[,geel], v = dd[,v], increasing = FALSE)]
  # Set scores where count = 0 to 1
  dd[count == 0, nem_score:=1]
  
  value <- mean(dd[,nem_score])
  return(value)
} 

#' Calculate indicator for plant parasitic nematodes
#'
#' This function calculates the indicator for the presence of plant parasitic nematodes. 
#' If input values are not given, the number is assumed to be zero.
#' 
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param A_RLN_PR_TOT (numeric) Number of pratylenchus spp. (n / 100g)
#' @param A_RLN_PR_CREN (numeric) Number of pratylenchus crenatus (n / 100g)
#' @param A_RLN_PR_NEG (numeric) Number of pratylenchus neglectus (n / 100g)
#' @param A_RLN_PR_PEN (numeric) Number of pratylenchus penetrans (n / 100g)
#' @param A_RLN_PR_PRA (numeric) Number of pratylenchus pratensis (n / 100g)
#' @param A_RLN_PR_THO (numeric) Number of pratylenchus thornei (n / 100g)
#' @param A_RLN_PR_FLA (numeric) Number of pratylenchus flakkensis (n / 100g)
#' @param A_RLN_PR_FAL (numeric) Number of pratylenchus fallax (n / 100g)
#' @param A_RLN_PR_PIN (numeric) Number of pratylenchus pinguicaudatus (n / 100g)
#' @param A_RLN_PR_PSE (numeric) Number of pratylenchus pseudopratensis (n / 100g)
#' @param A_RLN_PR_VUL (numeric) Number of pratylenchus vulnus (n / 100g)
#' @param A_RLN_PR_DUN (numeric) Number of pratylenchus dunensis (n / 100g)
#' @param A_RLN_PR_ZEA (numeric) Number of pratylenchus zeae (n / 100g)
#' @param A_RKN_ME_TOT (numeric) Number of meloidogyne spp. (n / 100g)
#' @param A_RKN_ME_HAP (numeric) Number of meloidogyne hapla (n / 100g)
#' @param A_RKN_ME_CHIFAL (numeric) Number of meloidogyne chitwoodi/fallax (n / 100g)
#' @param A_RKN_ME_CHI (numeric) Number of meloidogyne chitwoodi (n / 100g)
#' @param A_RKN_ME_NAA (numeric) Number of meloidogyne naasi (n / 100g)
#' @param A_RKN_ME_FAL (numeric) Number of meloidogyne fallax (n / 100g)
#' @param A_RKN_ME_MIN (numeric) Number of meloidogyne minor (n / 100g)
#' @param A_RKN_ME_INC (numeric) Number of meloidogyne incognita (n / 100g)
#' @param A_RKN_ME_JAV (numeric) Number of meloidogyne javanica (n / 100g)
#' @param A_RKN_ME_ART (numeric) Number of meloidogyne artiellia (n / 100g)
#' @param A_RKN_ME_ARE (numeric) Number of meloidogyne arenaria (n / 100g)
#' @param A_RKN_ME_ARD (numeric) Number of meloidogyne ardenensis (n / 100g)
#' @param A_DSN_TR_TOT (numeric) Number of trichodoridae spp. (n / 100g)
#' @param A_DSN_TR_SIM (numeric) Number of trichodorus similis (n / 100g)
#' @param A_DSN_TR_PRI (numeric) Number of trichodorus primitivus (n / 100g)
#' @param A_DSN_TR_VIR (numeric) Number of trichodorus viruliferus (n / 100g)
#' @param A_DSN_TR_SPA (numeric) Number of trichodorus sparsus (n / 100g)
#' @param A_DSN_TR_CYL (numeric) Number of trichodorus cylindricus (n / 100g)
#' @param A_DSN_TR_HOO (numeric) Number of trichodorus hooperi (n / 100g)
#' @param A_DSN_PA_TER (numeric) Number of paratrichodorus teres (n / 100g)
#' @param A_DSN_PA_PAC (numeric) Number of paratrichodorus pachydermus (n / 100g)
#' @param A_DSN_PA_ANE (numeric) Number of paratrichodorus anemones (n / 100g)
#' @param A_DSN_PA_NAN (numeric) Number of paratrichodorus nanus (n / 100g)
#' @param A_DSN_TY_TOT (numeric) Number of tylenchorhynchus spp. (n / 100g)
#' @param A_DSN_RO_TOT (numeric) Number of rotylenchus spp. (n / 100g)
#' @param A_DSN_XI_TOT (numeric) Number of xiphinema spp. (n / 100g)
#' @param A_DSN_LO_TOT (numeric) Number of longidorus spp. (n / 100g)
#' @param A_DSN_HEM_TOT (numeric) Number of hemicycliophora spp. (n / 100g)
#' @param A_DSN_HEL_TOT (numeric) Number of helicotylenchus spp. (n / 100g)
#' @param A_SN_DI_TOT (numeric) Number of ditylenchus spp. (n / 100g)
#' @param A_SN_DI_DIP (numeric) Number of ditylenchus dipsaci (n / 100g)
#' @param A_SN_DI_DES (numeric) Number of ditylenchus destructor (n / 100g)
#' @param A_OPN_PA_TOT (numeric) Number of paratylenchus spp. (n / 100g)
#' @param A_OPN_PA_BUK (numeric) Number of paratylenchus bukowinensis (n / 100g)
#' @param A_OPN_CY_TOT (numeric) Number of cysteaaltjes (n / 100g)
#' @param A_OPN_AP_TOT (numeric) Number of aphelenchoides spp. (n / 100g)
#' @param A_OPN_AP_FRA (numeric) Number of aphelenchoides fragariae (n / 100g)
#' @param A_OPN_AP_RIT (numeric) Number of aphelenchoides ritzemabosi (n / 100g)
#' @param A_OPN_AP_SUB (numeric) Number of aphelenchoides subtenuis (n / 100g)
#' @param A_OPN_CR_TOT (numeric) Number of criconematidae spp. (n / 100g)
#' @param A_OPN_SU_TOT (numeric) Number of subanguina spp. (n / 100g)
#' @param A_NPN_SA_TOT (numeric) Number of saprofage en overige (n / 100g)
#' 
#' @import data.table
#' 
#' @export
ind_nematodes <- function(B_LU_BRP = B_LU_BRP,
                          A_RLN_PR_TOT=0, A_RLN_PR_CREN=0, A_RLN_PR_NEG=0, A_RLN_PR_PEN=0, A_RLN_PR_PRA=0, A_RLN_PR_THO=0, A_RLN_PR_FLA=0,    
                          A_RLN_PR_FAL=0, A_RLN_PR_PIN=0, A_RLN_PR_PSE=0, A_RLN_PR_VUL=0, A_RLN_PR_DUN=0, A_RLN_PR_ZEA=0, A_RKN_ME_TOT=0,   
                          A_RKN_ME_HAP=0, A_RKN_ME_CHIFAL=0, A_RKN_ME_CHI=0, A_RKN_ME_NAA=0, A_RKN_ME_FAL=0, A_RKN_ME_MIN=0, A_RKN_ME_INC=0,    
                          A_RKN_ME_JAV=0, A_RKN_ME_ART=0, A_RKN_ME_ARE=0, A_RKN_ME_ARD=0, A_DSN_TR_TOT=0, A_DSN_TR_SIM=0, A_DSN_TR_PRI=0,    
                          A_DSN_TR_VIR=0, A_DSN_TR_SPA=0, A_DSN_TR_CYL=0, A_DSN_TR_HOO=0, A_DSN_PA_TER=0, A_DSN_PA_PAC=0, A_DSN_PA_ANE=0,    
                          A_DSN_PA_NAN=0, A_DSN_TY_TOT=0, A_DSN_RO_TOT=0, A_DSN_XI_TOT=0, A_DSN_LO_TOT=0, A_DSN_HEM_TOT=0, A_DSN_HEL_TOT=0,   
                          A_SN_DI_TOT=0, A_SN_DI_DIP=0, A_SN_DI_DES=0, A_OPN_PA_TOT=0, A_OPN_PA_BUK=0, A_OPN_CY_TOT=0, A_OPN_AP_TOT=0,    
                          A_OPN_AP_FRA=0, A_OPN_AP_RIT=0, A_OPN_AP_SUB=0, A_OPN_CR_TOT=0, A_OPN_SU_TOT = 0,A_NPN_SA_TOT = 0){
  
  # add visual bindings
  b = element = geel = id = nem_b = nem_v = nem_x = number = standaard = v = value = . = NULL
  
  # indicator scoring values per nematode type
  nema.obic <- as.data.table(OBIC::nema.obic)
  
  # combine inputs for root lesion nematodes
  dt.rln <- data.table(A_RLN_PR_TOT = A_RLN_PR_TOT,A_RLN_PR_CREN = A_RLN_PR_CREN,A_RLN_PR_NEG = A_RLN_PR_NEG,
                     A_RLN_PR_PEN = A_RLN_PR_PEN,A_RLN_PR_PRA = A_RLN_PR_PRA,A_RLN_PR_THO = A_RLN_PR_THO,
                     A_RLN_PR_FLA = A_RLN_PR_FLA,A_RLN_PR_FAL = A_RLN_PR_FAL,A_RLN_PR_PIN = A_RLN_PR_PIN,
                     A_RLN_PR_PSE = A_RLN_PR_PSE,A_RLN_PR_VUL = A_RLN_PR_VUL,A_RLN_PR_DUN = A_RLN_PR_DUN,
                     A_RLN_PR_ZEA = A_RLN_PR_ZEA)
  
  # combine inputs for root knot nematodes
  dt.rkn <- data.table(A_RKN_ME_TOT = A_RKN_ME_TOT,A_RKN_ME_HAP = A_RKN_ME_HAP,A_RKN_ME_CHIFAL = A_RKN_ME_CHIFAL,
                     A_RKN_ME_CHI = A_RKN_ME_CHI,A_RKN_ME_NAA = A_RKN_ME_NAA,A_RKN_ME_FAL = A_RKN_ME_FAL,
                     A_RKN_ME_MIN = A_RKN_ME_MIN,A_RKN_ME_INC = A_RKN_ME_INC,A_RKN_ME_JAV = A_RKN_ME_JAV,
                     A_RKN_ME_ART = A_RKN_ME_ART,A_RKN_ME_ARE = A_RKN_ME_ARE,A_RKN_ME_ARD = A_RKN_ME_ARD)
  
  # combine inputs for dwelling stunt nematodes
  dt.dsn <- data.table(A_DSN_TR_TOT = A_DSN_TR_TOT,A_DSN_TR_SIM = A_DSN_TR_SIM,A_DSN_TR_PRI = A_DSN_TR_PRI,
                     A_DSN_TR_VIR = A_DSN_TR_VIR,A_DSN_TR_SPA = A_DSN_TR_SPA,A_DSN_TR_CYL = A_DSN_TR_CYL,
                     A_DSN_TR_HOO = A_DSN_TR_HOO,A_DSN_PA_TER = A_DSN_PA_TER,A_DSN_PA_PAC = A_DSN_PA_PAC,
                     A_DSN_PA_ANE = A_DSN_PA_ANE,A_DSN_PA_NAN = A_DSN_PA_NAN,A_DSN_TY_TOT = A_DSN_TY_TOT,
                     A_DSN_RO_TOT = A_DSN_RO_TOT,A_DSN_XI_TOT = A_DSN_XI_TOT,A_DSN_LO_TOT = A_DSN_LO_TOT,
                     A_DSN_HEM_TOT = A_DSN_HEM_TOT,A_DSN_HEL_TOT = A_DSN_HEL_TOT)
  
  # combine inputs for stem nematodes
  dt.sn <- data.table(A_SN_DI_TOT = A_SN_DI_TOT,A_SN_DI_DIP = A_SN_DI_DIP, A_SN_DI_DES = A_SN_DI_DES)
  
  # combine inputs for other and non-plant pathogenic nematodes
  dt.opn <- data.table(A_OPN_PA_TOT = A_OPN_PA_TOT,A_OPN_PA_BUK = A_OPN_PA_BUK,A_OPN_CY_TOT = A_OPN_CY_TOT,
                     A_OPN_AP_TOT = A_OPN_AP_TOT,A_OPN_AP_FRA = A_OPN_AP_FRA,A_OPN_AP_RIT = A_OPN_AP_RIT,
                     A_OPN_AP_SUB = A_OPN_AP_SUB,A_OPN_CR_TOT = A_OPN_CR_TOT,A_OPN_SU_TOT = A_OPN_SU_TOT,
                     A_NPN_SA_TOT = A_NPN_SA_TOT)
  
  # length of input variables
  arg.length <- max(length(B_LU_BRP),nrow(dt.rln),nrow(dt.rkn),nrow(dt.dsn),nrow(dt.sn),nrow(dt.opn))
  
  # check B_LU_BRP
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  
  # combine all inputs into one
  dt.all <- data.table(id = 1:arg.length,
                       B_LU_BRP = B_LU_BRP,
                       dt.rln, dt.rkn, dt.dsn, dt.sn, dt.opn)
  
  
  # melt all nematode input variables from column to row
  dt.melt <- melt(dt.all,id.vars = 'id', variable.name = 'element',value.name = 'number')
  
  # merge with parameters logistic function
  dt.melt <- merge(dt.melt,nema.obic[,.(element,nem_x = geel, nem_b = b,nem_v = v,standaard)], by = c('element'))

  # replace missing values with zero
  dt.melt[is.na(number), number := 0]
  
  # estimate indicator value
  dt.melt[, value := evaluate_logistic(x = number, b = nem_b, x0 = nem_x, v = nem_v, increasing = FALSE),by = c('element')]

  # set value of nematodes where number = 0 to 1
  dt.melt[number == 0, value:= 1]
  
  # Select relevant nematodes given the crops in the rotation
  
  # round indicator value
  dt.melt[, value := round(pmin(value),3)]
  
  # select the lowest score per field being the limiting value for soil quality
  out <- dt.melt[order(value),.SD[1L],by = id]
  
  # ensure correct order
  setorder(out,id)
  
  # extract relevant score
  out <- out[,value]
  
  # return out
  return(out)
} 
