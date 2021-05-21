#' Calculate the Open Bodem Index score for one field
#' 
#' This functions wraps the functions of the OBIC into one main function to calculate the score for Open Bodem Index (OBI) for a single field.
#' 
#' @param B_LU_BRP (numeric) a series with crop codes given the crop rotation plan (source: the BRP) 
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006).
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_GWL_GLG (numeric) The lowest groundwater level averaged over the most dry periods in 8 years in cm below ground level
#' @param B_GWL_GHG (numeric) The highest groundwater level averaged over the most wet periods in 8 years in cm below ground level
#' @param B_Z_TWO  (numeric) The distance between ground level and groundwater level at which the groundwater can supply the soil surface with 2mm water per day (in cm)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CACO3_IF (numeric) The carbonate content of the soil (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param A_CN_FR (numeric) The carbon to nitrogen ratio (-)
#' @param A_COM_FR (numeric) The carbon fraction of soil organic matter (\%)
#' @param A_S_RT (numeric) The total Sulpher content of the soil (in mg S per kg)
#' @param A_N_PMN (numeric) The potentially mineralizable N pool (mg N / kg soil)
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_CC (numeric) The plant available P content, extracted with 0.01M CaCl2 (mg / kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / 100 ml soil)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analysed via Cobalt-hexamine extraction
#' @param A_CA_CO_PO (numeric) The The occupation of the CEC with Ca (\%)
#' @param A_MG_CO_PO (numeric) The The occupation of the CEC with Mg (\%)
#' @param A_K_CO_PO (numeric) The occupation of the CEC with K (\%)
#' @param A_K_CC (numeric) The plant available K content, extracted with 0.01M CaCl2 (mg / kg)
#' @param A_MG_CC (numeric) The plant available Mg content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_MN_CC (numeric) The plant available Mn content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_ZN_CC (numeric) The plant available Zn content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_CU_CC (numeric) The plant available Cu content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_EW_BCS (numeric) The presence of earth worms (optional, score 0-1-2)
#' @param A_SC_BCS (numeric) The presence of compaction of subsoil (optional, score 0-1-2)
#' @param A_GS_BCS (numeric) The presence of waterlogged conditions, gley spots (optional, score 0-1-2)
#' @param A_P_BCS (numeric) The presence / occurrence of water puddles on the land, ponding (optional, score 0-1-2)
#' @param A_C_BCS (numeric) The presence of visible cracks in the top layer (optional, score 0-1-2)
#' @param A_RT_BCS (numeric) The presence of visible tracks / rutting or trampling on the land (optional, score 0-1-2)
#' @param A_RD_BCS (integer) The rooting depth (optional, score 0-1-2)
#' @param A_SS_BCS (integer) The soil structure (optional, score 0-1-2)
#' @param A_CC_BCS (integer) The crop cover on the surface (optional, score 0-1-2)
#' @param M_COMPOST (numeric) The frequency that compost is applied (optional, every x years)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#' @param M_NONBARE (boolean) A soil measure. Is parcel for 80 percent of the year cultivated and 'green' (optional, option: yes or no)
#' @param M_EARLYCROP (boolean) A soil measure. Use of early crop varieties to avoid late harvesting (optional, option: yes or no)
#' @param M_SLEEPHOSE (boolean) A soil measure. Is sleephose used for slurry application (optional, option: yes or no)
#' @param M_DRAIN (boolean) A soil measure. Are under water drains installed in peaty soils (optional, option: yes or no)
#' @param M_DITCH (boolean) A soil measure. Are ditched maintained carefully and slib applied on the land (optional, option: yes or no)
#' @param M_UNDERSEED (boolean) A soil measure. Is grass used as second crop in between maize rows (optional, option: yes or no) 
#' @param M_LIME (boolean) measure. Has field been limed in last three years (option: yes or no)
#' @param M_NONINVTILL (boolean) measure. Non inversion tillage (option: yes or no)
#' @param M_SSPM (boolean) measure. Soil Structure Protection Measures, such as fixed driving lines, low pressure tires, and light weighted machinery (option: yes or no)
#' @param M_SOLIDMANURE (boolean) measure. Use of solid manure (option: yes or no)
#' @param M_STRAWRESIDUE (boolean) measure. Application of straw residues (option: yes or no)
#' @param M_MECHWEEDS (boolean) measure. Use of mechanical weed protection (option: yes or no)
#' @param M_PESTICIDES_DST (boolean) measure. Use of DST for pesticides (option: yes or no)
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
#' @param A_NEMA (data.table) Long data table with the counted nematodes of a parcel can be used instead of separate nematode entries
#' @param ID (character) A field id
#' @param output (character) An optional argument to select output: obic_score, scores, indicators, recommendations, or all. (default = all)
#' 
#' @import data.table
#' 
#' @export
obic_field <- function(B_SOILTYPE_AGR,B_GWL_CLASS,B_SC_WENR,B_HELP_WENR,B_AER_CBS,
                       B_GWL_GLG,B_GWL_GHG,B_Z_TWO,
                       B_LU_BRP, 
                       A_SOM_LOI, A_SAND_MI, A_SILT_MI, A_CLAY_MI,A_PH_CC,A_CACO3_IF,
                       A_N_RT,A_CN_FR,A_COM_FR, A_S_RT,A_N_PMN,
                       A_P_AL, A_P_CC, A_P_WA,
                       A_CEC_CO,A_CA_CO_PO, A_MG_CO_PO, A_K_CO_PO,
                       A_K_CC, A_MG_CC, A_MN_CC, A_ZN_CC, A_CU_CC,
                       A_C_BCS = NA, A_CC_BCS = NA,A_GS_BCS = NA,A_P_BCS = NA,A_RD_BCS = NA,
                       A_EW_BCS = NA,A_SS_BCS = NA,A_RT_BCS = NA,A_SC_BCS = NA,
                       M_COMPOST  = NA_real_,M_GREEN = NA, M_NONBARE = NA, M_EARLYCROP = NA, 
                       M_SLEEPHOSE = NA,M_DRAIN = NA,M_DITCH = NA,M_UNDERSEED = NA,
                       M_LIME = NA, M_NONINVTILL = NA, M_SSPM = NA, M_SOLIDMANURE = NA,
                       M_STRAWRESIDUE = NA,M_MECHWEEDS = NA,M_PESTICIDES_DST = NA,
                       A_RLN_PR_TOT=0, A_RLN_PR_CREN=0, A_RLN_PR_NEG=0, A_RLN_PR_PEN=0, A_RLN_PR_PRA=0, A_RLN_PR_THO=0, A_RLN_PR_FLA=0,    
                       A_RLN_PR_FAL=0, A_RLN_PR_PIN=0, A_RLN_PR_PSE=0, A_RLN_PR_VUL=0, A_RLN_PR_DUN=0, A_RLN_PR_ZEA=0, A_RKN_ME_TOT=0,   
                       A_RKN_ME_HAP=0, A_RKN_ME_CHIFAL=0, A_RKN_ME_CHI=0, A_RKN_ME_NAA=0, A_RKN_ME_FAL=0, A_RKN_ME_MIN=0, A_RKN_ME_INC=0,    
                       A_RKN_ME_JAV=0, A_RKN_ME_ART=0, A_RKN_ME_ARE=0, A_RKN_ME_ARD=0, A_DSN_TR_TOT=0, A_DSN_TR_SIM=0, A_DSN_TR_PRI=0,    
                       A_DSN_TR_VIR=0, A_DSN_TR_SPA=0, A_DSN_TR_CYL=0, A_DSN_TR_HOO=0, A_DSN_PA_TER=0, A_DSN_PA_PAC=0, A_DSN_PA_ANE=0,    
                       A_DSN_PA_NAN=0, A_DSN_TY_TOT=0, A_DSN_RO_TOT=0, A_DSN_XI_TOT=0, A_DSN_LO_TOT=0, A_DSN_HEM_TOT=0, A_DSN_HEL_TOT=0,   
                       A_SN_DI_TOT=0, A_SN_DI_DIP=0, A_SN_DI_DES=0, A_OPN_PA_TOT=0, A_OPN_PA_BUK=0, A_OPN_CY_TOT=0, A_OPN_AP_TOT=0,    
                       A_OPN_AP_FRA=0, A_OPN_AP_RIT=0, A_OPN_AP_SUB=0, A_OPN_CR_TOT=0, A_OPN_SU_TOT = 0,A_NPN_SA_TOT = 0, A_NEMA = NA,
                       ID = 1, output = 'all') {
  
  
  # define variables used within the function
  D_SE = D_CR = D_BDS = D_RD = D_OC = D_OS_BAL = D_GA = D_NL = D_K = D_PBI = NULL
  D_CP_STARCH = D_CP_POTATO = D_CP_SUGARBEET = D_CP_GRASS = D_CP_MAIS = D_CP_OTHER = D_CP_RUST = D_CP_RUSTDEEP = NULL
  D_NLV = D_PH_DELTA = D_MAN = D_SOM_BAL = D_WE = D_SLV = D_MG = D_CU = D_ZN = D_PMN = D_CEC = NULL
  D_AS =  D_BCS = D_WRI = D_WSI_DS = D_WSI_WS = D_NGW = D_NSW = D_WO = B_GWL_GLG = B_GWL_GHG = B_Z_TWO = NULL
  
  I_C_N = I_C_P = I_C_K = I_C_MG = I_C_S = I_C_PH = I_C_CEC = I_C_CU = I_C_ZN = I_P_WRI = I_BCS = NULL
  I_P_CR = I_P_SE = I_P_MS = I_P_BC = I_P_DU = I_P_CO = D_P_CO = I_B_DI = I_B_SF = I_B_SB = I_B_NEM = I_M = NULL
  I_P_DS = I_P_WS = I_P_CEC = D_P_CEC= I_P_WO = I_E_NGW = I_E_NSW = NULL
  I_M_GREEN = I_M_COMPOST = I_M_NONBARE = I_M_EARLYCROP = I_M_SLEEPHOSE = I_M_DRAIN = I_M_DITCH = I_M_UNDERSEED = NULL
  I_M_LIME = I_M_NONINVTILL = I_M_SSPM = I_M_SOLIDMANURE = I_M_STRAWRESIDUE = I_M_MECHWEEDS = I_M_PESTICIDES_DST = NULL
  crop_category = crops.obic = leaching_to = NULL
  
  crop_code = weight.obic = weight = score.cf = . = out.ind = NULL
  weight_peat = weight_nonpeat = NULL
  indicator = ind.n = value = value.w = value.cf = year.cf = value.group = value.year = NULL
  var = cf = ncat = id = NULL
  
  # combine input into one data.table
  # field properties start with B, soil analysis with A, Soil Visual Assessment ends with BCS and management starts with M
  dt <- data.table(ID = ID,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_AER_CBS = B_AER_CBS,
                   B_GWL_CLASS =  B_GWL_CLASS,
                   B_SC_WENR = B_SC_WENR,
                   B_HELP_WENR = B_HELP_WENR,
                   B_GWL_GLG = B_GWL_GLG,
                   B_GWL_GHG = B_GWL_GLG,
                   B_Z_TWO = B_Z_TWO,
                   A_SOM_LOI = A_SOM_LOI, 
                   A_SAND_MI = A_SAND_MI, 
                   A_SILT_MI = A_SILT_MI, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   A_CACO3_IF = A_CACO3_IF,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_CN_FR,
                   A_COM_FR = A_COM_FR, 
                   A_S_RT = A_S_RT,
                   A_N_PMN = A_N_PMN,
                   A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC, 
                   A_P_WA = A_P_WA,
                   A_CEC_CO = A_CEC_CO,
                   A_CA_CO_PO = A_CA_CO_PO, 
                   A_MG_CO_PO = A_MG_CO_PO, 
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_CC = A_K_CC, 
                   A_MG_CC = A_MG_CC, 
                   A_MN_CC = A_MN_CC, 
                   A_ZN_CC = A_ZN_CC, 
                   A_CU_CC = A_CU_CC,
                   A_C_BCS = A_C_BCS, 
                   A_CC_BCS = A_CC_BCS,
                   A_GS_BCS = A_GS_BCS,
                   A_P_BCS = A_P_BCS,
                   A_RD_BCS = A_RD_BCS,
                   A_EW_BCS = A_EW_BCS,
                   A_SS_BCS = A_SS_BCS,
                   A_RT_BCS = A_RT_BCS,
                   A_SC_BCS = A_SC_BCS,
                   M_NONBARE = M_NONBARE, 
                   M_EARLYCROP = M_EARLYCROP, 
                   M_SLEEPHOSE = M_SLEEPHOSE,
                   M_DRAIN = M_DRAIN,
                   M_DITCH = M_DITCH,
                   M_UNDERSEED = M_UNDERSEED,
                   M_COMPOST = M_COMPOST,
                   M_GREEN = M_GREEN,
                   M_LIME = M_LIME,
                   M_NONINVTILL = M_NONINVTILL,
                   M_SSPM = M_SSPM,
                   M_SOLIDMANURE = M_SOLIDMANURE,
                   M_STRAWRESIDUE = M_STRAWRESIDUE,
                   M_MECHWEEDS = M_MECHWEEDS,
                   M_PESTICIDES_DST = M_PESTICIDES_DST,
                   A_RLN_PR_TOT = A_RLN_PR_TOT,
                   A_RLN_PR_CREN = A_RLN_PR_CREN,
                   A_RLN_PR_NEG = A_RLN_PR_NEG,
                   A_RLN_PR_PEN = A_RLN_PR_PEN,
                   A_RLN_PR_PRA = A_RLN_PR_PRA,
                   A_RLN_PR_THO = A_RLN_PR_THO,
                   A_RLN_PR_FLA = A_RLN_PR_FLA,    
                   A_RLN_PR_FAL = A_RLN_PR_FAL,
                   A_RLN_PR_PIN = A_RLN_PR_PIN,
                   A_RLN_PR_PSE = A_RLN_PR_PSE,
                   A_RLN_PR_VUL = A_RLN_PR_VUL,
                   A_RLN_PR_DUN = A_RLN_PR_DUN,
                   A_RLN_PR_ZEA = A_RLN_PR_ZEA,
                   A_RKN_ME_TOT = A_RKN_ME_TOT,   
                   A_RKN_ME_HAP = A_RKN_ME_HAP,
                   A_RKN_ME_CHIFAL = A_RKN_ME_CHIFAL,
                   A_RKN_ME_CHI = A_RKN_ME_CHI,
                   A_RKN_ME_NAA = A_RKN_ME_NAA,
                   A_RKN_ME_FAL = A_RKN_ME_FAL,
                   A_RKN_ME_MIN = A_RKN_ME_MIN,
                   A_RKN_ME_INC = A_RKN_ME_INC,    
                   A_RKN_ME_JAV = A_RKN_ME_JAV,
                   A_RKN_ME_ART = A_RKN_ME_ART,
                   A_RKN_ME_ARE = A_RKN_ME_ARE,
                   A_RKN_ME_ARD = A_RKN_ME_ARD,
                   A_DSN_TR_TOT = A_DSN_TR_TOT,
                   A_DSN_TR_SIM = A_DSN_TR_SIM,
                   A_DSN_TR_PRI = A_DSN_TR_PRI,    
                   A_DSN_TR_VIR = A_DSN_TR_VIR,
                   A_DSN_TR_SPA = A_DSN_TR_SPA,
                   A_DSN_TR_CYL = A_DSN_TR_CYL,
                   A_DSN_TR_HOO = A_DSN_TR_HOO,
                   A_DSN_PA_TER = A_DSN_PA_TER,
                   A_DSN_PA_PAC = A_DSN_PA_PAC,
                   A_DSN_PA_ANE = A_DSN_PA_ANE,    
                   A_DSN_PA_NAN = A_DSN_PA_NAN,
                   A_DSN_TY_TOT = A_DSN_TY_TOT,
                   A_DSN_RO_TOT = A_DSN_RO_TOT,
                   A_DSN_XI_TOT = A_DSN_XI_TOT,
                   A_DSN_LO_TOT = A_DSN_LO_TOT,
                   A_DSN_HEM_TOT = A_DSN_HEM_TOT,
                   A_DSN_HEL_TOT = A_DSN_HEL_TOT,   
                   A_SN_DI_TOT = A_SN_DI_TOT,
                   A_SN_DI_DIP = A_SN_DI_DIP,
                   A_SN_DI_DES = A_SN_DI_DES,
                   A_OPN_PA_TOT = A_OPN_PA_TOT,
                   A_OPN_PA_BUK = A_OPN_PA_BUK,
                   A_OPN_CY_TOT = A_OPN_CY_TOT,
                   A_OPN_AP_TOT = A_OPN_AP_TOT,    
                   A_OPN_AP_FRA = A_OPN_AP_FRA,
                   A_OPN_AP_RIT = A_OPN_AP_RIT,
                   A_OPN_AP_SUB = A_OPN_AP_SUB,
                   A_OPN_CR_TOT = A_OPN_CR_TOT,
                   A_OPN_SU_TOT = A_OPN_SU_TOT,
                   A_NPN_SA_TOT = A_NPN_SA_TOT)
  
  # Copy I_NEMA table when present
  if(!is.na(I_NEMA)) {I_NEMA <- copy(I_NEMA)}
  
  # Merge dt with crops.obic
  dt <- merge(dt,OBIC::crops.obic[,list(crop_code,crop_category)], by.x = 'B_LU_BRP', by.y = 'crop_code') 
  
  # Step 1 Pre-processing ------------------
  
    # check format B_SC_WENR and B_GWL_CLASS
    dt[, B_SC_WENR := format_soilcompaction(B_SC_WENR)]
    dt[, B_GWL_CLASS := format_gwt(B_GWL_CLASS)]
  
    # check format B_AER_CBS
    dt[, B_AER_CBS := format_aer(B_AER_CBS)]
    
    # add management when input is missing
    cols <- c('M_GREEN', 'M_NONBARE', 'M_EARLYCROP','M_COMPOST','M_SLEEPHOSE','M_DRAIN','M_DITCH','M_UNDERSEED',
              'M_LIME', 'M_NONINVTILL', 'M_SSPM', 'M_SOLIDMANURE','M_STRAWRESIDUE','M_MECHWEEDS','M_PESTICIDES_DST')
    dt[, c(cols) := add_management(ID,B_LU_BRP, B_SOILTYPE_AGR,
                                   M_GREEN, M_NONBARE, M_EARLYCROP,M_COMPOST,M_SLEEPHOSE,M_DRAIN,M_DITCH,M_UNDERSEED,
                                   M_LIME, M_NONINVTILL, M_SSPM, M_SOLIDMANURE,M_STRAWRESIDUE,M_MECHWEEDS,M_PESTICIDES_DST)]
    
    # calculate derivative supporting soil properties
    dt[, D_BDS := calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI)]
    dt[, D_RD := calc_root_depth(B_LU_BRP)]
    dt[, D_OC := calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
    dt[, D_GA := calc_grass_age(ID, B_LU_BRP)]

    # Calculate the crop rotation fraction
    dt[, D_CP_STARCH := calc_rotation_fraction(ID, B_LU_BRP, crop = "starch")]
    dt[, D_CP_POTATO := calc_rotation_fraction(ID, B_LU_BRP, crop = "potato")]
    dt[, D_CP_SUGARBEET := calc_rotation_fraction(ID, B_LU_BRP, crop = "sugarbeet")]
    dt[, D_CP_GRASS := calc_rotation_fraction(ID, B_LU_BRP, crop = "grass")]
    dt[, D_CP_MAIS := calc_rotation_fraction(ID, B_LU_BRP, crop = "mais")]
    dt[, D_CP_OTHER := calc_rotation_fraction(ID, B_LU_BRP, crop = "other")]
    dt[, D_CP_RUST := calc_rotation_fraction(ID, B_LU_BRP, crop = "rustgewas")]
    dt[, D_CP_RUSTDEEP := calc_rotation_fraction(ID, B_LU_BRP, crop = "rustgewasdiep")]
    
    # Calculate series of chemical soil functions
    dt[, D_PH_DELTA := calc_ph_delta(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC, D_CP_STARCH,
                                     D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS, D_CP_MAIS, D_CP_OTHER)]
    dt[, D_CEC := calc_cec(A_CEC_CO)]
    dt[, D_NLV := calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA)]
    dt[, D_PBI := calc_phosphate_availability(B_LU_BRP, A_P_AL, A_P_CC, A_P_WA)]
    dt[, D_K :=  calc_potassium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC,
                                             A_CEC_CO, A_K_CO_PO, A_K_CC)]
    dt[, D_SLV := calc_slv(B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS,A_SOM_LOI,A_S_RT, D_BDS)]
    dt[, D_MG := calc_magnesium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC, A_CEC_CO,
                                             A_K_CO_PO, A_MG_CC, A_K_CC)]
    dt[, D_CU := calc_copper_availability(B_LU_BRP, A_SOM_LOI, A_CLAY_MI, A_K_CC, A_MN_CC, A_CU_CC)]
    dt[, D_ZN := calc_zinc_availability(B_LU_BRP, B_SOILTYPE_AGR, A_PH_CC, A_ZN_CC)]
    
    # Calculate series of physical soil functions
    dt[, D_SE := calc_sealing_risk(A_SOM_LOI, A_CLAY_MI)]
    dt[, D_CR := calc_crumbleability(A_SOM_LOI, A_CLAY_MI,A_PH_CC)]
    dt[, D_WE := calc_winderodibility(B_LU_BRP, A_CLAY_MI, A_SILT_MI)]
    dt[, D_AS := calc_aggregatestability(B_SOILTYPE_AGR,A_SOM_LOI,A_K_CO_PO,A_CA_CO_PO,A_MG_CO_PO)]
    dt[, D_WSI_DS := calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'droughtstress')]
    dt[, D_WSI_WS := calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'wetnessstress')]
    dt[, D_WRI := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'plant available water')]
    dt[, D_WO := calc_workability(A_CLAY_MI, A_SILT_MI, B_LU_BRP, B_SOILTYPE_AGR, B_GWL_GLG, B_GWL_GHG, B_Z_TWO)]
    
    # Calculate series of biological soil functions
    dt[, D_PMN := calc_pmn(B_LU_BRP, B_SOILTYPE_AGR, A_N_PMN)]
    
    # Calculate series of environmental soil functions
    dt[,D_NGW := calc_nleach(B_SOILTYPE_AGR, B_LU_BRP, B_GWL_CLASS, D_NLV, B_AER_CBS, leaching_to = "gw")]
    dt[,D_NSW := calc_nleach(B_SOILTYPE_AGR, B_LU_BRP, B_GWL_CLASS, D_NLV, B_AER_CBS, leaching_to = "ow")]
    
    # Calculate series of management actions
    dt[, D_SOM_BAL := calc_sombalance(B_LU_BRP,A_SOM_LOI, A_P_AL, A_P_WA, M_COMPOST, M_GREEN)]
    dt[, D_MAN := calc_management(A_SOM_LOI,B_LU_BRP, B_SOILTYPE_AGR,B_GWL_CLASS,
                                  D_SOM_BAL,D_CP_GRASS,D_CP_POTATO,D_CP_RUST,D_CP_RUSTDEEP,D_GA,
                                  M_COMPOST,M_GREEN, M_NONBARE, M_EARLYCROP, M_SLEEPHOSE, M_DRAIN, M_DITCH, M_UNDERSEED,
                                  M_LIME, M_NONINVTILL, M_SSPM, M_SOLIDMANURE,M_STRAWRESIDUE,M_MECHWEEDS,M_PESTICIDES_DST
                                  )]
  
    # Calculate the score of the BodemConditieScore
    dt[, D_BCS := calc_bcs(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, D_PH_DELTA, 
                           A_EW_BCS, A_SC_BCS, A_GS_BCS,A_P_BCS, A_C_BCS, A_RT_BCS, A_RD_BCS, A_SS_BCS, A_CC_BCS)]
  
 
  # Step 2 Add indicators ------------------
  
    # Calculate indicators for soil chemical functions
    dt[, I_C_N := ind_nitrogen(D_NLV, B_LU_BRP)]
    dt[, I_C_P := ind_phosphate_availability(D_PBI)]
    dt[, I_C_K := ind_potassium(D_K,B_LU_BRP,B_SOILTYPE_AGR,A_SOM_LOI)]
    dt[, I_C_MG := ind_magnesium(D_MG, B_LU_BRP, B_SOILTYPE_AGR)]
    dt[, I_C_S := ind_sulpher(D_SLV, B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS)]
    dt[, I_C_PH := ind_ph(D_PH_DELTA)]
    dt[, I_C_CEC := ind_cec(D_CEC)]
    dt[, I_C_CU := ind_copper(D_CU,B_LU_BRP)]
    dt[, I_C_ZN := ind_zinc(D_ZN)]
    
    # Calculate indicators for soil physical functions
    dt[, I_P_CR := ind_crumbleability(D_CR, B_LU_BRP)]
    dt[, I_P_SE := ind_sealing(D_SE, B_LU_BRP)]
    dt[, I_P_DS := ind_waterstressindex(D_WSI_DS)]
    dt[, I_P_WS := ind_waterstressindex(D_WSI_WS)]
    dt[, I_P_DU := ind_winderodibility(D_WE)]
    dt[, I_P_CO := ind_compaction(B_SC_WENR)]
    dt[, I_P_WRI := ind_waterretention(D_WRI)]
    dt[, I_P_CEC := ind_aggregatestability(D_AS)]
    dt[, I_P_WO := ind_workability(D_WO)]
  
    # Calculate indicators for soil biological functions
    dt[, I_B_DI := ind_resistance(A_SOM_LOI)]
    dt[, I_B_SF := ind_pmn(D_PMN)]
    dt[, I_B_NEM := fifelse(is.na(A_NEMA),ind_nematodes(B_LU_BRP = B_LU_BRP,
                                  A_RLN_PR_TOT, A_RLN_PR_CREN, A_RLN_PR_NEG, A_RLN_PR_PEN, A_RLN_PR_PRA, A_RLN_PR_THO, A_RLN_PR_FLA,    
                                  A_RLN_PR_FAL, A_RLN_PR_PIN, A_RLN_PR_PSE, A_RLN_PR_VUL, A_RLN_PR_DUN, A_RLN_PR_ZEA, A_RKN_ME_TOT,   
                                  A_RKN_ME_HAP, A_RKN_ME_CHIFAL, A_RKN_ME_CHI, A_RKN_ME_NAA, A_RKN_ME_FAL, A_RKN_ME_MIN, A_RKN_ME_INC,    
                                  A_RKN_ME_JAV, A_RKN_ME_ART, A_RKN_ME_ARE, A_RKN_ME_ARD, A_DSN_TR_TOT, A_DSN_TR_SIM, A_DSN_TR_PRI,    
                                  A_DSN_TR_VIR, A_DSN_TR_SPA, A_DSN_TR_CYL, A_DSN_TR_HOO, A_DSN_PA_TER, A_DSN_PA_PAC, A_DSN_PA_ANE,    
                                  A_DSN_PA_NAN, A_DSN_TY_TOT, A_DSN_RO_TOT, A_DSN_XI_TOT, A_DSN_LO_TOT, A_DSN_HEM_TOT, A_DSN_HEL_TOT,   
                                  A_SN_DI_TOT, A_SN_DI_DIP, A_SN_DI_DES, A_OPN_PA_TOT, A_OPN_PA_BUK, A_OPN_CY_TOT, A_OPN_AP_TOT,    
                                  A_OPN_AP_FRA, A_OPN_AP_RIT, A_OPN_AP_SUB, A_OPN_CR_TOT, A_OPN_SU_TOT,A_NPN_SA_TOT),
                            ind_nematodes_list(A_NEMA))
       ]
  
    # Calculate indicators for soil visual assessment (optional)
    bcs.par <- c('I_B_EW_BCS', 'I_P_SC_BCS', 'I_P_GS_BCS', 'I_P_P_BCS', 'I_P_C_BCS', 'I_P_RT_BCS', 'I_P_RD_BCS',
                 'I_P_SS_BCS', 'I_P_CC_BCS')
    dt[,c(bcs.par) := calc_bcs(B_LU_BRP,B_SOILTYPE_AGR,A_SOM_LOI, D_PH_DELTA,
                              A_EW_BCS, A_SC_BCS, A_GS_BCS, A_P_BCS, A_C_BCS, A_RT_BCS, A_RD_BCS, A_SS_BCS, A_CC_BCS,
                              type = 'indicator')]
  
    # overwrite soil physical functions for compaction when BCS is available
    dt[,D_P_CO := (3 * A_EW_BCS + 3 * A_SC_BCS + 3 * A_RD_BCS  - 2 * A_P_BCS - A_RT_BCS)/18]
    dt[,D_P_CO := pmax(0, D_P_CO)]
    dt[,I_P_CO := fifelse(is.na(D_P_CO),I_P_CO,D_P_CO)]
    
    # overwrite soil physical functions for aggregate stability when BCS is available
    dt[,D_P_CEC := (3 * A_EW_BCS + 3 * A_SS_BCS - A_C_BCS)/12]
    dt[,D_P_CEC := pmax(0, D_P_CEC)]
    dt[,I_P_CEC := fifelse(is.na(D_P_CEC),I_P_CEC,D_P_CEC)]  
  
    # Calculate Visual Soil Assessment Indicator
    dt[, I_BCS := ind_bcs(D_BCS)]
    
    # Calculate indicators for soil management
    dt[, I_M_GREEN := fifelse(M_GREEN == TRUE, 1,0)]
    dt[, I_M_COMPOST := fifelse(M_COMPOST > 0,1,0)]
    dt[, I_M_NONBARE := fifelse(M_NONBARE == TRUE, 1,0)]
    dt[, I_M_EARLYCROP := fifelse(M_EARLYCROP == TRUE, 1,0)]
    dt[, I_M_SLEEPHOSE := fifelse(M_SLEEPHOSE == TRUE & B_SOILTYPE_AGR != 'veen' & crop_category == 'mais',1,0)]
    dt[, I_M_SLEEPHOSE := fifelse(M_SLEEPHOSE == TRUE & crop_category == 'grasland',1,I_M_SLEEPHOSE)]
    dt[, I_M_DRAIN := fifelse(M_DRAIN == TRUE & B_SOILTYPE_AGR == 'veen',1,0)]
    dt[, I_M_DITCH := fifelse(M_DITCH == TRUE & B_SOILTYPE_AGR == 'veen',1,0)]
    dt[, I_M_UNDERSEED := fifelse(M_UNDERSEED == TRUE,1,0)]
    dt[, I_M_LIME := fifelse(M_LIME == TRUE,1,0)]
    dt[, I_M_NONINVTILL := fifelse(M_NONINVTILL == TRUE,1,0)]
    dt[, I_M_SSPM := fifelse(M_SSPM == TRUE,1,0)]
    dt[, I_M_SOLIDMANURE := fifelse(M_SOLIDMANURE == TRUE,1,0)]
    dt[, I_M_STRAWRESIDUE := fifelse(M_STRAWRESIDUE == TRUE,1,0)]
    dt[, I_M_MECHWEEDS := fifelse(M_MECHWEEDS == TRUE,1,0)]
    dt[, I_M_PESTICIDES_DST := fifelse(M_PESTICIDES_DST == TRUE,1,0)]
    
    # Calculate integrated management indicator
    dt[, I_M := ind_management(D_MAN, B_LU_BRP, B_SOILTYPE_AGR)]
  
    # Calculate indicators for environment
    dt[, I_E_NGW := ind_nretention(D_NGW, leaching_to = "gw")]
    dt[, I_E_NSW := ind_nretention(D_NSW, leaching_to = "ow")]

  # Step 3 Reformat dt given weighing per indicator and prepare for aggregation  ------------------
    
    # load weights.obic (set indicator to zero when not applicable)
    w <- as.data.table(OBIC::weight.obic)
    
    # Add years per field
    dt[,year := .I, by = ID]
    
    # Select all indicators used for scoring
    cols <- colnames(dt)[grepl('I_C|I_B|I_P|I_E|I_M|year|crop_cat|SOILT',colnames(dt))]
    #cols <- cols[!(grepl('^I_P|^I_B',cols) & grepl('_BCS$',cols))]
    #cols <- cols[!grepl('^I_M_',cols)]
    
    # Melt dt and assign main categories for OBI
    dt.melt <- melt(dt[,mget(cols)],
                    id.vars = c('B_SOILTYPE_AGR','crop_category','year'), 
                    variable.name = 'indicator')
    
    # add categories relevant for aggregating
    # C = chemical, P = physics, B = biological, BCS = visual soil assessment
    # indicators not used for integrating: IBCS and IM
    dt.melt[,cat := tstrsplit(indicator,'_',keep = 2)]
    dt.melt[grepl('_BCS$',indicator) & indicator != 'I_BCS', cat := 'IBCS']
    dt.melt[grepl('^I_M_',indicator), cat := 'IM']
    
    # Determine amount of indicators per category
    dt.melt.ncat <- dt.melt[year==1 & !cat %in% c('IBCS','IM')][,list(ncat = .N),by='cat']
    
    # add weighing factor to indicator values
    dt.melt <- merge(dt.melt,w[,list(crop_category,indicator,weight_nonpeat,weight_peat)], 
                     by = c('crop_category','indicator'), all.x = TRUE)
    
    # calculate correction factor for indicator values (low values have more impact than high values, a factor 5)
    dt.melt[,cf := cf_ind_importance(value)]
    
    # calculate weighted value for crop category
    dt.melt[,value.w := value]
    dt.melt[grepl('veen',B_SOILTYPE_AGR) & weight_peat < 0,value.w := -999]
    dt.melt[!grepl('veen',B_SOILTYPE_AGR) & weight_nonpeat < 0,value.w := -999]
    
  # Step 4 Aggregate indicators ------------------

    # subset dt.melt for relevant columns only
    out.ind <-  dt.melt[,list(indicator,year,value = value.w)]
    
    # calculate correction factor per year; recent years are more important
    out.ind[,cf := log(12 - pmin(10,year))]
    
    # calculate weighted average per indicator over the year
    out.ind <- out.ind[,list(value = round(sum(cf * pmax(0,value) / sum(cf[value >= 0])),3)), by = indicator]
       
    # non relevant indicators, set to -999
    out.ind[is.na(value), value := -999]
    
  # Step 5 Add scores ------------------
    
    # subset dt.melt for relevant columns only
    out.score <-  dt.melt[,list(cat, year, cf, value = value.w)]
  
    # remove indicator categories that are not used for scoring
    out.score <- out.score[!cat %in% c('IBCS','IM','BCS')]
    
    # calculate weighted average per indicator category
    out.score <- out.score[,list(value = sum(cf * pmax(0,value) / sum(cf[value >= 0]))), by = list(cat,year)]
  
      # for case that a cat has one indicator or one year and has NA
      out.score[is.na(value), value := -999]
      
      # calculate correction factor per year; recent years are more important
      out.score[,cf := log(12 - pmin(10,year))]
  
    # calculate weighted average per indicator category per year
    out.score <- out.score[,list(value = sum(cf * pmax(0,value)/ sum(cf[value >= 0]))), by = cat]
  
      # merge out with number per category
      out.score <- merge(out.score,dt.melt.ncat, by='cat')
    
      # calculate weighing factor depending on number of indicators
      out.score[,cf := log(ncat + 1)]
  
    # calculated final obi score
    out.score <- rbind(out.score[,list(cat,value)],
                       out.score[,list(cat = "T",value = sum(value * cf / sum(cf)))])
  
    # update element names
    out.score[,cat := paste0('S_',cat,'_OBI_A')]
    out.score[, value := round(value,3)]
   
#  Step 6 Add recommendations ------------------
    
    # dcast output
    out.ind[,id:=1]
    out.ind <- dcast(out.ind,id~indicator)[,id:=NULL]
    
    # dcast output
    out.score[,id:=1]
    out.score <- dcast(out.score,id~cat)[,id:=NULL]
    
    # get most occurring soil type and crop type
    dt.sc <- dt[, lapply(.SD, function (x) names(sort(table(x),decreasing = TRUE)[1])), 
                            .SDcols = c('B_LU_BRP','B_SOILTYPE_AGR'),by = ID]
    dt.sc[, B_LU_BRP := as.integer(B_LU_BRP)]
    
    # combine indicators and score in one data.table
    dt.score <- data.table(dt.sc,out.ind,out.score)
  
    # evaluate measures
    dt.measure <- OBIC::obic_evalmeasure(dt.score, extensive = FALSE)
    
    # make recommendations of top 3 measures
    out.recom <- OBIC::obic_recommendations(dt.measure)
    
  #  Step 6 Combine all outputs into one ------------------
 
    # combine both outputs
    if(output == 'all'){out <- data.table(out.ind,out.score,out.recom)}
    if(output == 'indicators'){out <- out.ind}
    if(output == 'recommendations'){out <- out.recom}
    if(output == 'scores'){out <- out.score}
    if(output == 'obic_score'){out <- out.score[,'S_T_OBI_A']}
  
  # return output
  return(out)
}

#' Calculate the Open Bodem Index score for a single field
#' 
#' This functions wraps the functions of the OBIC into one main function to calculate the score for Open Bodem Index (OBI).
#' In contrast to obic_field, this wrapper can handle a data.table as input.
#' 
#' @param dt (data.table) A data.table containing the data of the fields to calculate the OBI
#' @param output (character) An optional argument to select output: obic_score, scores, indicators, recommendations, or all. (default = all)
#' 
#' @import data.table
#' 
#' @export
obic_field_dt <- function(dt,output = 'all') {
  
  # make local copy
  dt <- copy(dt)
  
  # Check inputs
  checkmate::assert_data_table(dt)
  
  # column names in input
  dt.cols <- colnames(dt)
  
  # column names mandatory
  dt.req <- c('B_SOILTYPE_AGR','B_GWL_CLASS','B_SC_WENR','B_HELP_WENR','B_AER_CBS', 'B_LU_BRP', 
              'B_GWL_GLG','B_GWL_GHG','B_Z_TWO',
              'A_SOM_LOI', 'A_SAND_MI', 'A_SILT_MI', 'A_CLAY_MI','A_PH_CC','A_CACO3_IF',
              'A_N_RT','A_CN_FR','A_COM_FR', 'A_S_RT','A_N_PMN','A_P_AL', 'A_P_CC', 'A_P_WA',
              'A_CEC_CO','A_CA_CO_PO', 'A_MG_CO_PO', 'A_K_CO_PO',
              'A_K_CC', 'A_MG_CC', 'A_MN_CC', 'A_ZN_CC', 'A_CU_CC')
  
  # check input
  dt.check <- length(dt.req[dt.req %in% dt.cols]) == 32
  
  # check type of dt
  checkmate::assert_true(dt.check)
  
  # check which BodemConditieScore input is missing
  bcs.all <- c('A_C_BCS', 'A_CC_BCS','A_GS_BCS','A_P_BCS','A_RD_BCS','A_EW_BCS','A_SS_BCS','A_RT_BCS','A_SC_BCS')
  bcs.missing <- bcs.all[!bcs.all %in% colnames(dt)]
  
  # check which Soil Measures input is missing
  sm.all <- c('M_GREEN', 'M_NONBARE', 'M_EARLYCROP','M_SLEEPHOSE','M_DRAIN','M_DITCH','M_UNDERSEED',
              'M_LIME', 'M_NONINVTILL', 'M_SSPM', 'M_SOLIDMANURE','M_STRAWRESIDUE','M_MECHWEEDS','M_PESTICIDES_DST')
  sm.missing <- sm.all[!sm.all %in% colnames(dt)]
  
  # check if compost measure is missing
  smc.all <- 'M_COMPOST'
  smc.missing <- smc.all[!smc.all %in% colnames(dt)]
  
  # chek if nematodes are missing
  nema.all <- c('A_RLN_PR_TOT', 'A_RLN_PR_CREN', 'A_RLN_PR_NEG', 'A_RLN_PR_PEN', 'A_RLN_PR_PRA', 'A_RLN_PR_THO', 'A_RLN_PR_FLA', 
                'A_RLN_PR_FAL', 'A_RLN_PR_PIN', 'A_RLN_PR_PSE', 'A_RLN_PR_VUL', 'A_RLN_PR_DUN', 'A_RLN_PR_ZEA', 'A_RKN_ME_TOT', 
                'A_RKN_ME_HAP', 'A_RKN_ME_CHIFAL', 'A_RKN_ME_CHI', 'A_RKN_ME_NAA', 'A_RKN_ME_FAL', 'A_RKN_ME_MIN', 'A_RKN_ME_INC', 
                'A_RKN_ME_JAV', 'A_RKN_ME_ART', 'A_RKN_ME_ARE', 'A_RKN_ME_ARD', 'A_DSN_TR_TOT', 'A_DSN_TR_SIM', 'A_DSN_TR_PRI', 
                'A_DSN_TR_VIR', 'A_DSN_TR_SPA', 'A_DSN_TR_CYL', 'A_DSN_TR_HOO', 'A_DSN_PA_TER', 'A_DSN_PA_PAC', 'A_DSN_PA_ANE', 
                'A_DSN_PA_NAN', 'A_DSN_TY_TOT', 'A_DSN_RO_TOT', 'A_DSN_XI_TOT', 'A_DSN_LO_TOT', 'A_DSN_HEM_TOT', 'A_DSN_HEL_TOT', 
                'A_SN_DI_TOT', 'A_SN_DI_DIP', 'A_SN_DI_DES', 'A_OPN_PA_TOT', 'A_OPN_PA_BUK', 'A_OPN_CY_TOT', 'A_OPN_AP_TOT', 
                'A_OPN_AP_FRA', 'A_OPN_AP_RIT', 'A_OPN_AP_SUB', 'A_OPN_CR_TOT', 'A_OPN_SU_TOT', 'A_NPN_SA_TOT')
  nema.missing <- nema.all[!nema.all %in% colnames(dt)]
  
  # extend dt with missing elements, so that these are replaced by default estimates
  if(length(bcs.missing)>0){dt[,c(bcs.missing) := NA]}
  if(length(sm.missing)>0){dt[,c(sm.missing) := NA]}
  if(length(smc.missing)>0){dt[,c(smc.missing) := NA_real_]}
  if(length(nema.missing)>0){dt[,c(nema.missing) := NA_integer_]}
  
  # calculate obic_field
  out <- obic_field(dt$B_SOILTYPE_AGR,dt$B_GWL_CLASS,dt$B_SC_WENR,dt$B_HELP_WENR,dt$B_AER_CBS,
                    dt$B_GWL_GLG,dt$B_GWL_GHG,dt$B_Z_TWO,
                    dt$B_LU_BRP, 
                    dt$A_SOM_LOI, dt$A_SAND_MI, dt$A_SILT_MI, dt$A_CLAY_MI,dt$A_PH_CC,dt$A_CACO3_IF,
                    dt$A_N_RT,dt$A_CN_FR,dt$A_COM_FR, dt$A_S_RT,dt$A_N_PMN,
                    dt$A_P_AL, dt$A_P_CC, dt$A_P_WA,
                    dt$A_CEC_CO,dt$A_CA_CO_PO, dt$A_MG_CO_PO, dt$A_K_CO_PO,
                    dt$A_K_CC, dt$A_MG_CC, dt$A_MN_CC, dt$A_ZN_CC, dt$A_CU_CC,
                    dt$A_C_BCS, dt$A_CC_BCS,dt$A_GS_BCS,dt$A_P_BCS,dt$A_RD_BCS,
                    dt$A_EW_BCS,dt$A_SS_BCS,dt$A_RT_BCS,dt$A_SC_BCS,
                    dt$M_COMPOST,dt$M_GREEN, dt$M_NONBARE, dt$M_EARLYCROP, 
                    dt$M_SLEEPHOSE,dt$M_DRAIN,dt$M_DITCH,dt$M_UNDERSEED,
                    dt$M_LIME, dt$M_NONINVTILL, dt$M_SSPM, dt$M_SOLIDMANURE,
                    dt$M_STRAWRESIDUE,dt$M_MECHWEEDS,dt$M_PESTICIDES_DST,
                    dt$A_RLN_PR_TOT, dt$A_RLN_PR_CREN, dt$A_RLN_PR_NEG, dt$A_RLN_PR_PEN, dt$A_RLN_PR_PRA, dt$A_RLN_PR_THO, dt$A_RLN_PR_FLA, 
                    dt$A_RLN_PR_FAL, dt$A_RLN_PR_PIN, dt$A_RLN_PR_PSE, dt$A_RLN_PR_VUL, dt$A_RLN_PR_DUN, dt$A_RLN_PR_ZEA, dt$A_RKN_ME_TOT, 
                    dt$A_RKN_ME_HAP, dt$A_RKN_ME_CHIFAL, dt$A_RKN_ME_CHI, dt$A_RKN_ME_NAA, dt$A_RKN_ME_FAL, dt$A_RKN_ME_MIN, dt$A_RKN_ME_INC, 
                    dt$A_RKN_ME_JAV, dt$A_RKN_ME_ART, dt$A_RKN_ME_ARE, dt$A_RKN_ME_ARD, dt$A_DSN_TR_TOT, dt$A_DSN_TR_SIM, dt$A_DSN_TR_PRI, 
                    dt$A_DSN_TR_VIR, dt$A_DSN_TR_SPA, dt$A_DSN_TR_CYL, dt$A_DSN_TR_HOO, dt$A_DSN_PA_TER, dt$A_DSN_PA_PAC, dt$A_DSN_PA_ANE, 
                    dt$A_DSN_PA_NAN, dt$A_DSN_TY_TOT, dt$A_DSN_RO_TOT, dt$A_DSN_XI_TOT, dt$A_DSN_LO_TOT, dt$A_DSN_HEM_TOT, dt$A_DSN_HEL_TOT, 
                    dt$A_SN_DI_TOT, dt$A_SN_DI_DIP, dt$A_SN_DI_DES, dt$A_OPN_PA_TOT, dt$A_OPN_PA_BUK, dt$A_OPN_CY_TOT, dt$A_OPN_AP_TOT, 
                    dt$A_OPN_AP_FRA, dt$A_OPN_AP_RIT, dt$A_OPN_AP_SUB, dt$A_OPN_CR_TOT, dt$A_OPN_SU_TOT, dt$A_NPN_SA_TOT,
                    ID = 1,output = output)
  
  # return output
  return(out)
  
}
