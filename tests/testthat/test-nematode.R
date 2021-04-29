library(data.table)

# make data.table with testspecies
testspecies = c("Ditylenchus spp."             ,"Ditylenchus dipsaci"          ,"Ditylenchus destructor"       ,"Xiphinema spp."           ,   
                "Longidorus spp."              ,"(Para)Trichodoridae spp."     ,"Trichodorus similis"          ,"Trichodorus primitivus"   ,   
                "Trichodorus viruliferus"      ,"Trichodorus sparsus"          ,"Trichodorus cylindricus"      ,"Trichodorus hooperi"      ,   
                "Paratrichodorus teres"        ,"Paratrichodorus pachydermus"  ,"Paratrichodorus anemones"     ,"Paratrichodorus nanus"    ,   
                "Aphelenchoides spp."          ,"Aphelenchoides fragariae"     ,"Aphelenchoides ritzemabosi"   ,"Aphelenchoides subtenuis" ,   
                "Criconematidae spp."          ,"Subanguina spp."              ,"Rotylenchus spp."             ,"Paratylenchus spp."       ,   
                "Paratylenchus bukowinensis"   ,"Meloidogyne spp."             ,"Meloidogyne chitwoodi/fallax" ,"Meloidogyne chitwoodi"    ,   
                "Meloidogyne fallax"           ,"Meloidogyne minor"            ,"Meloidogyne incognita"        ,"Meloidogyne javanica"     ,   
                "Meloidogyne artiellia"        ,"Meloidogyne arenaria"         ,"Meloidogyne ardenensis"       ,"Meloidogyne naasi"        ,   
                "Meloidogyne hapla"            ,"Cysteaaltjes"                 ,"Hemicycliophora spp."         ,"Pratylenchus spp."        ,   
                "Pratylenchus penetrans"       ,"Pratylenchus crenatus"        ,"Tylenchorhynchus spp."        ,"Helicotylenchus spp."     ,   
                "Pratylenchus neglectus"       ,"Pratylenchus pratensis"       ,"Pratylenchus thornei"         ,"Pratylenchus flakkensis"  ,   
                "Pratylenchus fallax"          ,"Pratylenchus pinguicaudatus"  ,"Pratylenchus pseudopratensis" ,"Pratylenchus vulnus"      ,   
                "Pratylenchus dunensis"        ,"Pratylenchus zeae")

# test function for ind_nematodes_list
test_that("ind_nematodes works", {
  expect_equal(
    ind_nematodes_list(
      data.table(species = testspecies,
                 count = rep(0,54))
      ),
    expected = 1,
    tolecance = 0.01
  )
  expect_lt(
    ind_nematodes_list(
      data.table(species = testspecies,
                 count = rep(10000,54))
    ),
    expected = 0.0011
  )
})

# number of fields
nfields = 15

# set random number for testing
set.seed(123)
nmet = pmax(0,rnorm(nfields,100,50))
A_RLN_PR_TOT=nmet; A_RLN_PR_CREN=nmet; A_RLN_PR_NEG=nmet; A_RLN_PR_PEN=nmet; A_RLN_PR_PRA=nmet; A_RLN_PR_THO=nmet; A_RLN_PR_FLA=nmet;    
A_RLN_PR_FAL=nmet; A_RLN_PR_PIN=nmet; A_RLN_PR_PSE=nmet; A_RLN_PR_VUL=nmet; A_RLN_PR_DUN=nmet; A_RLN_PR_ZEA=nmet; 

set.seed(123)
nmet = pmax(0,rnorm(nfields,50,15))
A_RKN_ME_TOT=nmet; A_RKN_ME_HAP=nmet; A_RKN_ME_CHIFAL=nmet; A_RKN_ME_CHI=nmet; A_RKN_ME_NAA=nmet; A_RKN_ME_FAL=nmet; A_RKN_ME_MIN=nmet; A_RKN_ME_INC=nmet;    
A_RKN_ME_JAV=nmet; A_RKN_ME_ART=nmet; A_RKN_ME_ARE=nmet; A_RKN_ME_ARD=nmet

set.seed(123)
nmet = pmax(0,rnorm(nfields,5,1))
A_DSN_TR_TOT=nmet; A_DSN_TR_SIM=nmet; A_DSN_TR_PRI=nmet;    
A_DSN_TR_VIR=nmet; A_DSN_TR_SPA=nmet; A_DSN_TR_CYL=nmet; A_DSN_TR_HOO=nmet; A_DSN_PA_TER=nmet; A_DSN_PA_PAC=nmet; A_DSN_PA_ANE=nmet;    
A_DSN_PA_NAN=nmet; A_DSN_TY_TOT=nmet; A_DSN_RO_TOT=nmet; A_DSN_XI_TOT=nmet; A_DSN_LO_TOT=nmet; A_DSN_HEM_TOT=nmet; A_DSN_HEL_TOT=nmet;   
A_SN_DI_TOT=nmet; A_SN_DI_DIP=nmet; A_SN_DI_DES=nmet; A_OPN_PA_TOT=nmet; A_OPN_PA_BUK=nmet; A_OPN_CY_TOT=nmet; A_OPN_AP_TOT=nmet;    
A_OPN_AP_FRA=nmet; A_OPN_AP_RIT=nmet; A_OPN_AP_SUB=nmet; A_OPN_CR_TOT=nmet; A_OPN_SU_TOT = nmet;A_NPN_SA_TOT = nmet

set.seed(123)
B_LU_BRP = sample(c(2014, 265,1079, 265, 308),nfields,replace = T)

test_that("ind_nematodes works with complete input", {
  expect_equal(
    ind_nematodes(B_LU_BRP = B_LU_BRP,
                  A_RLN_PR_TOT = A_RLN_PR_TOT,A_RLN_PR_CREN = A_RLN_PR_CREN,A_RLN_PR_NEG = A_RLN_PR_NEG,
                  A_RLN_PR_PEN = A_RLN_PR_PEN,A_RLN_PR_PRA = A_RLN_PR_PRA,A_RLN_PR_THO = A_RLN_PR_THO,
                  A_RLN_PR_FLA = A_RLN_PR_FLA,A_RLN_PR_FAL = A_RLN_PR_FAL,A_RLN_PR_PIN = A_RLN_PR_PIN,
                  A_RLN_PR_PSE = A_RLN_PR_PSE,A_RLN_PR_VUL = A_RLN_PR_VUL,A_RLN_PR_DUN = A_RLN_PR_DUN,
                  A_RLN_PR_ZEA = A_RLN_PR_ZEA,A_RKN_ME_TOT = A_RKN_ME_TOT,A_RKN_ME_HAP = A_RKN_ME_HAP,
                  A_RKN_ME_CHIFAL = A_RKN_ME_CHIFAL,
                  A_RKN_ME_CHI = A_RKN_ME_CHI,A_RKN_ME_NAA = A_RKN_ME_NAA,A_RKN_ME_FAL = A_RKN_ME_FAL,
                  A_RKN_ME_MIN = A_RKN_ME_MIN,A_RKN_ME_INC = A_RKN_ME_INC,A_RKN_ME_JAV = A_RKN_ME_JAV,
                  A_RKN_ME_ART = A_RKN_ME_ART,A_RKN_ME_ARE = A_RKN_ME_ARE,A_RKN_ME_ARD = A_RKN_ME_ARD,
                  A_DSN_TR_TOT = A_DSN_TR_TOT,A_DSN_TR_SIM = A_DSN_TR_SIM,A_DSN_TR_PRI = A_DSN_TR_PRI,
                  A_DSN_TR_VIR = A_DSN_TR_VIR,A_DSN_TR_SPA = A_DSN_TR_SPA,A_DSN_TR_CYL = A_DSN_TR_CYL,
                  A_DSN_TR_HOO = A_DSN_TR_HOO,A_DSN_PA_TER = A_DSN_PA_TER,A_DSN_PA_PAC = A_DSN_PA_PAC,
                  A_DSN_PA_ANE = A_DSN_PA_ANE,A_DSN_PA_NAN = A_DSN_PA_NAN,A_DSN_TY_TOT = A_DSN_TY_TOT,
                  A_DSN_RO_TOT = A_DSN_RO_TOT,A_DSN_XI_TOT = A_DSN_XI_TOT,A_DSN_LO_TOT = A_DSN_LO_TOT,
                  A_DSN_HEM_TOT = A_DSN_HEM_TOT,A_DSN_HEL_TOT = A_DSN_HEL_TOT,
                  A_SN_DI_TOT = A_SN_DI_TOT,A_SN_DI_DIP = A_SN_DI_DIP, A_SN_DI_DES = A_SN_DI_DES,
                  A_OPN_PA_TOT = A_OPN_PA_TOT,A_OPN_PA_BUK = A_OPN_PA_BUK,A_OPN_CY_TOT = A_OPN_CY_TOT,
                  A_OPN_AP_TOT = A_OPN_AP_TOT,A_OPN_AP_FRA = A_OPN_AP_FRA,A_OPN_AP_RIT = A_OPN_AP_RIT,
                  A_OPN_AP_SUB = A_OPN_AP_SUB,A_OPN_CR_TOT = A_OPN_CR_TOT,A_OPN_SU_TOT = A_OPN_SU_TOT,
                  A_NPN_SA_TOT = A_NPN_SA_TOT
    ),
    expected = c(0.261, 0.224, 0.023, 0.193, 0.188, 0.017, 0.126, 0.356, 0.277, 0.247, 0.040, 0.144, 0.136, 0.189, 0.260),
    tolecance = 0.01
  )
})

# add some NA values
set.seed(123)
selrow = sample(1:nfields,2)

A_DSN_TR_CYL[selrow] <- NA
A_RLN_PR_FAL[selrow] <- NA
A_RKN_ME_JAV[selrow] <- NA
A_OPN_SU_TOT[selrow] <- NA

test_that("ind_nematodes works with complete input but with missing values", {
  expect_equal(
    ind_nematodes(B_LU_BRP = B_LU_BRP,
                  A_RLN_PR_TOT = A_RLN_PR_TOT,A_RLN_PR_CREN = A_RLN_PR_CREN,A_RLN_PR_NEG = A_RLN_PR_NEG,
                  A_RLN_PR_PEN = A_RLN_PR_PEN,A_RLN_PR_PRA = A_RLN_PR_PRA,A_RLN_PR_THO = A_RLN_PR_THO,
                  A_RLN_PR_FLA = A_RLN_PR_FLA,A_RLN_PR_FAL = A_RLN_PR_FAL,A_RLN_PR_PIN = A_RLN_PR_PIN,
                  A_RLN_PR_PSE = A_RLN_PR_PSE,A_RLN_PR_VUL = A_RLN_PR_VUL,A_RLN_PR_DUN = A_RLN_PR_DUN,
                  A_RLN_PR_ZEA = A_RLN_PR_ZEA,A_RKN_ME_TOT = A_RKN_ME_TOT,A_RKN_ME_HAP = A_RKN_ME_HAP,
                  A_RKN_ME_CHIFAL = A_RKN_ME_CHIFAL,
                  A_RKN_ME_CHI = A_RKN_ME_CHI,A_RKN_ME_NAA = A_RKN_ME_NAA,A_RKN_ME_FAL = A_RKN_ME_FAL,
                  A_RKN_ME_MIN = A_RKN_ME_MIN,A_RKN_ME_INC = A_RKN_ME_INC,A_RKN_ME_JAV = A_RKN_ME_JAV,
                  A_RKN_ME_ART = A_RKN_ME_ART,A_RKN_ME_ARE = A_RKN_ME_ARE,A_RKN_ME_ARD = A_RKN_ME_ARD,
                  A_DSN_TR_TOT = A_DSN_TR_TOT,A_DSN_TR_SIM = A_DSN_TR_SIM,A_DSN_TR_PRI = A_DSN_TR_PRI,
                  A_DSN_TR_VIR = A_DSN_TR_VIR,A_DSN_TR_SPA = A_DSN_TR_SPA,A_DSN_TR_CYL = A_DSN_TR_CYL,
                  A_DSN_TR_HOO = A_DSN_TR_HOO,A_DSN_PA_TER = A_DSN_PA_TER,A_DSN_PA_PAC = A_DSN_PA_PAC,
                  A_DSN_PA_ANE = A_DSN_PA_ANE,A_DSN_PA_NAN = A_DSN_PA_NAN,A_DSN_TY_TOT = A_DSN_TY_TOT,
                  A_DSN_RO_TOT = A_DSN_RO_TOT,A_DSN_XI_TOT = A_DSN_XI_TOT,A_DSN_LO_TOT = A_DSN_LO_TOT,
                  A_DSN_HEM_TOT = A_DSN_HEM_TOT,A_DSN_HEL_TOT = A_DSN_HEL_TOT,
                  A_SN_DI_TOT = A_SN_DI_TOT,A_SN_DI_DIP = A_SN_DI_DIP, A_SN_DI_DES = A_SN_DI_DES,
                  A_OPN_PA_TOT = A_OPN_PA_TOT,A_OPN_PA_BUK = A_OPN_PA_BUK,A_OPN_CY_TOT = A_OPN_CY_TOT,
                  A_OPN_AP_TOT = A_OPN_AP_TOT,A_OPN_AP_FRA = A_OPN_AP_FRA,A_OPN_AP_RIT = A_OPN_AP_RIT,
                  A_OPN_AP_SUB = A_OPN_AP_SUB,A_OPN_CR_TOT = A_OPN_CR_TOT,A_OPN_SU_TOT = A_OPN_SU_TOT,
                  A_NPN_SA_TOT = A_NPN_SA_TOT
    ),
    expected = c(0.261, 0.224, 0.023, 0.193, 0.188, 0.017, 0.126, 0.356, 0.277, 0.247, 0.040, 0.144, 0.136, 0.189, 0.260),
    tolecance = 0.01
  )
})


# remove some measurement from input
test_that("ind_nematodes works with incomplete input", {
  expect_equal(
    ind_nematodes(B_LU_BRP = B_LU_BRP,
                  A_RLN_PR_TOT = A_RLN_PR_TOT,A_RLN_PR_CREN = A_RLN_PR_CREN,A_RLN_PR_NEG = A_RLN_PR_NEG,
                  A_RLN_PR_PEN = A_RLN_PR_PEN,A_RLN_PR_PRA = A_RLN_PR_PRA,A_RLN_PR_THO = A_RLN_PR_THO,
                  A_RLN_PR_FLA = NULL,A_RLN_PR_FAL = NULL,A_RLN_PR_PIN = A_RLN_PR_PIN,
                  A_RLN_PR_PSE = A_RLN_PR_PSE,A_RLN_PR_VUL = A_RLN_PR_VUL,A_RLN_PR_DUN = NULL,
                  A_RLN_PR_ZEA = A_RLN_PR_ZEA,A_RKN_ME_TOT = A_RKN_ME_TOT,A_RKN_ME_HAP = A_RKN_ME_HAP,
                  A_RKN_ME_CHIFAL = A_RKN_ME_CHIFAL,
                  A_RKN_ME_CHI = A_RKN_ME_CHI,A_RKN_ME_NAA = A_RKN_ME_NAA,A_RKN_ME_FAL = A_RKN_ME_FAL,
                  A_RKN_ME_MIN = A_RKN_ME_MIN,A_RKN_ME_INC = A_RKN_ME_INC,A_RKN_ME_JAV = A_RKN_ME_JAV,
                  A_RKN_ME_ART = NULL,A_RKN_ME_ARE = A_RKN_ME_ARE,A_RKN_ME_ARD = A_RKN_ME_ARD,
                  A_DSN_TR_TOT = A_DSN_TR_TOT,A_DSN_TR_SIM = NULL,A_DSN_TR_PRI = A_DSN_TR_PRI,
                  A_DSN_TR_VIR = A_DSN_TR_VIR,A_DSN_TR_SPA = A_DSN_TR_SPA,A_DSN_TR_CYL = A_DSN_TR_CYL,
                  A_DSN_TR_HOO = A_DSN_TR_HOO,A_DSN_PA_TER = A_DSN_PA_TER,A_DSN_PA_PAC = A_DSN_PA_PAC,
                  A_DSN_PA_ANE = NULL,A_DSN_PA_NAN = A_DSN_PA_NAN,A_DSN_TY_TOT = A_DSN_TY_TOT,
                  A_DSN_RO_TOT = A_DSN_RO_TOT,A_DSN_XI_TOT = A_DSN_XI_TOT,A_DSN_LO_TOT = A_DSN_LO_TOT,
                  A_DSN_HEM_TOT = A_DSN_HEM_TOT,A_DSN_HEL_TOT = A_DSN_HEL_TOT,
                  A_SN_DI_TOT = A_SN_DI_TOT,A_SN_DI_DIP = A_SN_DI_DIP, A_SN_DI_DES = A_SN_DI_DES,
                  A_OPN_PA_TOT = A_OPN_PA_TOT,A_OPN_PA_BUK = A_OPN_PA_BUK,A_OPN_CY_TOT = A_OPN_CY_TOT,
                  A_OPN_AP_TOT = NULL,A_OPN_AP_FRA = A_OPN_AP_FRA,A_OPN_AP_RIT = NULL,
                  A_OPN_AP_SUB = A_OPN_AP_SUB,A_OPN_CR_TOT = A_OPN_CR_TOT,A_OPN_SU_TOT = A_OPN_SU_TOT,
                  A_NPN_SA_TOT = A_NPN_SA_TOT
    ),
    expected = c(0.261, 0.224, 0.023, 0.193, 0.188, 0.017, 0.126, 0.356, 0.277, 0.247, 0.040, 0.144, 0.136, 0.189, 0.260),
    tolecance = 0.01
  )
})






