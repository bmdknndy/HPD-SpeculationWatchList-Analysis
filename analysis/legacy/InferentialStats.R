##### UPP Inferential Data SWL ###################################################

################################################################################
### Set Up

# Set WD
setwd("/Volumes/BigBrady/UPP_PASSPORT")

# Load Packages
library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)
library(stringr)
library(arsenal)
library(tigris)
library(sf)
library(Rfast)
library(fixest)
library(ggplot2)
library(etwfe)
library(marginaleffects)
library(modelsummary)

dict = c(
  rsu = "nRentStabUnits",
  bbl = "BBL",
  year = "Year",
  alt1_filed = "Level One Alteration Filed Or Not",
  did_alt1 = "Level One Alteration Filed Or Not",
  HPDv_all = "All HMC Violations",
  HPDv_C = "Class C HMC Violations",
  evicts = "Evictions",
  zipcode = "ZipCode",
  "0" = "No Alt1",
  "1" = "Alt1",
  ATT = "IW ATT"
)

### Import Processed Data
infdf <- fread("./ProcessedData/infdf.csv") %>%
  mutate(across(20:25, ~if_else(is.na(.x),0,.x))) %>%
  mutate(across(27:34, ~if_else(is.na(.x),0,.x))) %>%
  mutate(SWL_year = if_else(isSWL == 1, sale_year, Inf)) %>%
  mutate(mySWLb_year25 = if_else(isBot25boro == 1, sale_year, Inf)) %>%
  mutate(mySWLb_year10 = if_else(isBot10boro == 1, sale_year, Inf)) %>%
  mutate(mySWLn_year50 = if_else(isBot50nta == 1, sale_year, Inf)) %>%
  mutate(mySWLn_year25 = if_else(isBot25nta == 1, sale_year, Inf)) %>%
  mutate(mySWLn_year20 = if_else(isBot20nta == 1, sale_year, Inf)) %>%
  mutate(mySWLn_year10 = if_else(isBot10nta == 1, sale_year, Inf)) %>%
  filter(nchar(bbl) >= 9, bbl != 0, !is.na(bbl), bbl != 0000000000) %>%
  group_by(bbl) %>%
  mutate(did_alt1 = if_else(any(alt1_filed == 1) & year >= sale_year,1,0))

dofsales <- fread("./RawData/24/DOFsales10_23.csv") %>%
  clean_names() %>%
  select(-sale_date, -sale_price) %>%
  filter(!is.na(residential_units),!is.na(build_class))

infdOf <- infdf %>%
  left_join(dofsales, by = "bbl") 
  #select(-rsu) %>%
  #filter(!if_any(c(year, SWL_year, total_units, build_class),~is.na(.x))) %>%
  #mutate(build_class = as.factor(build_class))
################################################################################
### RSU SunAb Models ###

# True SWL
sa_SWL_RSU <- feglm(rsu ~ sunab(SWL_year, year
                                #, bin.rel=list("-2"=-5:-2,"0"=0:3)
                                ) | year + bbl
                    , data = infdf
                    , family = "poisson"
                    , vcov = ~bbl 
                    #,fixef.rm = "singleton"
                    )

#etable(sa_SWL_RSU, agg = "att")

# My Boro SWL 25
sa_mySWLb25_RSU <- feglm(rsu ~ sunab(mySWLb_year25, year
                                     # ,bin.rel=list("-2"=-5:-2,"0"=0:3)
                                      ) | year + bbl
                         , data = infdf
                         , family = "poisson" 
                         , vcov = ~bbl
                         #, fixef.rm = "singleton"
                         )

#etable(sa_mySWLb25_RSU,  agg = "att")

# My Boro SWL 10
sa_mySWLb10_RSU <- feglm(rsu ~ sunab(mySWLb_year10, year
                                    # ,bin.rel=list("-2"=-5:-2,"0"=0:3)
                                    ) | year + bbl
                                      , data = infdf
                                      , family = "poisson" 
                                      , vcov = ~bbl
                                      #, fixef.rm = "singleton"
                                    )

#etable(sa_mySWLb10_RSU, agg = "att")

SWLbRSU <- etable(sa_SWL_RSU,sa_mySWLb10_RSU,sa_mySWLb25_RSU, agg = "att", 
                  tex = TRUE, 
                  style.tex = style.tex('aer'),
                  dict = dict,
                  export = "/Volumes/BigBrady/UPP_PASSPORT/Tables/SWLbRSU.png")


# My NTA SWL 50
mySWLn_year50_RSU <- feglm(rsu ~ sunab(mySWLn_year50, year
                                     #  , bin.rel=list("-2"=-5:-2,"0"=0:3)
                                       )| bbl + year
                           , data = infdf
                           , family = "poisson"
                           , vcov = ~bbl
                           #, fixef.rm = "singleton"
                           )




# My NTA SWL 25
mySWLn_year25_RSU <- feglm(rsu ~ sunab(mySWLn_year25, year
                                       #  , bin.rel=list("-2"=-5:-2,"0"=0:3)
)| bbl + year
, data = infdf
, family = "poisson"
, vcov = ~bbl
#, fixef.rm = "singleton"
)

#etable(mySWLn_year25_RSU, agg = "att")

# My NTA SWL 20
mySWLn_year20_RSU <- feglm(rsu ~ sunab(mySWLn_year20, year
                                     #  , bin.rel=list("-2"=-5:-2,"0"=0:3)
                                       )| bbl + year
                           , data = infdf
                           , family = "poisson"
                           , vcov = ~bbl
                           #, fixef.rm = "singleton"
                           )

#etable(mySWLn_year20_RSU, agg = "att")

# My NTA SWL 10
mySWLn_year10_RSU <- feglm(rsu ~ sunab(mySWLn_year10, year
                                      # , bin.rel=list("-2"=-5:-2,"0"=0:3)
                                       )| bbl + year
                           , data = infdf
                           , family = "poisson"
                           , vcov = ~bbl
                           #, fixef.rm = "singleton"
                           )

#etable(mySWLn_year10_RSU, agg = "att")


SWLnRSU <- etable(mySWLn_year50_RSU, mySWLn_year25_RSU, mySWLn_year10_RSU, agg = "att", 
                  tex = TRUE, 
                  style.tex = style.tex('aer'),
                  dict = dict,
                  export = "/Volumes/BigBrady/UPP_PASSPORT/Tables/SWLnRSU.png")

################################################################################
### HPD C SunAb Models ###

# True SWL
sa_SWL_HPDv_C <- feglm(HPDv_C ~ sunab(SWL_year, year
                                     # ,bin.rel=list("-2"=-5:-2,"0"=0:3)
                                      ) | bbl + year, data = infdf
                           , family = "poisson"
                           , vcov = ~bbl, 
                           #,fixef.rm = "singleton"
                            )
                           

etable(sa_SWL_HPDv_C, agg = "att")

# My Boro SWL 25
sa_mySWLb25_HPDv_C <- feglm(HPDv_C ~ sunab(mySWLb_year25, year
                                    # ,bin.rel=list("-2"=-5:-2,"0"=0:3)
                            ) | year + bbl
                            , data = infdf
                            , family = "poisson" 
                            , vcov = ~bbl
                            #, fixef.rm = "singleton"
                            )

etable(sa_mySWLb25_HPDv_C, agg = "att")

# My Boro SWL 10
sa_mySWLb10_HPDv_C <- feglm(HPDv_C ~ sunab(mySWLb_year10, year
                                   #  ,bin.rel=list("-2"=-5:-2,"0"=0:3)
                            ) | year + bbl
                            , data = infdf
                            , family = "poisson" 
                            , vcov = ~bbl
                            , fixef.rm = "singleton"
                            )


SWLbHPDc <- etable(sa_SWL_HPDv_C, sa_mySWLb25_HPDv_C, sa_mySWLb10_HPDv_C, agg = "att",
                   tex = TRUE, 
                   style.tex = style.tex('aer'),
                   dict = dict,
                   export = "/Volumes/BigBrady/UPP_PASSPORT/Tables/SWLbHPDc.png")                                        

# My NTA SWL 50
mySWLn_year50_HPDv_C <- feglm(HPDv_C ~ sunab(mySWLn_year50, year
                                             #, bin.rel=list("-2"=-5:-2,"0"=0:3)
                                             )| bbl + year
                              , data = infdf
                              , family = "poisson"
                              , vcov = ~bbl
                              , fixef.rm = "singleton"
                              )

etable(mySWLn_year50_HPDv_C, agg = "att")



# My NTA SWL 25
mySWLn_year25_HPDv_C <- feglm(HPDv_C ~ sunab(mySWLn_year25, year
                                             #, bin.rel=list("-2"=-5:-2,"0"=0:3)
)| bbl + year
, data = infdf
, family = "poisson"
, vcov = ~bbl
, fixef.rm = "singleton"
)

#etable(mySWLn_year25_HPDv_C, agg = "att") 


# My NTA SWL 20
mySWLn_year20_HPDv_C <- feglm(HPDv_C ~ sunab(mySWLn_year20, year
                                             #, bin.rel=list("-2"=-5:-2,"0"=0:3)
                                             )| bbl + year
                              , data = infdf
                              , family = "poisson"
                              , vcov = ~bbl
                              , fixef.rm = "singleton"
                              )

#etable(mySWLn_year20_HPDv_C, agg = "att") 

# My NTA SWL 10
mySWLn_year10_HPDv_C <- feglm(HPDv_C ~ sunab(mySWLn_year10, year
                                             #, bin.rel=list("-2"=-5:-2,"0"=0:3)
                                             )| bbl + year, data = infdf
                              , family = "poisson"
                              , vcov = ~bbl,
                              #fixef.rm = "singleton"
                              )

#etable(mySWLn_year10_HPDv_C, agg = "att")

SWLnHPDc <- etable(mySWLn_year50_HPDv_C, mySWLn_year25_HPDv_C, mySWLn_year10_HPDv_C, agg = "att",
                   tex = TRUE, 
                   style.tex = style.tex('aer'),
                   dict = dict,
                   export = "/Volumes/BigBrady/UPP_PASSPORT/Tables/SWLnHPDc.png")   

SWLnHPDc

################################################################################
### HPD All SunAb Models ###

# True SWL
sa_SWL_HPDv_all <- feglm(HPDv_all ~ sunab(SWL_year, year,
                                          #,bin.rel=list("-2"=-5:-2,"0"=0:3) 
                                          ) | bbl + year
                         , data = infdf
                          , family = "poisson"
                          , vcov = ~bbl, 
                          #,fixef.rm = "singleton"
)


etable(sa_SWL_HPDv_all, agg = "att")

# My Boro SWL 25
sa_mySWLb25_HPDv_all <- feglm(HPDv_all ~ sunab(mySWLb_year25, year
                                           #,bin.rel=list("-2"=-5:-2,"0"=0:3)
                                            ) | year + bbl
                                            , data = infdf
                                            , family = "poisson" 
                                            , vcov = ~bbl
                                            #, fixef.rm = "singleton"
                              )

etable(sa_mySWLb25_HPDv_all, agg = "att")

# My Boro SWL 10
sa_mySWLb10_HPDv_all <- feglm(HPDv_all ~ sunab(mySWLb_year10, year
                                           #,bin.rel=list("-2"=-5:-2,"0"=0:3)
                                            ) | year + bbl
                                            , data = infdf
                                            , family = "poisson" 
                                            , vcov = ~bbl
                                            #, fixef.rm = "singleton"
                                            )

etable(sa_mySWLb10_HPDv_all, agg = "att")

SWLbHPDall <- etable(sa_SWL_HPDv_all, sa_mySWLb25_HPDv_all, sa_mySWLb10_HPDv_all, agg = "att",
                     tex = TRUE, 
                     style.tex = style.tex('aer'),
                     dict = dict,
                     export = "/Volumes/BigBrady/UPP_PASSPORT/Tables/SWLbHPDall.png") 


# My NTA SWL 50
mySWLn_year50_HPDv_all <- feglm(HPDv_all ~ sunab(mySWLn_year50, year
                                                 #, bin.rel=list("-2"=-5:-2,"0"=0:3)
                                                 )| bbl + year
                                , data = infdf
                                , family = "poisson"
                                , vcov = ~bbl
                                #, fixef.rm = "singleton"
                                )

etable(mySWLn_year50_HPDv_all, agg = "att")



# My NTA SWL 25
mySWLn_year25_HPDv_all <- feglm(HPDv_all ~ sunab(mySWLn_year25, year
                                                 #, bin.rel=list("-2"=-5:-2,"0"=0:3)
)| bbl + year
, data = infdf
, family = "poisson"
, vcov = ~bbl
#, fixef.rm = "singleton"
)

etable(mySWLn_year25_HPDv_all, agg = "att") 


# My NTA SWL 20 
mySWLn_year20_HPDv_all <- feglm(HPDv_all ~ sunab(mySWLn_year20, year
                                                 #, bin.rel=list("-2"=-5:-2,"0"=0:3)
                                                 )| bbl + year
                                , data = infdf
                                , family = "poisson"
                                , vcov = ~bbl
                                #, fixef.rm = "singleton"
                                )

etable(mySWLn_year20_HPDv_all, agg = "att") 

# My NTA SWL 10
mySWLn_year10_HPDv_all <- feglm(HPDv_all ~ sunab(mySWLn_year10, year
                                                 , bin.rel=list("-2"=-5:-2,"0"=0:3)
                                                 )| bbl + year
                                , data = infdf
                                , family = "poisson"
                                , vcov = ~bbl
                                #, fixef.rm = "singleton"
                                )

SWLnHPDall <- etable(mySWLn_year50_HPDv_all, mySWLn_year25_HPDv_all, mySWLn_year10_HPDv_all, agg = "att",
                   tex = TRUE, 
                   style.tex = style.tex('aer'),
                   dict = dict,
                   export = "/Volumes/BigBrady/UPP_PASSPORT/Tables/SWLnHPDall.png")   


################################################################################
### Evictions SunAb Models ###

# True SWL
sa_SWL_evicts <- feglm(evicts ~ sunab(SWL_year, year
                                      #, bin.rel=list("-2"=-5:-2,"0"=0:3)
                                      ) | bbl + year
                            , data = infdf
                            , family = "poisson"
                            , vcov = ~bbl, 
                            #,fixef.rm = "singleton"
                            )


etable(sa_SWL_evicts, agg = "att")

# My Boro SWL 25
sa_mySWLb25_evicts <- feglm(evicts ~ sunab(mySWLb_year25, year
                            #,bin.rel=list("-2"=-5:-2,"0"=0:3)
                            ) | year + bbl
                            , data = infdf
                            , family = "poisson" 
                            , vcov = ~bbl
                            #, fixef.rm = "singleton"
                            )

etable(sa_mySWLb25_evicts, agg = "att")

# My Boro SWL 10
sa_mySWLb10_evicts <- feglm(evicts ~ sunab(mySWLb_year10, year
                                            #,bin.rel=list("-2"=-5:-2,"0"=0:3)
                                            ) | year + bbl
                            , data = infdf
                            , family = "poisson" 
                            , vcov = ~bbl 
                            #, fixef.rm = "singleton"
                            )

etable(sa_mySWLb10_evicts, agg = "att")

SWLbEvicts <- etable(sa_SWL_evicts, sa_mySWLb25_evicts, sa_mySWLb10_evicts, agg = "att",
                     tex = TRUE, 
                     style.tex = style.tex('aer'),
                     dict = dict,
                     export = "/Volumes/BigBrady/UPP_PASSPORT/Tables/SWLbEvicts.png") 


# My NTA SWL 50
mySWLn_year50_evicts <- feglm(evicts ~ sunab(mySWLn_year50, year
                                             #, bin.rel=list("-2"=-5:-2,"0"=0:3)
                                             )| bbl + year
                              , data = infdf
                              , family = "poisson"
                              , vcov = ~bbl 
                              #, fixef.rm = "singleton"
                              )

etable(mySWLn_year50_evicts, agg = "att")


# My NTA SWL 25 
mySWLn_year25_evicts <- feglm(evicts ~ sunab(mySWLn_year25, year
                                             #, bin.rel=list("-2"=-5:-2,"0"=0:3)
)| bbl + year
, data = infdf
, family = "poisson"
, vcov = ~bbl
#, fixef.rm = "singleton"
)

etable(mySWLn_year25_evicts, agg = "att") # significant & positive !!!!


# My NTA SWL 20 
mySWLn_year20_evicts <- feglm(evicts ~ sunab(mySWLn_year20, year
                                             #, bin.rel=list("-2"=-5:-2,"0"=0:3)
                                             )| bbl + year
                              , data = infdf
                              , family = "poisson"
                              , vcov = ~bbl
                              #, fixef.rm = "singleton"
                              )

etable(mySWLn_year20_evicts, agg = "att") 

# My NTA SWL 10
mySWLn_year10_evicts <- feglm(evicts ~ sunab(mySWLn_year10, year
                                             #, bin.rel=list("-2"=-5:-2,"0"=0:3)
                                             )| bbl + year
                              , data = infdf
                              , family = "poisson"
                              , vcov = ~bbl
                              #, fixef.rm = "singleton"
                              )

etable(mySWLn_year10_evicts, agg = "att")

SWLnEvicts <- etable(mySWLn_year50_evicts, mySWLn_year25_evicts, mySWLn_year10_evicts, agg = "att",
                     tex = TRUE, 
                     style.tex = style.tex('aer'),
                     dict = dict,
                     export = "/Volumes/BigBrady/UPP_PASSPORT/Tables/SWLnEvicts.png") 



### Alt1's ###

# Boro Level

##### TRYING ETWFE - DOESNT SEEM TO WORK SINCE BINARY DEPENDENT (USUALLY?) NEEDS TO CONTINUE EQUALING 1 FOR ALL PERIODS AFTER IT FIRST EQUALS ONE
 
#Alt1_ETWFE_SWL_year = etwfe::etwfe(alt1_filed ~ residential_units,
#                          tvar = year,
#                          gvar = SWL_year, 
#                          data = infdOf,      
#                          family = "binomial",
#                          type = "group", # right? by treatment cohort...
#                          vcov = ~bbl) 

#fixest::etable(Alt1_ETWFE_SWL_year) 
#alt1_agg <- etwfe::emfx(Alt1_ETWFE_SWL_year)

##### BACK TO SUNAB FOR NOW - NEED TO FIGURE OUT ETWFE.... MAYBE DID PACKAGE COULD WORK TOO?

SA_Alt1_bSWL = feglm(alt1_filed ~ sunab(SWL_year, year, bin.rel= list("-2"=-5:-2, "0"=0:5)) | year + bbl,
                                     , data = infdOf
                                     , vcov = ~bbl
                                     , fixef.rm = "singleton"
                                     , family = "binomial"
                                     )
                                     
SA_Alt1_b25th = feglm(alt1_filed ~ sunab(mySWLb_year25, year, bin.rel= list("-2"=-5:-2, "0"=0:5)) | year + bbl,
                    , data = infdOf
                    , vcov = ~bbl
                    , fixef.rm = "singleton"
                    , family = "binomial"
)

SA_Alt1_b10th = feglm(alt1_filed ~ sunab(mySWLb_year10, year, bin.rel= list("-2"=-5:-2, "0"=0:5)) | year + bbl,
                     , data = infdOf
                     , vcov = ~bbl
                     , fixef.rm = "singleton"
                     , family = "binomial"
)

SWLbAlt1s <- etable(SA_Alt1_bSWL, SA_Alt1_b25th, SA_Alt1_b10th, 
                     agg = "att",
                     tex = TRUE, 
                     style.tex = style.tex('aer'),
                     dict = dict,
                     export = "/Volumes/BigBrady/UPP_PASSPORT/Tables/SWLbAlt1s.png") 

# NTA Level Models

SA_Alt1_n50 = feglm(alt1_filed ~ sunab(mySWLn_year50, year, bin.rel= list("-2"=-5:-2, "0"=0:5)) | year + bbl,
                     , data = infdOf
                     , vcov = ~bbl
                     , fixef.rm = "singleton"
                     , family = "binomial"
)

SA_Alt1_n25 = feglm(alt1_filed ~ sunab(mySWLn_year25, year, bin.rel= list("-2"=-5:-2, "0"=0:5)) | year + bbl,
                      , data = infdOf
                      , vcov = ~bbl
                      , fixef.rm = "singleton"
                      , family = "binomial"
)

SA_Alt1_n10 = feglm(alt1_filed ~ sunab(mySWLn_year10, year, bin.rel= list("-2"=-5:-2, "0"=0:5)) | year + bbl,
                      , data = infdOf
                      , vcov = ~bbl
                      , fixef.rm = "singleton"
                      , family = "binomial"
)

SWLbAlt1s <- etable(SA_Alt1_n50, SA_Alt1_n25, SA_Alt1_n10, 
                    agg = "att",
                    tex = TRUE, 
                    style.tex = style.tex('aer'),
                    dict = dict,
                    export = "/Volumes/BigBrady/UPP_PASSPORT/Tables/SWLnAlt1s.png") 



# interacting rsu and alt1

# @ boro
SA_rsu_alt1_b50 = feglm(rsu ~ sunab(SWL_year, year, 
                                    #bin.rel= list("-2"=-5:-2, "0"=0:5)
) | year + bbl,
, data = infdOf
, vcov = ~bbl
, fixef.rm = "singleton"
, family = "poisson"
, split = ~did_alt1
)

etable(SA_rsu_alt1_b50, agg= "att")

SA_rsu_alt1_b25 = feglm(rsu ~ sunab(mySWLb_year25, year, 
                                    #bin.rel= list("-2"=-5:-2, "0"=0:5)
) | year + bbl,
, data = infdOf
, vcov = ~bbl
, fixef.rm = "singleton"
, family = "poisson"
, split = ~did_alt1
)

etable(SA_rsu_alt1_b25, agg= "att")

SA_rsu_alt1_b10 = feglm(rsu ~ sunab(mySWLb_year10, year, 
                                    #bin.rel= list("-2"=-5:-2, "0"=0:5)
) | year + bbl,
, data = infdOf
, vcov = ~bbl
, fixef.rm = "singleton"
, family = "poisson"
, split = ~did_alt1
)

etable(SA_rsu_alt1_b10, agg= "att")



# @ nta
SA_rsu_alt1_n50 = feglm(rsu ~ sunab(mySWLn_year50, year, 
                                    #bin.rel= list("-2"=-5:-2, "0"=0:5)
                                    ) | year + bbl,
                    , data = infdOf
                    , vcov = ~bbl
                    , fixef.rm = "singleton"
                    , family = "poisson"
                    , split = ~did_alt1
)

etable(SA_rsu_alt1_n50, agg= "att")

SA_rsu_alt1_n25 = feglm(rsu ~ sunab(mySWLn_year25, year, 
                                    #bin.rel= list("-2"=-5:-2, "0"=0:5)
                                    ) | year + bbl,
                        , data = infdOf
                        , vcov = ~bbl
                        , fixef.rm = "singleton"
                        , family = "poisson"
                        , split = ~did_alt1
)

etable(SA_rsu_alt1_n25, agg= "att")

SA_rsu_alt1_n10 = feglm(rsu ~ sunab(mySWLn_year10, year, 
                                    #bin.rel= list("-2"=-5:-2, "0"=0:5)
                                    ) | year + bbl,
                        , data = infdOf
                        , vcov = ~bbl
                        , fixef.rm = "singleton"
                        , family = "poisson"
                        , split = ~did_alt1
)

Swl_rsu_alt1_n10 <- etable(SA_rsu_alt1_n10, agg= "att",
       tex = TRUE, 
       style.tex = style.tex('aer'),
       dict = dict,
       export = "/Volumes/BigBrady/UPP_PASSPORT/Tables/SWLn10Alt1RSUinteraction.png") 

### Demos ###

SA_demo_bSWL = feglm(demo_filed ~ sunab(SWL_year, year, bin.rel= list("-2"=-5:-2, "0"=0:5)) | year + bbl,
                     , data = infdOf
                     , vcov = ~bbl
                     , fixef.rm = "singleton"
                     , family = "binomial"
)

SA_demo_b25th = feglm(demo_filed ~ sunab(mySWLb_year25, year, bin.rel= list("-2"=-5:-2, "0"=0:5)) | year + bbl,
                      , data = infdOf
                      , vcov = ~bbl
                      , fixef.rm = "singleton"
                      , family = "binomial"
)

SA_demo_b10th = feglm(demo_filed ~ sunab(mySWLb_year10, year, bin.rel= list("-2"=-5:-2, "0"=0:5)) | year + bbl,
                      , data = infdOf
                      , vcov = ~bbl
                      , fixef.rm = "singleton"
                      , family = "binomial"
)

SWLbdemos <- etable(SA_demo_bSWL, SA_demo_b25th, SA_demo_b10th, 
                    agg = "att",
                    tex = TRUE, 
                    style.tex = style.tex('aer'),
                    dict = dict,
                    export = "/Volumes/BigBrady/UPP_PASSPORT/Tables/SWLbdemos.png") 

# NTA Level Models

SA_demo_n50 = feglm(demo_filed ~ sunab(mySWLn_year50, year, bin.rel= list("-2"=-5:-2, "0"=0:5)) | year + bbl,
                    , data = infdOf
                    , vcov = ~bbl
                    , fixef.rm = "singleton"
                    , family = "binomial"
)

SA_demo_n25 = feglm(demo_filed ~ sunab(mySWLn_year25, year, bin.rel= list("-2"=-5:-2, "0"=0:5)) | year + bbl,
                    , data = infdOf
                    , vcov = ~bbl
                    , fixef.rm = "singleton"
                    , family = "binomial"
)

SA_demo_n10 = feglm(demo_filed ~ sunab(mySWLn_year10, year, bin.rel= list("-2"=-5:-2, "0"=0:5)) | year + bbl,
                    , data = infdOf
                    , vcov = ~bbl
                    , fixef.rm = "singleton"
                    , family = "binomial"
)

SWLbdemos <- etable(SA_demo_n50, SA_demo_n25, SA_demo_n10, 
                    agg = "att",
                    tex = TRUE, 
                    style.tex = style.tex('aer'),
                    dict = dict,
                    export = "/Volumes/BigBrady/UPP_PASSPORT/Tables/SWLndemos.png") 


