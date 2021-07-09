# Changelog OBIC
## Version 1.1.3.9000 2021-06-15
### Added
* vignette describing how workability is calculated and affected by its input variables

### Changed
* renamed B_Z_TWO to B_GWL_ZCRIT

### Fixed
* improved function description of ind_workability
* removed negative values from season.obic
* ensured total_days >= req_days_pre_glg + req_days_post_glg in season.obic 


## Version 1.1.2 2021-06-15
### Changed
* increased required days post GLG for maize on sand or loess to match October 1
* increased required days post GLG for maize on other soils to match October 20
* season.obic can now be merged on both landuse and soiltype.m

## Version 1.1.1 2021-05-21
### Added
* table nema.crop.rot.obic added
* ppr_bouwplan_tables.r in dev/scripts
* merge_nema_tables.r in dev/scripts

## Version 1.1.0 2021-05-03
### Added
* argument B_GWL_GHG, B_GWL_GLG and B_Z_TWO to obic_field and obic_field_dt
* function calc_workability is active now

### Changed
* updated documentation, weight.obic for workability
* update calc_workability
* update column_descriptons_obic.Rdata

## Version 1.0.0 2021-04-28
### Added
* column crops_season to crops.obic
* column scientific_names to crops.obic
* preparation crops.obic in dev
* function format_gwt to reformat input for groundwater table 
* function format_soilcompaction to reformat input for subsoil compaction
* season.obic added as data.table
* new function and tests for workability indicator
* nema.obic added as data.table
* new function and tests for nematode indicator
* preparation nema.obic in dev
* preparation season.obic in dev
* obic_field is added to run obic score for one field
* ppr_column_description plus column_descriptons_obic.Rdata
* ppr_maatregel plus recom_obic.Rdata
* ppr_weights plus weights.obic
* I_P_DS and I_P_WS added (drought and wetness stress separately)
* add_management added to estimate default values for measures when not given 
* set of eight I_M_XX indicators for measures are added
* set of nine I_X_BCS indicators are added (optional)
* new M_codes added: M_LIME, M_NONINVTILL, M_SSPM, M_SOLIDMANURE,M_STRAWRESIDUE, M_MECHWEEDS, M_PESTICIDES_DST, issue #107
* obic_field_dt to run obic_field() for a data.table input 

### Changed
* calculation PBI updated for grassland
* PBI input is flexible for both grassland and arable
* calculation SLV updated
* update element_names for A_P_CC, A_P_AL in phosphate_availability.R
* update element_names for A_S_RT, A_SOM_LOI, B_AER_CBS, B_SOILTYPE_AGR in sulpher.R
* update element_names for B_GWL_CLASS in wateravailability.R
* update element_names for B_GWL_CLASS and B_AER_CBS in nretention.R
* correct weighing for S_C_A
* all element names are updated to english ones
* weighing for score calculated is changed into a more generic approach
* structure for running obi is changed into one function
* weights.obic is extended and used to distinguish for relevant/non-relevant indicators
* grass_age function is updated: count actual years
* BCS is optional input, and when given it overwrite risk indicators compaction / aggregate stability
* M codes for no advice, issue #62
* directory dev is updated and cleaned, issue #104
* lower and upper limits for soil properties updated
* allow unknown B_HELP_WENR in checkmate water_availability, issue 65
* very low Mg indicator values for maize are changed, plus tests updated
* very low Mg indicator values for grassland on clay and peat, issue 84
* evaluation and calculation for Cu are updated

### Fixed
* setorder in winderodibility


## Version 0.11.1 2020-09-02
### Changed
* The water holding capacity is now between 0 and 1 (as theta should be) and uses new evaluation function #67
* Lower range of `D_NLV` is set to -30
* Upper range of `D_MG` is set to 1000
* Range of `D_P_DU` is set between 0 and 1

### Fixed
* Fixed the use of `B_LG_CBS` at sulphur
* `ID` can now also be character and does not have to be numeric #66


## Version 0.11.0 2020-01-16
### Added
* Adds two other pedotransfer functions (PTFs) to calculate water retention parameters #53

### Changed
* Optimal pH values of grassland are now the middle value of the class `good` #55
* pF of field capacity is changed from 2.2 to 2.0 #53
* Bulk density in the default PTF is now aggregated into 1 continous function #53
* Parameters of evaluation for PAW are adjusted #53
* Parameters for the evaluation of sealing are adjusted #57
* Changes the formula to calculate nitrogen leaching and run-off #58
* Parameters for the evaluation of nretention are adjusted #58
* Paramaters and funs for CEC are updated and split for soil fertility and aggregate stability #59
* Parameters for the evaluation of Cu availability are adjusted #59
* potassium index is multiplied so that the evaluation is equal for grass and maize #60

### Fixed
* Improved the function for crumbleability to match the factsheet #54 #56
* Fix error in calc_potassium_availability #59

## Version 0.10.0 2019-12-13
### Added
* Adds the evaluation of measures #37
* Adds giving recommendations based on the OBI score #37
* A relative score per reference group is added #31
* Adds the leaching of nitrogen to surface water and groundwater #17
* Adds a table with the description of the columns and a vignette about it #46
* Adds scoring for environmental performance #31

### Changed
* Update of crumbeability groups in `crops.obic`
* Maximum value of `A_MN_CC` is increased from 30000 to 250000
* The code for evaluating  in `ind_crumbeability` is simplified
* M_M10 user input changed in `D_CP_RUST` input, plus comments added #29
* new table with weighing factors included 'weight.obic' #32
* weighing factors updated in obic_indicators.R #32
* Improved the zinc indicator function #39
* The evaluation function for sulphur on maize and grass is updated #40
* The management function is improved, especially for maize #38
* The maximum score of `ind_management` is adjusted #38
* The parameter values for sulphur on arable land is adjusted #41
* The optimal pH for grassland depends now on organic matter content and presence of clover #42
* The score aggragation is now 60% average of last 4 years and 40% of older years #31
* The maximum value for `D_CR` is limited to 10 #47
* The parameters for the evaluation for `I_C_K` is set to the same as grassland #51
* The evaluation for `I_C_N` is now split into grassland and arable land #50
* Set lifecyle status to `maturing`
* The evaluation of `I_C_MG` is adjusted according to factsheet

### Fixed
* Preprocessing, indicators and scores make use of internal copy of the table now rather than referencing to an object outside the scope of the function
* Fixed the droughtstress, wetnessstress and waterstress values for GtI in `waterstress.obic`
* For grasslands on peat pr clay, the `D_MG` is now scaled #51
* Limit `D_SLV` between -30 and 250 

## Version 0.9.0 2019-10-22
### Changed
* The uppper limit for `D_BCS` is increased from 40 to 50
* Switch on crumbleability
* For `calc_phosphate_availability` the category `arable` is added for the crop categories
* Changed evaluation of sulphur for arable fields #26

### Fixed
* Fixed typo if mais in `ind_managment`
* Fixed test for winderodibility
* Use the correct correction factor in `calc_sealing_risk` #19
* Fix for calculating `I_P_CEC` #24
* Fix for calculating difficult values in `calc_sombalance` #25
* Fix for `calc_sbal_arable` where combinations of soil type and region that do not exist in table 6.2 of Handboek Bodem & Bemesting gave a NA #26
* Fix for `calc_bcs` where a value of 1 is given instead of 0 for `bcs_om` #28

## Version 0.8.0 2019-08-02
### Added
* Adds index and evaluation of the BodemConditieScore
* Adds potassium to main functions
* Adds a random recommondation to the output

### Changed
* The upper limit for `A_MN_CC` is increased from 20000 to 30000
* The upper limit for `A_CA_CEC` is increased from 100 to  400

### Fixed
* The indicator of pH had the wrong direction 
* Removed the minus sign before `ind_zinc`
* The function `calc_potassium_availability` containted a wrong column name for `A_PH_CC`
* Some unknown situations at `calc_potassium_availability` have now the same values as comparable situations. This should be looked at later

## Version 0.7.0 2019-08-02
### Added
* Adds index and evaluation of waterretention paramaters
* Adds index and evaluation of water related yield stress
* Adds index and evaluation of metals (Zn and Cu)
* Adds index and evaluation of microbial activity (PMN)
* Adds index and evaluation of CEC for soil fertility and structure
* Adds waterstress.obic table with information of HELP-tables

### Changed
* Change crops.obic with crop category related to water stress


## Version 0.6.0 2019-08-01
### Added
* Adds index and evaluation of K-availability

## Version 0.5.0 2019-07-31
### Added
* Adds crop classification (sensitivity to P, K and S supply)
* Adds soil organic matter balance (simple one)
* Adds risk for soil compaction (following risk map Van den Akker, 2013)
* Adds index and evaluation of Mg-availability
* Adds index and evaluation of S-availability (evaluation function need still to be parameterized)
* Adds index and evaluation of winderodibility
* Adds index and evaluation of soil strategic management (following Label Duurzaam Bodembeheer)
* Adds columns `crop_eos` and `crop_eos_residue` in `crops.obic`

## Version 0.4.0 2019-07-26
### Added
* The existing functions for pH, P, N, crumbleability, sealing and resistance are intergrated into `obic()`

### Changed
* The column name `brp` is changed to `B_LU_BRP`

## Version 0.3.0 2019-07-24
### Added
* Adds main function that wraps all the functions needed to calculated the OBI score and recommendations
* Adds documentation website for package using `pkgdown` #3

## Version 0.2.0 2019-07-12
### Added
* Adds evaluation of pH #1
* Adds evaluation of nitrogen #2

## Version 0.1.0 2019-07-08
First version of OBIC