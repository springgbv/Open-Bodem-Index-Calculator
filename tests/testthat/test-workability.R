test_that("calc_workability works", {
  expect_equal(
    calc_workability(
      A_CLAY_MI = c(15.6,13.6, 4.3, 22.6, 1.9, 2.9, 3.1, 4.3, 15.6, 1.9),
      A_SILT_MI = c(16.7,30.5, 11.8, 36.6, 9.2, 8.6, 10.6, 11.8, 16.7, 9.2),
      B_LU_BRP = c(233, 234, 236, 256, 259, 265, 265, 317, 2014, 259),
      B_SOILTYPE_AGR = c('zeeklei','zeeklei', 'dekzand','zeeklei', 'dekzand', 'dekzand', 'veen', 'dekzand', 'zeeklei', 'maasklei'),
      B_GWL_GLG = c(173,139, 106, 144, 115, 113, 42, 106, 173, 115),
      B_GWL_GHG = c(21, 18, 62, 70, 49, 81, 9, 62, 21, 49),
      B_GWL_ZCRIT = c(400, 400, 400, 400, 400, 400, 400, 400, 400, 400)
    ),
    expected = c(0.57,0.45,0.54,0.84,0.80, 1, 0.00,0.76,0.66, 0.66),
    tolerance = .01
  )
})

test_that("ind_workability works", {
  expect_equal(
    ind_workability(
      D_WO = c(0, 0.75, 1, 0, 0.5, 1),
      B_LU_BRP = c(256, 256, 256, 265, 265, 265)
    ),
    expected = c(0, 0.5, 1, 0, 0.5, 1),
    tolerance = .015
  )
})

test_that('diffrenttiation maize on sand vs mais on clay workabilty season works',{
  expect_true(calc_workability(
    A_CLAY_MI = c( 1.9),
    A_SILT_MI = c(9.2),
    B_LU_BRP = c(259),
    B_SOILTYPE_AGR = c( 'dekzand'),
    B_GWL_GLG = c(115),
    B_GWL_GHG = c(49),
    B_GWL_ZCRIT = c(400)
  )>
    calc_workability(
      A_CLAY_MI = c( 1.9),
      A_SILT_MI = c(9.2),
      B_LU_BRP = c(259),
      B_SOILTYPE_AGR = c( 'zeeklei'),
      B_GWL_GLG = c(115),
      B_GWL_GHG = c(49),
      B_GWL_ZCRIT = c(400)
    ))
})
