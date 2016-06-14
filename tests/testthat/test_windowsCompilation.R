
check_windows <- function() {
  if (.Platform$OS.type != "windows") {
    msg <- sprintf("no test for available for %s (only windows)", 
                   .Platform$OS.type)
    skip(msg)
  } 
}

testthat::test_that("WTAQ-2 calculates correctly", {
  check_windows()
  
  ### Using model engine compiled from source during package installation
  wtaq.exe_src <- system.file("bin", 
                              .Platform$r_arch, 
                              "wtaq2_1.exe", 
                              package = "kwb.wtaq")
  
  ### Using pre-compiled model engine (https://water.usgs.gov/ogw/wtaq/WTAQ_2.1.exe)
  wtaq.exe_bin <- system.file("extdata", 
                              "wtaq.2.1.exe", 
                              package = "kwb.wtaq")
  
  ### Reading input file
  input <- system.file("extdata", 
                       "example1.inp", 
                       package = "kwb.wtaq")
  
  config <- wtReadInputFile(input)
  
  ### Running WTAQ
  res_src <- wtRunConfiguration(configuration = config, 
                                wtaq.exe = wtaq.exe_src )
  res_bin <- wtRunConfiguration(configuration = config,  
                                wtaq.exe = wtaq.exe_bin )
  
  
  ### Are calculated drawdowns equal?
  testthat::expect_equal(res_src$CALCDD, res_bin$CALCDD)
  
})