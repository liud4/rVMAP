# 18 February 2016, JN
## Divided echo.co.calc by 1000
## echo.cardiac.index no longer needs to be divided by 1000

# 05 Dec 2016, LS:
#  renaming function to match filename, which had been changed.

cardiacStructure <- function(dat){
  dat = within(dat,{
    echo.ef.calc <- (echo.edv - echo.esv)/echo.edv * 100
    echo.co.calc <- 3.14159*((echo.lvot/2)^2)*echo.lvot.vti*echo.hrate/1000
    echo.cardiac.index <- echo.co.calc/bsa
    
    echo.lv.stroke.volume <- 3.14159 * ((echo.lvot/2)^2)  * echo.lvot.vti 
    label(echo.lv.stroke.volume) <- "echo.lv.stroke.volume"
    echo.myocardial.contraction.fraction <- (echo.lv.stroke.volume) / (echo.lvmass * 1.05)
    label(echo.myocardial.contraction.fraction) <- "echo.myocardial.contraction.fraction"
  
    label(echo.ef.calc) <- "Ejection Fraction (from echo)"
    label(echo.co.calc) <- "Cardiac Output (from echo)"
    label(echo.cardiac.index) <- "Cardiac Index (from echo)"
  })
  dat
}
