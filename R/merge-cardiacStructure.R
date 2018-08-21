cardiacStructure <- function(dat) {
  dat <- within(dat, {
    echo.ef.calc <- (echo.edv - echo.esv)/echo.edv * 100
    label(echo.ef.calc) <- "Ejection Fraction (from echo)"

    echo.co.calc <- 3.14159*((echo.lvot/2)^2)*echo.lvot.vti*echo.hrate/1000
    label(echo.co.calc) <- "Cardiac Output (from echo)"

    echo.cardiac.index <- echo.co.calc/bsa
    label(echo.cardiac.index) <- "Cardiac Index (from echo)"

    echo.lv.stroke.volume <- 3.14159 * ((echo.lvot/2)^2)  * echo.lvot.vti
    label(echo.lv.stroke.volume) <- "echo.lv.stroke.volume"

    echo.myocardial.contraction.fraction <- (echo.lv.stroke.volume) / (echo.lvmass * 1.05)
    label(echo.myocardial.contraction.fraction) <- "echo.myocardial.contraction.fraction"
  })
  dat
}
