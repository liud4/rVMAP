#' Derive, label, and add cardiac structure variables to the merged data set.
#'
#' @param data A data frame containing VMAC variables.
#' @return \code{data} with added cardiac structure variables.
#' @export

derive_cardiac_structure <- function(data) {
  data <- within(data, {
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

  return(data)
}
