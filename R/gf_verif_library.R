#' Active librairies
#' @description Importe si absente puis active les librairies.
#'
#' @author Bruciamacchie Max
#'
#' @export

gf_verif_library <- function (lib.name){
  x = require(lib.name, quietly = TRUE, character.only = TRUE)
  if (!x) {
    install.packages(lib.name, dependencies = TRUE, quiet = TRUE)
    x = require(lib.name, quietly = TRUE, character.only = TRUE)
  }
}
