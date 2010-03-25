##Get the Stochiometric Martrix of sbmlobj type "Model" from rsbml package
getStoichiometricMatrix<-function(sbmlobj){
  species <- species(sbmlobj)
  reactions <- reactions(sbmlobj)
  mat <- matrix(0, length(species), length(reactions),
                dimnames = list(names(species), names(reactions)))
  setStoich <- function(refList, factor) {
    col <- rep(seq_along(reactions), sapply(refList, length))
    refs <- unlist(refList)
    row <- match(sapply(refs, species), names(species))
    ind <- row + (col-1)*length(species)
    mat[ind] <<- mat[ind] + sapply(refs, stoichiometry) * factor
  }
  setStoich(lapply(reactions, reactants), -1)
  setStoich(lapply(reactions, products), 1)
  return(mat)
}
