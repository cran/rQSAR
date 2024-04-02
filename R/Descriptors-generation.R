#' Generate Molecular Descriptors from SDF File
#'
#' This function reads an SDF (Structure Data File) containing molecular structures and calculates molecular descriptors for each molecule.
#'
#' @importFrom rcdk load.molecules get.desc.names eval.desc
#' @param sdf_file Path to the SDF file.
#' @return A matrix containing molecular descriptors for each molecule in the SDF file.
#' @export
generate_descriptors_from_sdf <- function(sdf_file) {
  # Read SDF file
  molecules <- load.molecules(sdf_file)

  # Calculate molecular descriptors
  dn <- get.desc.names()
  descriptors <- eval.desc(molecules, dn)

  return(descriptors)
}
