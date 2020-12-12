#' Functional Traits of Frugivorous Birds along a Tropical Gradient
#'
#' A dataset containing some functional traits of frugivorous birds in the Manú
#' biosphere reserve, southeast Peru. Given are species mean trait values.
#' The row names of the dataset give species names. Morphological
#' traits have been measured on museum specimen following Eck et al.(2011).
#' Traits have been measured only for adult and, if possible, for a minimum of
#' two female and two male specimens. Body mass was taken
#' from Dunning et al. (2007).
#'
#' @format A data frame with 217 rows and 4 variables:
#' \describe{
#'   \item{Bill.width..mm.}{bill width, in mm}
#'   \item{Bill.length..mm.}{bill length, in mm}
#'   \item{Kipp.s.index}{Kipp's index indicating wing Pointedness}
#'   \item{Bodymass..g.}{adult's bodymass, in g}
#' }
#' @source Nowak, Larissa et al. (2019), Data from: Projecting consequences of
#' global warming for the functional diversity of fleshy-fruited plants and
#' frugivorous birds along a tropical elevational gradient, Dryad, Dataset,
#' \doi{10.5061/dryad.c0n737b}
"traits_birds"

#' Functional Traits of Fleshy-fruit plants along a Tropical Gradient
#'
#' Taxonomy and functional traits of 392 fleshy-fruited plant species from
#' the Manu National Park in south-east Peru. Given are fruit length and
#' width (mm), plant height (m) and crop mass (g). Fruit traits have been
#' measured on fresh fruit samples. Number of fruits per plant
#' (used to determine the crop mass) and plant height have been estimated
#' in the field. Species names are indicated as
#' row names.
#'
#' @format A data frame with 392 rows and 4 variables:
#' \describe{
#'   \item{Fruit.length..mm.}{fruit length, in mm}
#'   \item{Fruit.width..mm.}{fruit width, in mm}
#'   \item{Plant.height..m.}{plant height, in m}
#'   \item{Crop.mass..g.}{seed mass, in g}
#' }
#' @source Nowak, Larissa et al. (2019), Data from: Projecting consequences of
#' global warming for the functional diversity of fleshy-fruited plants and
#' frugivorous birds along a tropical elevational gradient, Dryad, Dataset,
#' \doi{10.5061/dryad.c0n737b}
"traits_plants"

#' Site-species matrix of birds along a Tropical Gradient
#'
#' Presences and absences of birds at different elevations along a tropical
#' gradient. Species names are indicated as row names.
#'
#' @format A matrix with 217 rows and 8 columns:
#' \describe{
#'   \item{elev_250}{is species present at 250 m elevation? 0=No, 1=Yes}
#'   \item{elev_500}{is species present at 500 m elevation? 0=No, 1=Yes}
#'   \item{elev_1000}{is species present at 1000 m elevation? 0=No, 1=Yes}
#'   \item{elev_1500}{is species present at 1500 m elevation? 0=No, 1=Yes}
#'   \item{elev_2000}{is species present at 2000 m elevation? 0=No, 1=Yes}
#'   \item{elev_2500}{is species present at 2500 m elevation? 0=No, 1=Yes}
#'   \item{elev_3000}{is species present at 3000 m elevation? 0=No, 1=Yes}
#'   \item{elev_3500}{is species present at 3500 m elevation? 0=No, 1=Yes}
#' }
#' @source Nowak, Larissa et al. (2019), Data from: Projecting consequences of
#' global warming for the functional diversity of fleshy-fruited plants and
#' frugivorous birds along a tropical elevational gradient, Dryad, Dataset,
#' \doi{10.5061/dryad.c0n737b}
"site_sp_birds"

#' Site-species matrix of plants along a Tropical Gradient
#'
#' Presences and absences of plants at different elevations along a tropical
#' gradient. Species names are indicated as row names.
#'
#' @format A matrix with 392 rows and 1° columns:
#' \describe{
#'   \item{elev_250}{is species present at 250 m elevation? 0=No, 1=Yes}
#'   \item{elev_500}{is species present at 500 m elevation? 0=No, 1=Yes}
#'   \item{elev_1000}{is species present at 1000 m elevation? 0=No, 1=Yes}
#'   \item{elev_1500}{is species present at 1500 m elevation? 0=No, 1=Yes}
#'   \item{elev_2000}{is species present at 2000 m elevation? 0=No, 1=Yes}
#'   \item{elev_2500}{is species present at 2500 m elevation? 0=No, 1=Yes}
#'   \item{elev_3000}{is species present at 3000 m elevation? 0=No, 1=Yes}
#'   \item{elev_3500}{is species present at 3500 m elevation? 0=No, 1=Yes}
#'   \item{elev_3750}{is species present at 3750 m elevation? 0=No, 1=Yes}
#'   \item{elev_4000}{is species present at 4000 m elevation? 0=No, 1=Yes}
#' }
#' @source Nowak, Larissa et al. (2019), Data from: Projecting consequences of
#' global warming for the functional diversity of fleshy-fruited plants and
#' frugivorous birds along a tropical elevational gradient, Dryad, Dataset,
#' \doi{10.5061/dryad.c0n737b}
"site_sp_plants"
