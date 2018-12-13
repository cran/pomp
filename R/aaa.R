.onAttach <- function (...) {
  exampleDir <- getOption("pomp.examples")
  pompExampleDir <- system.file("examples",package="pomp")
  options(pomp.examples=c(exampleDir,pompExampleDir,recursive=TRUE))
  packageStartupMessage("Welcome to pomp version 1.19!\n",
    "pomp version 2 is now available on https://kingaa.github.io/pomp/.\n",
    "This new version contains many improvements but is not backward compatible.\n",
    "To allow users time to modify their codes, 'pomp' version 2 will be provided as package 'pomp2' for a short while.\n\n",
    "Around the middle of 2019, 'pomp2' will be renamed 'pomp' version 2.1 and will be released to CRAN. ",
    "Until then, 'pomp' versions < 2 will continue to be available.\n\n",
    "See the ",sQuote("pomp version 2 upgrade guide")," at ",
    "https://kingaa.github.io/pomp/",".")
}

.onDetach <- function (...) {
  exampleDir <- getOption("pomp.examples")
  pompExampleDir <- system.file("examples",package="pomp")
  exampleDir <- exampleDir[exampleDir!=pompExampleDir]
  options(pomp.examples=exampleDir)
}
