library(devtools)

install_github(repo='tfojo1/jheem')

install_github(repo='tfojo1/distributions')

install_github(repo='tfojo1/bayesian.simulations')

# https://github.com/r-dbi/RPostgres/issues/291
install.packages("RPostgres", repos="https://r-dbi.r-universe.dev")
