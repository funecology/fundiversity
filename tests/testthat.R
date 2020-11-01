library(testthat)
library(fundiversity)

data("fd_tussock")

full_tussock = fd_tussock$trait[complete.cases(fd_tussock$trait[, c("height", "LDMC"), drop = FALSE]),
                                c("height", "LDMC"), drop = FALSE]

fd_indices = FD::dbFD(full_tussock, fd_tussock$abun[, row.names(full_tussock)])

test_check("fundiversity")


