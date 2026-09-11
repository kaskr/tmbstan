if (requireNamespace("tinytest", quietly=TRUE)) {
    tinytest::test_package("tmbstan", ncpu=getOption("Ncpus", 1))
}
