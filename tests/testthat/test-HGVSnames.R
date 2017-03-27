context("HGVSnames")

test_that("Substitution works", {
    output <- HGVSnames(start = 45576, ref = "A", alt = "C")
    expect_equal(output, "g.45576A>C")
    output <- HGVSnames(start = "88+1", ref = "G", alt = "T", type = "c")
    expect_equal(output, "c.88+1G>T")
})

test_that("Deletion / insertion (indel) works", {
    output <- HGVSnames(start = 6775, ref = "T", alt = "GA")
    expect_equal(output, "g.6775delinsGA")
    output <- HGVSnames(start = 6775, ref = "TCA", alt = "C")
    expect_equal(output, "g.6775_6777delinsC")
    output <- HGVSnames(start = 145, ref = "CGA", alt = "TGG", type = "c")
    expect_equal(output, "c.145_147delinsTGG")
    output <- HGVSnames(start = 9002, ref = "AAAAAAAA", alt = "TTT")
    expect_equal(output, "g.9002_9009delinsTTT")
    output <- HGVSnames(start = 4, ref = "GC", alt = "TG")
    expect_equal(output, "g.4_5delinsTG")
})

test_that("seqnames parameter works", {
    output <- HGVSnames(start = 12345611, ref = "G", alt = "A", seqnames = "chr11")
    expect_equal(output, "chr11:g.12345611G>A")
})

test_that("Vectorization works", {
    start <- c(45576, "88+1", 6775, 6775, 145, 9002, 4, 12345611)
    ref <- c("A", "G", "T", "TCA", "CGA", "AAAAAAAA", "GC", "G")
    alt <- c("C", "T", "GA", "C", "TGG", "TTT", "TG", "A")
    type <- c("g", "c", "g", "g", "c", "g", "g", "g")
    seqnames <- c("", "", NA, NA, NA, NA, NA, "chr11")
    output <- HGVSnames(start, ref, alt, type, seqnames)
    expect_equal(output, c("g.45576A>C", "c.88+1G>T", "g.6775delinsGA",
        "g.6775_6777delinsC", "c.145_147delinsTGG", "g.9002_9009delinsTTT",
        "g.4_5delinsTG", "chr11:g.12345611G>A"))
})

test_that("alt paramter with * works", {
    output <- HGVSnames(start = 58347698, ref = "A", alt = "*")
    expect_equal(output, "g.58347698del")
})
