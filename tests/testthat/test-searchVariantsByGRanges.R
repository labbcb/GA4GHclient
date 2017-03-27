context("searchVariantsByGRanges")

library(GenomicRanges)

test_that("searchVariantsByGRanges works", {
    skip_on_bioc()

    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id

    granges <- GRanges(seqnames = "1", IRanges(start = 15000, end = 16000))
    response <- searchVariantsByGRanges(host, variantSetId, granges)
    expect_is(response, "list")
    expect_length(response, 1)

    granges <- GRanges(seqnames = c("1", "2"), IRanges(start = c(15000, 10000),
        end = c(16000, 20000)))
    response <- searchVariantsByGRanges(host, variantSetId, granges)
    expect_is(response, "list")
    expect_length(response, 2)
})
