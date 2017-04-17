context("as.VCF")

library(VariantAnnotation)
host <- "http://1kgenomes.ga4gh.org/"
datasetId <- searchDatasets(host)$datasets$id
variantSetId <- searchVariantSets(host, datasetId, pageSize = 1)$variantSets$id
callSetIds <- searchCallSets(host, variantSetId)$callSets$id
response <- searchVariants(host, variantSetId, referenceName = "1",
    start = 15000, end = 16000, callSetIds = callSetIds)

test_that("as.VCF works", {
  output <- as.VCF(response$variants)
  info.idx <- startsWith(names(variants), "info.")
  expect_equal(as.data.frame(info(output)), response$variants[, info.idx],
    check.attributes = FALSE, check.names = FALSE)
})
