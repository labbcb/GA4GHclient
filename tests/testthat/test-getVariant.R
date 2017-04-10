context("getVariant")

test_that("getVariant works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    variantId <- searchVariants(host, variantSetId, referenceName = "1",
        start = 15000, end = 16000, nrows = 1, asVCF = FALSE)$id
    variant <- getVariant(host, variantId)
    expect_s4_class(variant, "VCF")
})

test_that("getVariant asVCF=FALSE works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    variantId <- searchVariants(host, variantSetId, referenceName = "1",
        start = 15000, end = 16000, nrows = 1, asVCF = FALSE)$id
    variant <- getVariant(host, variantId, asVCF = FALSE)
    expect_s4_class(variant, "DataFrame")
    expect_equal(ncol(variant), 24)
})

test_that("getVariant multiple alt bases works", {
    skip_on_bioc()
    library(VariantAnnotation)
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    variantId <- searchVariants(host, variantSetId, referenceName = "1",
        start = 15274, end = 15274, nrows = 1, asVCF = FALSE)$id
    variant.vcf <- getVariant(host, variantId)
    expect_equal(alt(variant.vcf), CharacterList("G", "T"))
    variant.df <- getVariant(host, variantId, asVCF = FALSE)
    expect_equal(dim(variant.df), c(2, 24))
})
