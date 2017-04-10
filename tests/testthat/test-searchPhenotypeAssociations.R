context("searchPhenotypeAssociations")

test_that("searchPhenotypeAssociations works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    id <- searchPhenotypeAssociationSets(host, datasetId)$id
    response <- searchPhenotypeAssociations(host, id, nrows = 10)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(10, 10))
})

test_that("searchPhenotypeAssociations featureIds parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    id <- searchPhenotypeAssociationSets(host, datasetId)$id
    response <- searchPhenotypeAssociations(host, id,
        featureIds = "http://ohsu.edu/cgd/67343f55")
    expect_null(NULL)
})

test_that("searchPhenotypeAssociations phenotypeIds parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    id <- searchPhenotypeAssociationSets(host, datasetId)$id
    response <- searchPhenotypeAssociations(host, id,
        phenotypeIds = "http://ohsu.edu/cgd/af773ed7")
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 10))
})
