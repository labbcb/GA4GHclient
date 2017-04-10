context("request.get")

test_that("request.get works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    response <- request.get(host, "datasets", "WyIxa2dlbm9tZXMiXQ")
    expect_is(response, "list")
    expect_length(response, 3)
})

test_that("request.get should throw error message", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    expect_error(request.get(host, "datasets", "invalid"))

    host <- "invalid"
    expect_error(request.get(host, "datasets", "invalid"))
})
