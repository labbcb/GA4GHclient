context("request.post")

test_that("request.post works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    request <- unbox(data.frame(NA_integer_, NA_integer_))
    response <- request.post(host, "datasets/search", request)
    expect_is(response, "list")
    expect_named(response, c("datasets"))
})

test_that("request.post should throw error message", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    expect_error(request.post(host, "datasets/search", "invalid"))

    host <- "invalid"
    expect_error(request.post(host, "datasets", "invalid"))
})
