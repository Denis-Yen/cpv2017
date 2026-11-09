test_that("sin zstd ni snappy se escribe sin compresión y se lee igual", {
  local_mocked_bindings(codec_is_available = function(type) FALSE, .package = "arrow")
  expect_identical(compresion_parquet(), "uncompressed")
  f <- tempfile(fileext = ".parquet")
  escribir_parquet(data.frame(x = 1:3), f)
  expect_identical(arrow::read_parquet(f)$x, 1:3)
})

test_that("cpv_descargar avisa si arrow no trae zstd", {
  local_mocked_bindings(codec_is_available = function(type) FALSE, .package = "arrow")
  expect_error(cpv_descargar(tempdir(), url = URL_DATOS), "NOT_CRAN")
})
