test_that("leer_pck recupera valores de distintos anchos de bits", {
  set.seed(1)
  for (nbits in c(1, 2, 3, 7, 13, 18, 20)) {
    n <- 1000
    x <- sample(0:(2^nbits - 1), n, replace = TRUE)
    f <- withr_tempfile()
    escribir_pck(x, nbits, f)
    expect_identical(leer_pck(f, nbits, n), as.integer(x), info = paste(nbits, "bits"))
    # Bloques pequeños: obliga a leer el archivo en varias partes.
    expect_identical(leer_pck(f, nbits, n, bloque = 1), as.integer(x), info = paste(nbits, "bits, por bloques"))
  }
})

test_that("leer_pck avisa si el archivo es demasiado corto", {
  f <- withr_tempfile()
  escribir_pck(1:10, 4, f)
  expect_error(leer_pck(f, 4, 100), "se esperaban")
})

test_that("leer_chr convierte Windows-1252 a UTF-8", {
  f <- withr_tempfile()
  escribir_chr(c("Áncash", "Apurímac", "Junín"), 12, f)
  expect_identical(leer_chr(f, 12, 3), c("Áncash", "Apurímac", "Junín"))
})

test_that("padre_desde_ptr reconstruye los enlaces, incluso con padres sin hijos", {
  expect_identical(padre_desde_ptr(c(0L, 2L, 2L, 5L)), c(1L, 1L, 3L, 3L, 3L))
})
