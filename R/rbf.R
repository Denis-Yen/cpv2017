# Lectores internos de los archivos de datos REDATAM (.rbf y .ptr).

# PCK: valores enteros de `nbits` bits empaquetados en palabras de 32 bits
# little-endian, leídos desde el bit más significativo. Un valor puede cruzar
# el límite entre dos palabras. Se lee por bloques de 32 * `bloque` valores
# (que siempre empiezan en un límite de palabra) para acotar la memoria.
leer_pck <- function(archivo, nbits, n, bloque = 2^15) {
  if (nbits < 1 || nbits > 31) {
    stop("PCK de ", nbits, " bits no soportado: ", archivo, call. = FALSE)
  }
  minimo <- ceiling(n * nbits / 32) * 4
  if (file.size(archivo) < minimo) {
    stop("El archivo ", basename(archivo), " tiene ", file.size(archivo),
         " bytes; se esperaban al menos ", minimo, ".", call. = FALSE)
  }
  con <- file(archivo, "rb")
  on.exit(close(con))
  pesos <- 2^((nbits - 1):0)
  bytes_bloque <- 4 * bloque * nbits
  out <- integer(n)
  pos <- 0
  while (pos < n) {
    raw <- readBin(con, "raw", bytes_bloque)
    raw <- raw[seq_len(length(raw) %/% 4 * 4)]
    k <- min(n - pos, (length(raw) * 8) %/% nbits)
    if (k == 0) stop("Fin inesperado de ", basename(archivo), call. = FALSE)
    palabras <- matrix(raw, nrow = 4)[4:1, , drop = FALSE]
    bits <- matrix(rawToBits(as.vector(palabras)), nrow = 8)[8:1, , drop = FALSE]
    bits <- as.integer(bits)[seq_len(k * nbits)]
    out[pos + seq_len(k)] <- as.integer(colSums(matrix(bits, nrow = nbits) * pesos))
    pos <- pos + k
  }
  out
}

# CHR: texto de ancho fijo codificado en Windows-1252.
leer_chr <- function(archivo, ancho, n) {
  raw <- readBin(archivo, "raw", file.size(archivo))
  if (length(raw) < ancho * n) {
    stop("El archivo ", basename(archivo), " es más corto de lo esperado.", call. = FALSE)
  }
  m <- matrix(raw[seq_len(ancho * n)], nrow = ancho)
  txt <- apply(m, 2, function(b) rawToChar(b[b != as.raw(0)]))
  trimws(iconv(txt, from = "CP1252", to = "UTF-8"))
}

# DBL: números de doble precisión (8 bytes, little-endian).
leer_dbl <- function(archivo, n) {
  x <- readBin(archivo, "double", n, size = 8, endian = "little")
  if (length(x) != n) {
    stop("El archivo ", basename(archivo), " es más corto de lo esperado.", call. = FALSE)
  }
  x
}

# PTR: enteros de 32 bits con el número acumulado de registros hijos de cada
# registro padre (n_padres + 1 valores, empezando en 0).
leer_ptr <- function(archivo) {
  x <- readBin(archivo, "integer", file.size(archivo) / 4, size = 4, endian = "little")
  if (!length(x) || x[1] != 0 || is.unsorted(x)) {
    stop("Archivo de punteros inválido: ", basename(archivo), call. = FALSE)
  }
  x
}

# Id del registro padre de cada registro hijo, a partir de un .ptr.
padre_desde_ptr <- function(ptr) {
  rep.int(seq_len(length(ptr) - 1L), diff(ptr))
}
