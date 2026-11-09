# cpv2017

Consulta los microdatos del **Censo Nacional de Población y Vivienda 2017 del Perú** (INEI) con `dplyr`, sin REDATAM y sin cargar 29 millones de registros en memoria.

Los datos ya vienen convertidos a archivos Parquet (unos 900 MB, frente a 1.7 GB de la base REDATAM original) y se consultan de forma perezosa con [arrow](https://arrow.apache.org/docs/r/).

Fuente de los datos: base REDATAM del CPV 2017 del INEI, de libre acceso en el portal REDATAM de CEPAL ([CP2017PER.zip](https://prod.redatam.org/cdr/descargas/censos/poblacion/CP2017PER.zip), 679 MB).

## 1. Instalar y descargar los datos (una sola vez)

```r
# install.packages("remotes")
remotes::install_github("denis-yen/cpv2017")

library(cpv2017)
cpv_descargar()   # ~940 MB
```

Eso es todo: no necesitas la base REDATAM. Cada archivo se verifica con su firma MD5, y si la conexión se corta basta con volver a ejecutar `cpv_descargar()` (solo baja lo que falta). En las siguientes sesiones el paquete encuentra los datos solo.

## 2. Consultar

```r
library(dplyr)

# Población censada y población total estimada (con factor de ponderación)
cpv_tbl("persona") |>
  summarise(censada = n(), total = sum(factorpond)) |>
  collect()
#>    censada    total
#> 1 29381884 31237385

# Población por departamento y sexo, con nombres y etiquetas
cpv_tbl("persona") |>
  mutate(ubigeo = substr(ubigeo, 1, 2)) |>
  count(ubigeo, c5p2) |>
  collect() |>
  cpv_etiquetar(c5p2) |>
  left_join(cpv_geo("departamento"), by = "ubigeo")

# % de hogares con Internet por departamento
cpv_tbl("hogar") |>
  cpv_na(c3p213) |>
  group_by(dep = substr(ubigeo, 1, 2)) |>
  summarise(pct_internet = 100 * mean(c3p213 == 1, na.rm = TRUE)) |>
  collect()
```

## Ejemplos completos

La viñeta [`vignettes/ejemplos.Rmd`](vignettes/ejemplos.Rmd) explica paso a paso:

- **Índice de envejecimiento por distrito** y cómo guardarlo en Excel (`writexl` u `openxlsx`).
- **Un indicador que cruza vivienda, hogar y persona**: niños de 0 a 4 años en hogares que cocinan con leña o bosta y en viviendas sin agua por red pública.
- **Cálculos con el factor de ponderación**: población total, porcentajes e índices ponderados, y cómo ponderar indicadores de hogar.

Muestra breve (índice de envejecimiento por distrito → Excel):

```r
ie <- cpv_tbl("persona") |>
  cpv_na(c5p41) |>
  group_by(ubigeo) |>
  summarise(pob_0_14   = sum(c5p41 <= 14, na.rm = TRUE),
            pob_60_mas = sum(c5p41 >= 60, na.rm = TRUE)) |>
  collect() |>
  mutate(indice_envejecimiento = round(100 * pob_60_mas / pob_0_14, 1)) |>
  left_join(cpv_geo("distrito"), by = "ubigeo")

writexl::write_xlsx(ie, "indice_envejecimiento.xlsx")
```

Para ver la viñeta instalada: `remotes::install_github("denis-yen/cpv2017", build_vignettes = TRUE)` y luego `vignette("ejemplos", "cpv2017")`.

## Tablas

| Tabla | Registros | Enlaces |
|---|---|---|
| `persona` | 29,381,884 | `persona_id`, `hogar_id`, `vivienda_id`, `ubigeo` |
| `hogar` | 8,283,285 | `hogar_id`, `vivienda_id`, `ubigeo` |
| `vivienda` | 10,133,850 | `vivienda_id`, `distrito_id`, `ubigeo` |
| `distrito` / `provincia` / `departamento` | 1,874 / 196 / 25 | `ubigeo`, `nombre` |

`ubigeo` es el código del distrito (6 dígitos): `substr(ubigeo, 1, 2)` da el departamento y `substr(ubigeo, 1, 4)` la provincia.

## Funciones

| Función | Para qué sirve |
|---|---|
| `cpv_descargar()` | Descarga los datos y verifica cada archivo. |
| `cpv_convertir()` | (Avanzado) Convierte la base REDATAM a Parquet. |
| `cpv_manifiesto()` | (Avanzado) Para publicar: genera `manifiesto.csv` con tamaño y MD5 de cada archivo. |
| `cpv_tbl()` | Abre una tabla para consultarla con dplyr. |
| `cpv_variables()` | Busca variables por nombre o etiqueta (`cpv_variables("agua")`). |
| `cpv_etiquetas()` | Muestra los códigos de una variable (`cpv_etiquetas("c5p2")`). |
| `cpv_etiquetar()` | Convierte códigos en etiquetas (factores) tras `collect()`. |
| `cpv_na()` | Convierte los códigos de "no aplica" y "sin dato" en `NA`. |
| `cpv_geo()` | Tabla de ubigeos con nombres de departamento, provincia y distrito. |
| `cpv_ejemplo()` | Muestra incluida de dos distritos, para probar sin descargar (`dir = cpv_ejemplo()`). |

## Notas

- Los datos conservan los códigos originales de REDATAM. Usa `cpv_na()` antes de calcular promedios o porcentajes.
- Contar filas da la **población censada**; sumar `factorpond` estima la **población total** (con omisión censal).
- El paquete lee los archivos REDATAM con un lector propio (`.dicX`, `.ptr` y `.rbf` en formato PCK, CHR y DBL). No usa `redatam::read_redatam()`, que en esta base omite 21 variables y daña las tildes.
- **Linux:** los datos usan compresión zstd. Si `cpv_descargar()` avisa que tu `arrow` no la incluye, reinstálalo con `Sys.setenv(NOT_CRAN = "true"); install.packages("arrow")`.

## Uso avanzado

### Guardar los datos en otra carpeta

Por defecto se guardan en la carpeta de datos de usuario de R (`cpv_dir()`). Para usar otra:

```r
cpv_descargar(destino = "D:/datos/cpv2017")
```

y agrega a tu `.Renviron` (`usethis::edit_r_environ()`) la línea `CPV2017_DIR=D:/datos/cpv2017`, o usa `cpv_usar_dir("D:/datos/cpv2017")` en cada sesión.

### Convertir la base REDATAM por tu cuenta

No es necesario para usar el paquete. Sirve para verificar los datos publicados desde la fuente original, o si la descarga no estuviera disponible. Descarga y descomprime la base oficial ([CP2017PER.zip](https://prod.redatam.org/cdr/descargas/censos/poblacion/CP2017PER.zip), 679 MB) y conviértela (~3–4 minutos):

```r
cpv_convertir("ruta/a/BaseD/BaseR/CPVPER2017D.dicX", destino = "ruta/a/datos_cpv2017")
cpv_usar_dir("ruta/a/datos_cpv2017")
```

`cpv_manifiesto()` genera el `manifiesto.csv` (tamaño y MD5 de cada archivo) necesario para publicar los datos convertidos en otro lugar; `cpv_descargar(url = ...)` los descarga desde ahí.
