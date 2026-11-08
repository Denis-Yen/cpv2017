# cpv2017 0.1.0

* Primera versión.
* `cpv_descargar()` descarga los datos del censo ya convertidos a Parquet y verifica cada archivo con su firma MD5.
* `cpv_convertir()` convierte la base REDATAM original con un lector propio de los formatos `.dicX`, `.ptr` y `.rbf`.
* `cpv_tbl()`, `cpv_variables()`, `cpv_etiquetas()`, `cpv_na()`, `cpv_etiquetar()` y `cpv_geo()` para consultar los datos con dplyr.
* `cpv_ejemplo()`: muestra de dos distritos incluida en el paquete.
* Viñeta con ejemplos: índice de envejecimiento, cruce de entidades y factor de ponderación.
