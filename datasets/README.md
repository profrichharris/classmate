# Classmate online datasets

Data offered in Classmate's **Load Package Data** window under *Classmate (online datasets)*. The
files are kept here in their original formats and are **not** part of the R package (the folder is
excluded in `.Rbuildignore`). Each file carries its own licence: the MIT licence of the Classmate
code does not apply to them.

`datasets.json` describes every dataset: title, description, file, format, size, SHA-256 checksum,
source, date retrieved, licence (and its link), the credit wording required, any notice that must
accompany it, and the date the licence was reviewed. Classmate shows this beside each dataset and
adds the credit and licence as comments to the code log when the data are downloaded.

## Adding a dataset

1. Check the licence yourself: the data must be free to redistribute (public domain, CC0, CC-BY, or
   ODbL with its share-alike wording in `notice`, OGL, or published free to download with no formal licence, `free-with-credit`, which needs a `notice` saying so). `tools/prepare_childhood_lottery.R` shows how the Childhood Lottery files were built. Prefer plain formats (`.gpkg`, `.geojson`,
   `.csv`); avoid `.rds` / `.RData`. Name the geometry column `geometry` (write GeoPackages with
   `layer_options = "GEOMETRY_NAME=geometry"`): sf's default `geom` makes `st_area(geometry)` find R's
   `geometry()` function instead of the column.
2. Put the file in this folder and add an entry to `datasets.json` (copy an existing one). Fill in
   every field; set `reviewed` to the date you checked the licence.
3. Run `Rscript tools/check_datasets.R --update` from the package root to fill in the size and
   checksum, then `Rscript tools/check_datasets.R` to confirm everything passes. The check covers
   the paperwork (fields present, licence on the approved list, files and checksums current); it
   cannot decide whether a licence really allows redistribution.
4. Commit and push. The files are fetched from
   `https://raw.githubusercontent.com/profrichharris/classmate/main/datasets/`.

## Current datasets

| File | Source | Licence |
|------|--------|---------|
| `chicago_tracts_income.gpkg` | US Census Bureau, ACS 2018-2022 5-year estimates (B19013) with 2022 TIGER/Line tracts, Chicago | Public domain (US federal government work) |
| `chicago_boundary.gpkg` | US Census Bureau, 2022 TIGER/Line cartographic boundary file (places), Chicago | Public domain (US federal government work) |
| `childhood_lottery.gpkg`, `childhood_lottery_metrics.csv` | Children's Commissioner for England, The Childhood Lottery (2026); ONS Dec 2025 UTLA boundaries | Free to download, credit required (no formal licence stated); boundaries OGL v3.0 |

Not yet included: the Rogers Park residential building outlines (OpenStreetMap, ODbL): they need the
"(c) OpenStreetMap contributors" credit and share-alike notice written up first.
