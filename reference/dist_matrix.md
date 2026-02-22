# US state distance matrix

A 56 × 56 matrix of pairwise geographic distances (in metres) between US
states and territories, computed from boundary geometries via
`sf::st_distance`.

## Usage

``` r
dist_matrix
```

## Format

A numeric matrix with 56 rows and 56 columns. Row and column names are
state/territory names.

## Source

Computed from US Census TIGER/Line shapefiles using the tigris, sf, and
geosphere packages. See Walker (2024), Pebesma (2018), Hijmans (2022).
