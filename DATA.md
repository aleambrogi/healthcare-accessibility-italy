# Included data

All included tables are aggregate research outputs or facility-level source/derived tables supplied by the project author; they contain no patient-level records.

| File | Geographic unit / contents |
|---|---|
| `stress_comuni.csv` | 7,903 municipalities, keyed by `PRO_COM` |
| `stress_province.csv` | 107 provinces, keyed by `COD_PROV` |
| `stress_regioni.csv` | 20 regions, keyed by `COD_REG` |
| `ps_definitivi*.csv`, `ps_stress.csv` | Emergency-care facility characteristics, catchment population and derived stress |
| `mixed_model_dataset.csv` | Municipal accessibility scores joined to socioeconomic covariates |

Common score fields: `total_index_avg` (saved composite score), `stress_SSN_avg` (healthcare-stress component), `mobility_index_avg` (mobility component). Municipality outputs also include travel-time variables and aggregate population fields. Field names and numerical values are retained from the source files. Higher scores reflect greater burden or disadvantage in the saved study workflow; scores across geographic levels should not be presumed directly interchangeable.

The mixed-model CSV had an additional whole-row quoting layer. This copy removes that layer to make it readable with standard CSV tools; numerical values and field contents are preserved.

Raw routing matrices, complete grids, GeoPackages, temporary R sessions and third-party reports are excluded. Original source releases, coordinate reference systems, travel-time units and aggregation choices must be recovered from the upstream workflow before recomputing the final study or combining it with other releases. This portfolio does not silently assign units or thresholds absent from the source metadata.
