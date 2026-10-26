# Reproducibility

## Available now

`python scripts/summarise_accessibility.py` reads the included processed outputs, verifies unique geographic identifiers and writes missingness/descriptive summaries. This is a data audit, not independent reproduction of the research findings.

## Original workflow

The notebooks and R scripts are retained as an explicitly separate research archive. They depend on the original `Scripts/DATASET_DIST` layout, large travel-time and grid tables, geospatial data and additional Python/R packages. Notebook outputs were cleared, but code and methodological variants were preserved. Configure paths before running; do not execute the archive as a single ordered pipeline.

The regression scripts include exploratory fitting and diagnostics; they are not presented as a polished final model. For example, the archived distribution-fit block requests a lognormal fit but later reads beta shape parameters. Resolve that research variant against the intended analysis before rerunning it. The portable quick start does not invoke it.

The relation between notebook index variants and the under-review manuscript must be checked against the manuscript's final specification. Manuscript acceptance, publication and full end-to-end replication are not claimed here.

## Portfolio checks (26 October 2026)

The aggregate audit executed successfully for all three geographic levels. At municipality level, 7 total-index values, 7 healthcare-stress values and 4 mobility values are missing; these remain missing. The mixed-model table is readable as 7,939 rows and 66 fields after removing the extra quoting layer from affected rows. All four included R files parse; all five notebook code sources pass Python syntax checks. Routing and model fitting were not executed.
