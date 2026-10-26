# Healthcare Accessibility in Italy

**Geospatial data integration, travel-time analysis and municipal indicators of healthcare accessibility.**

Research material from the GRINS healthcare-accessibility project, combining population data, emergency-care facilities, travel times and territorial covariates. The repository makes the analysis workflow and selected aggregate outputs accessible without requiring the original multi-gigabyte working directory.

## Research context

Related manuscript: **“A national high-resolution, non-compensatory index of healthcare accessibility in Italy” — under review.**

Authors: **M. Greco, A. Ambrogi, M. Alborghetti, L. Cerlini, R. Besana, F. Colombo, F. Ieva and P. Secchi**. This is collaborative work; the repository does not attribute the full study to a single contributor. The files are a research snapshot and should not be assumed to be the definitive manuscript replication package.

## Workflow and methods

- Integrate facility, population and geographic information.
- Estimate and analyse travel times to emergency departments and transport infrastructure.
- Construct and aggregate accessibility and healthcare-stress indicators.
- Examine territorial associations through regression and mixed-effects model experiments in R.
- Present spatial patterns using maps and interactive exploration.

## Quick start: inspect the published aggregates

```sh
python -m pip install -r requirements.txt
python scripts/summarise_accessibility.py
```

The script checks geographic identifiers, counts missing scores and writes descriptive summaries for **7,903 municipalities, 107 provinces and 20 regions** to `results/generated/`. Means are explicitly unweighted; the script does not reconstruct the study's aggregation or refit its models.

## Repository guide

| Folder | Contents |
|---|---|
| `data/processed/` | Selected municipality, province, region and facility aggregates; mixed-model input |
| `scripts/` | Portable audit and descriptive summaries |
| `research/notebooks/` | Original integration, routing and indicator experiments, with outputs cleared |
| `research/R/` | Original regression, travel-time and mapping scripts |
| `results/` | Selected original figures where available |

## Reproducibility and interpretation

The quick start runs entirely on the included aggregate CSVs. The original notebooks and R scripts require additional files, packages, working-directory configuration and potentially routing services. They preserve research development history; methodological variants must be reconciled against the final manuscript before reporting study results. See [REPRODUCIBILITY.md](REPRODUCIBILITY.md) and [DATA.md](DATA.md).

These indicators describe geographic accessibility and system-level patterns. They are not individual medical records, individual risk predictions or a clinical decision tool. No new blanket license is assigned to coauthored work or third-party data.
