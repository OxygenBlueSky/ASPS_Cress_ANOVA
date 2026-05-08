# ASPS 1-5 remeasurement pipeline

This doc covers the scripts written in May 2026 to fold the **remeasured
ASPS 1-5 cress lengths** (`260506_ASPS1-5_cress_measures.xlsx`) back into the
project as a parallel **v2 stream**, alongside the original v1 measurements.

The combine pipeline is now `s1 → s2 → s3` inside `cress_combine_files/`
(`s1_build_filename_lookup.r`, `s2_import_imagej_remeasured.r`,
`s3_cress_combine_files_v2.r`), feeding `s4_cress_skewness_check.r` and
`s5_cress_descriptive_anova.r` in the project root. The legacy v1 combine
script (`20251022_cress_combine_files.r`) was retired to `OLD/` once its
output became a frozen artifact consumed by s3.

---

## Why a v2 stream

ASPS 1-5 was measured with a non-standard protocol the first time (a
calibration / standardisation error). The data was remeasured in May 2026.
Rather than throw the original v1 measurements away, we keep both streams in
the combined file under a `version` column (`v1_original` / `v2_remeasured`)
so we can:

- Run the analysis on the corrected v2 numbers for ASPS 1-5.
- Check repeatability v1 vs v2 per bag (sanity check the standardisation fix
  didn't introduce a new artifact).

Bag/exp/code/potency metadata is **not re-entered by hand**: it's reused from
the existing labelled xlsx files via a filename-based lookup.

---

## Where ASPS 6-10 comes from (heads up)

> ⚠️ **The current s1 → s2 → s3 pipeline does not re-derive ASPS 6-10.** It
> reads them straight from the frozen v1 artifact
> `input_data/cress_length_ASPS_1-10_alldata_decoded.xlsx`. That file is the
> *output* of the retired legacy combiner
> (`OLD/20251022_cress_combine_files_v1.r`), which read the canonical raw
> JZ source `input_data/only_combined_data_Kresselaenge_ASPS_6-10_SL.xlsx`,
> applied LASPR1/2 / LAGES1/2 halving, recalculated `root = seedling -
> sprout`, and decoded potencies. Once produced, the combined xlsx was
> treated as immutable.

**Practical consequences**

- Any data issue in ASPS 6-10 (filename ↔ bag-tag mismatches, duplicate
  rows, etc.) lives in `only_combined_data_Kresselaenge_ASPS_6-10_SL.xlsx`
  and propagates verbatim through the pipeline. Known cases as of
  2026-05-08:
  - ASPS 6 potencies B and D: filename suffix (`B_001..B_012`,
    `D_001..D_014`) does not line up with the `bag` column
    (`B_4..B_15`, `D_2..D_15`). Cosmetic; doesn't affect the analysis.
  - `6_A_9` ≡ `6_A_10` and `6_C_10` ≡ `6_C_11`: identical measurements,
    different photos — copy-paste artifacts. To be dropped (separate task).
- s3 has no knowledge of ASPS 6-10's raw source. To regenerate the frozen
  file you'd need to resurrect the legacy combiner from `OLD/`.
- `in_v2_analysis` deliberately falls back to v1 rows for ASPS 6-10 (see
  s3 step 5 below) because no remeasurement exists for those experiments.
  So the v2 analysis view is "remeasured ASPS 1-5 + original ASPS 6-10".

If we ever want ASPS 6-10 to be reproducible from raw inputs the same way
ASPS 1-5 is, that's a planned-but-not-done refactor.

---

## File layout

```
ASPS_Cress_ANOVA/
├── input_data/                            ← all raw inputs the pipeline reads
│   ├── 260506_ASPS1-5_cress_measures.xlsx     ← new ImageJ remeasurement
│   ├── *_labeled.xlsx                          ← 10 hand-labelled v1 files
│   ├── ASPS1-10-decoding table.csv             ← potency code lookup
│   ├── cress_length_ASPS_1-10_alldata_decoded.xlsx
│   │                                            ← v1 frozen artifact (output
│   │                                             of legacy v1 combine script,
│   │                                             now in OLD/, not re-run)
│   ├── 251021_*.xlsx                           ← legacy analysis-ready files
│   └── … (other source xlsx)
├── s4_cress_skewness_check.r              ← downstream step 4
├── s5_cress_descriptive_anova.r           ← downstream step 5
├── outputs/
│   ├── <YYYYMMDD>_s4_skewness_<ver>/      ← s4 outputs
│   └── <YYYYMMDD>_s5_anova_<raw|skewcorr>_<ver>/  ← s5 outputs
├── cress_combine_files/
│   ├── s1_build_filename_lookup.r         ← STEP 1 (reads ../input_data/)
│   ├── s2_import_imagej_remeasured.r      ← STEP 2 (reads ../input_data/)
│   ├── s3_cress_combine_files_v2.r        ← STEP 3 (reads ../input_data/)
│   ├── <YYYYMMDD>_cress_lookup/           ← s1 outputs
│   ├── <YYYYMMDD>_cress_remeasured/       ← s2 outputs
│   └── <YYYYMMDD>_cress_combined/         ← s3 outputs
├── OLD/                                   ← retired scripts and outputs
└── REMEASUREMENT_PIPELINE.md              ← this file
```

Each script writes to a date-prefixed folder next to itself. Re-running on a
different day creates a new folder rather than overwriting earlier outputs.
The downstream scripts auto-detect the most recent matching folder.

---

## How the data is joined

ImageJ records each measurement against a `Label` of the form
`ASPS3_un_P4xxxxx.jpg:1138-0702` (filename + cell coordinate). The
remeasurement clicked the seedlings again, so the **coordinates won't match**
between v1 and v2 — but the **filename does**. The new pipeline therefore:

1. Builds a `filename → (asps_exp_num, code, bag_no)` lookup from the v1
   labelled files (Step 1).
2. Reads the new ImageJ wide-format xlsx, pairs odd/even rows into LASPR
   (sprout) / LAGES (total seedling), then **left-joins on the filename
   portion** of `Label` to attach the v1 metadata (Step 2).
3. Decodes potency from the same `ASPS1-10-decoding table.csv` used by the
   v1 pipeline and binds v1 + v2 into one analysis-ready file (Step 3).

Two failure modes are handled explicitly:

- **copies** — single rows in a labelled file marked as a copy photo
  (`code` is not A-F or `bag no` is text like "COPY OF E2"). These are split
  off in Step 1 to `*_copies.xlsx` and never enter the lookup, so the
  remeasured rows for those filenames carry NA metadata and get dropped at
  the end of Step 3 (matching the v1 "doublet removal" behaviour).
- **ambiguous** — a filename mapped to >1 distinct `(exp, code, bag)` triplet
  across the labelled files (the same image hand-assigned to two bags). Split
  off in Step 1 to `*_ambiguous.xlsx` for manual review. They are also
  excluded from the lookup until you resolve them by hand.

---

## Run order

Set R's working directory to `cress_combine_files/`, then run the three
scripts in order. They are independent — each reads its inputs from disk
and writes its outputs to disk — so you can stop after any step and inspect.

```r
setwd("cress_combine_files")

source("s1_build_filename_lookup.r")
# → inspect <date>_cress_lookup/filename_to_bag_lookup_ambiguous.xlsx
# → inspect <date>_cress_lookup/filename_to_bag_lookup_copies.xlsx

source("s2_import_imagej_remeasured.r")
# → inspect any "needs_resolution = TRUE" rows in the output
# → check console for "LASPR > LAGES" and "duplicate Label" warnings

source("s3_cress_combine_files_v2.r")
# → final combined file (ASPS 1-10, v1+v2 streams, with in_v1_analysis
#   and in_v2_analysis membership flags) ready for downstream s4/s5
```

Console output at each step tells you (a) which folder it's writing to,
(b) which prior-step folder it picked as input, and (c) how many rows
passed/were filtered.

---

## Step 1 (s1) — `s1_build_filename_lookup.r`

**Purpose.** Build the filename → bag lookup table by reading the 10
hand-labelled `*_labeled.xlsx` files in `cress_combine_files/`.

**Inputs**
- `20251003-ASPS{1..5}{gerade,ungerade}_labeled.xlsx`
- `20251020-ASPS2gerade_labeled.xlsx` (Paul's reprocessed version)

**What it does**
1. Reads the `Label`, `code`, `bag no` columns from each file.
2. Strips the `:xxxx-yyyy` suffix from `Label` to get the bare filename.
3. Splits rows into three buckets:
   - **valid**: `code` is A-F and `bag no` is numeric.
   - **copies**: anything else (e.g. `code` blank, `bag no` = "COPY OF E2").
4. Among the valid rows, collapses to one row per filename. Filenames that
   appear with more than one distinct `(asps_exp_num, code, bag_no)` triplet
   are split into the **ambiguous** bucket.

**Outputs** (in `<YYYYMMDD>_cress_lookup/`)
| File | Contents |
|---|---|
| `filename_to_bag_lookup.xlsx` | Clean 1:1 mappings — the lookup used by Step 2. |
| `filename_to_bag_lookup_ambiguous.xlsx` | Filenames mapped to >1 bag, for manual resolution. |
| `filename_to_bag_lookup_copies.xlsx` | Single rows annotated as copies, for audit. |

---

## Step 2 (s2) — `s2_import_imagej_remeasured.r`

**Purpose.** Convert the new ImageJ wide-format remeasurement xlsx into a
long-format dataset matching the v1 schema, with bag/exp/code attached via
the lookup.

**Adapted from** Paul's `Import_ImageJ_data_withSetup.R`. The LASPR/LAGES
pairing logic and QA checks are kept; the Setup-file machinery is replaced
by the filename-based lookup.

**Inputs**
- `../input_data/260506_ASPS1-5_cress_measures.xlsx` (the new remeasurement file).
- Most recent `<date>_cress_lookup/filename_to_bag_lookup.xlsx`.

**What it does**
1. Reads the wide xlsx skipping the two header rows (filename row, then
   repeated `Count/Label/Length/empty` row), positionally so duplicate
   header names don't get auto-renamed.
2. For each image block (every 4 columns), tags odd rows as LASPR (sprout)
   and even rows as LAGES (total seedling). Warns if a block has an odd
   row count (one seedling missing its pair).
3. Stacks LASPR and LAGES into long form, computes
   `LAWU = LAGES − LASPR` (root) and `LAWUSPR = LAWU / LASPR` (rounded 2 dp).
4. Runs Paul's QA checks: duplicate Labels (= "Z" pressed twice in ImageJ),
   `LASPR == LAGES`, `LASPR > LAGES`. Warnings only — no automatic deletion.
5. Left-joins the filename portion of `Label` onto the lookup. Unmatched
   filenames get `bag_no = NA` and `needs_resolution = TRUE`.
6. Reshapes to the v1 column schema (`reference_cell, count, label, …,
   potency`), with extra columns `source_filename`, `source_coord`,
   `needs_resolution` for traceability.

**Output** (in `<YYYYMMDD>_cress_remeasured/`)
- `cress_length_ASPS_1-5_remeasured.xlsx`

---

## Step 3 (s3) — `s3_cress_combine_files_v2.r`

**Purpose.** Decode potency on the remeasured rows, drop unresolved rows,
bind v1 + v2 into one parallel-stream dataset, and tag each row with
analysis-membership flags so downstream scripts can pick a "view" without
having to know about provenance.

**Inputs**
- `cress_length_ASPS_1-10_alldata_decoded.xlsx` (v1 frozen artifact, the
  output of the legacy v1 combine script now in `OLD/`).
- Most recent `<date>_cress_remeasured/cress_length_ASPS_1-5_remeasured.xlsx`.
- `ASPS1-10-decoding table.csv` (same potency lookup as v1).

**What it does**
1. Reads v1 and tags `version = "v1_original"`.
2. Reads v2, derives `(asps_exp_num, code_letter)` from the `exp_no` column,
   looks up `potency` via the same `get_potency()` logic as v1, and tags
   `version = "v2_remeasured"`.
3. Drops v2 rows with `is.na(potency)` — these are either:
   - `needs_resolution = TRUE` rows (ambiguous filenames, no bag mapping), or
   - rows whose filename mapped to a copy-marked record (no v1 metadata).
   Either way they don't enter the analysis dataset. The console reports
   the count and points to the source file for inspection.
4. Casts both streams' `bag` to character (v1 stores it as text), then
   `bind_rows()` and arranges by `version, exp_no, bag, count`.
5. Adds two boolean membership columns:
   - `in_v1_analysis` = `version == "v1_original"` (the original ASPS 1-10
     dataset, untouched).
   - `in_v2_analysis` = (`version == "v2_remeasured"`) OR
     (`version == "v1_original"` AND ASPS exp ≥ 6) — i.e. v2 ASPS 1-5 +
     v1 ASPS 6-10. This is the **best-available** view: remeasured where
     we have it, original where we don't. ASPS 6-10 was never remeasured
     so its v1 rows are the canonical v2 data for those experiments.
6. Computes a per-bag repeatability table: for each `(exp_no, bag, potency)`
   in v2, side-by-side v1/v2 seedling count and mean seedling/sprout/root
   length, pivoted wide.

**Outputs** (in `<YYYYMMDD>_cress_combined/`)
| File | Contents |
|---|---|
| `cress_length_ASPS_1-10_alldata_decoded_v1v2.xlsx` | Long-format dataset, v1 + v2 with `version` + `in_v1_analysis` + `in_v2_analysis` columns. Single source of truth for downstream ANOVA / ICC. |
| `cress_length_ASPS_1-5_repeatability_v1_vs_v2.xlsx` | Per-bag v1 vs v2 means and counts (`n_v1_original` = seedlings in that bag in v1; `n_v2_remeasured` = seedlings in v2). |

---

## Downstream analysis (s4 + s5)

The combine pipeline (s1 → s2 → s3) feeds two numbered downstream scripts
in the project root:

- `s4_cress_skewness_check.r` — picks per-variable left cutoffs to bring
  the distributions toward symmetry, writes a copy of the dataset with
  `T<var>_cut<value>` columns appended.
- `s5_cress_descriptive_anova.r` — bag-level Type-III ANOVA + emmeans
  post-hoc + 24 normalized boxplots, in three groups (ALL / AS / JZ).

Both scripts use the same conventions:

- A **CONFIG block** at the top of the file. Key knob is
  `DATASET_VER <- "v1" | "v2" | "v1v2"`. s5 also has
  `USE_SKEWNESS_CORRECTED <- TRUE/FALSE`.
- They auto-discover the most recent
  `cress_combine_files/<date>_cress_combined/` folder and read the v1v2
  combined xlsx from there. Row selection is by membership flag:
  `"v1"` → `in_v1_analysis`, `"v2"` → `in_v2_analysis`, `"v1v2"` → no
  filter (comparison view; biological samples in ASPS 1-5 appear twice,
  so this is rarely the right ANOVA input).
- Outputs go to `outputs/<YYYYMMDD>_<scripttag>_<purpose>_<ver>/` and all
  files inside are named `<YYYYMMDD>_<scripttag>_<ver>_<purpose>.<ext>`.
  Re-running on a different day creates a new sibling folder; nothing
  gets overwritten.
- s5 with `USE_SKEWNESS_CORRECTED=TRUE` walks `outputs/` for the latest
  matching s4 folder, and swaps `T<var>_cut*` columns back over the
  corresponding raw column before computing bag-level means.

Originals from the pre-numbered era are in `OLD/`.

---

## Troubleshooting

| Symptom | Likely cause | Fix |
|---|---|---|
| `New names: \`\` -> \`...2\`` printed during Step 1 | readxl renaming the unnamed "skift" column. | Harmless. The script drops that column. |
| Step 2 `stop()`: "No filename_to_bag_lookup.xlsx found" | Step 1 wasn't run, or the `*_cress_lookup/` folder was renamed. | Run Step 1, or restore the folder name. |
| Step 2 warning: "odd row count in block N" | One seedling in that image was measured only once (sprout OR total, not both). | Open the source xlsx, find the image, decide whether to add the missing measurement or remove the orphan. |
| Step 2 message: "LASPR > LAGES at N rows" | A measurement order error (sprout > total seedling is impossible). | Inspect the offending Labels; usually a click error in ImageJ. |
| Step 3 `bind_rows` type error on `bag` | v1 file uses character bag, v2 uses numeric — script casts both to character before binding. | If you see this, confirm both `mutate(bag = as.character(bag))` lines are still present in section 3. |
| Many `needs_resolution = TRUE` rows | Lots of ambiguous filenames (>1 bag per filename). | Open `<date>_cress_lookup/filename_to_bag_lookup_ambiguous.xlsx`, decide which mapping is correct, and add the resolved entries to a manual lookup; rerun. |
