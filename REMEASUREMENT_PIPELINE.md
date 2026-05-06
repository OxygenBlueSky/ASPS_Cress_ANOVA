# ASPS 1-5 remeasurement pipeline

This doc covers the scripts written in May 2026 to fold the **remeasured
ASPS 1-5 cress lengths** (`260506_ASPS1-5_cress_measures.xlsx`) back into the
project as a parallel **v2 stream**, alongside the original v1 measurements.

The original v1 pipeline (ASPS 1-10 combine → skewness filter → ANOVA / ICC)
is unchanged. The new scripts live in `cress_combine_files/` and are dated
`20260506_*`.

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

## File layout

```
ASPS_Cress_ANOVA/
├── 260506_ASPS1-5_cress_measures.xlsx     ← INPUT: new ImageJ remeasurement
├── cress_combine_files/
│   ├── 20251022_cress_combine_files.r     ← v1 combine (existing, unchanged)
│   ├── 20260506_build_filename_lookup.r   ← STEP 1
│   ├── 20260506_import_imagej_remeasured.r← STEP 2
│   ├── 20260506_cress_combine_files_v2.r  ← STEP 3
│   ├── *_labeled.xlsx                     ← hand-labelled v1 files (input)
│   ├── ASPS1-10-decoding table.csv        ← potency code lookup
│   ├── cress_length_ASPS_1-10_alldata_decoded.xlsx ← v1 combined (input)
│   ├── <YYYYMMDD>_cress_lookup/           ← Step 1 outputs
│   ├── <YYYYMMDD>_cress_remeasured/       ← Step 2 outputs
│   └── <YYYYMMDD>_cress_combined/         ← Step 3 outputs
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

source("20260506_build_filename_lookup.r")
# → inspect <date>_cress_lookup/filename_to_bag_lookup_ambiguous.xlsx
# → inspect <date>_cress_lookup/filename_to_bag_lookup_copies.xlsx

source("20260506_import_imagej_remeasured.r")
# → inspect any "needs_resolution = TRUE" rows in the output
# → check console for "LASPR > LAGES" and "duplicate Label" warnings

source("20260506_cress_combine_files_v2.r")
# → final combined file ready for downstream analysis
```

Console output at each step tells you (a) which folder it's writing to,
(b) which prior-step folder it picked as input, and (c) how many rows
passed/were filtered.

---

## Step 1 — `20260506_build_filename_lookup.r`

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

## Step 2 — `20260506_import_imagej_remeasured.r`

**Purpose.** Convert the new ImageJ wide-format remeasurement xlsx into a
long-format dataset matching the v1 schema, with bag/exp/code attached via
the lookup.

**Adapted from** Paul's `Import_ImageJ_data_withSetup.R`. The LASPR/LAGES
pairing logic and QA checks are kept; the Setup-file machinery is replaced
by the filename-based lookup.

**Inputs**
- `../260506_ASPS1-5_cress_measures.xlsx` (the new remeasurement file).
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

## Step 3 — `20260506_cress_combine_files_v2.r`

**Purpose.** Decode potency on the remeasured rows, drop unresolved rows,
and bind v1 + v2 into one parallel-stream dataset.

**Inputs**
- `cress_length_ASPS_1-10_alldata_decoded.xlsx` (v1, produced by
  `20251022_cress_combine_files.r`).
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
5. Computes a per-bag repeatability table: for each `(exp_no, bag, potency)`
   in v2, side-by-side v1/v2 seedling count and mean seedling/sprout/root
   length, pivoted wide.

**Outputs** (in `<YYYYMMDD>_cress_combined/`)
| File | Contents |
|---|---|
| `cress_length_ASPS_1-10_alldata_decoded_v1v2.xlsx` | Long-format dataset, v1 + v2 with `version` column. Use this for downstream ANOVA / ICC. |
| `cress_length_ASPS_1-5_repeatability_v1_vs_v2.xlsx` | Per-bag v1 vs v2 means and counts (`n_v1_original` = seedlings in that bag in v1; `n_v2_remeasured` = seedlings in v2). |

---

## Downstream analysis

To run the existing analysis on the new combined dataset, point the input
path of `cress_screwness/20251022_check_skewness_adapted.r` and
`251021_cress_descriptive_ANOVA_graphs.r` at
`<date>_cress_combined/cress_length_ASPS_1-10_alldata_decoded_v1v2.xlsx`,
and add a `filter()` for the version you want to analyse, e.g.

```r
df <- df %>% filter(version == "v2_remeasured" |
                    (version == "v1_original" & !exp_no %in% asps_1_5_exp_nos))
```

(or simpler — analyse the two streams separately and compare).

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
