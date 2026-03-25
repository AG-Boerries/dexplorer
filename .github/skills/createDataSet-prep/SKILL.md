---
name: create-dataset-prep
description: Prepare already-analyzed differential gene expression data so it conforms to `dexplorer::createDataSet()`. Use when a user already has counts, metadata, normalized expression, PCA, DGEA, GSEA, or gene annotation outputs and wants an agentic inventory of what is available, a gap analysis against the `dexDataSet` requirements, permission-gated transformation into valid `createDataSet()` inputs, a trial `createDataSet()` run, and a final sanity check against the original data.
---

# Create DataSet Prep

## Overview

Prepare user-supplied analysis outputs for `dexplorer::createDataSet()` without assuming the upstream workflow was run with dexplorer helpers. Start from the files the user already has, determine what can be reused directly, identify what is missing, and only transform data after the user approves the plan.

Read [references/create-dataset-requirements.md](references/create-dataset-requirements.md) before making any claims about required slots, columns, or validation behavior.

## Workflow

### 1. Inventory the user's data first

Inspect the user-supplied files, directories, objects, manifests, and nearby README notes before proposing any transformation. When the user provides serialized R artifacts such as `.rds`, `.RData`, or `.rda`, inspect their internal object names and structures instead of treating them as opaque files.

If subagents are available, begin with parallel read-only exploration. Use at least three explorers with disjoint scopes:

1. Counts and sample metadata: raw counts, QC tables, lab metadata, sequencing metadata, sample/group definitions.
2. Expression structure: normalized counts, PCA coordinates, variance explained, identifier columns, sample naming conventions.
3. Analysis outputs: gene annotations, DGEA tables, GSEA tables, gene-set member tables, contrasts, pathway collections.

Give each explorer read-only ownership. Tell them not to edit files and not to assume missing analyses exist. Have each explorer report:

- candidate files or objects for each `createDataSet()` slot
- exact column names and obvious schema mismatches
- what is missing, ambiguous, or duplicated
- any risks around identifiers, contrasts, sample names, or group labels

If subagents are unavailable, perform the same inventory locally and note that the exploration was not parallelized.

### 2. Synthesize before asking to edit anything

Combine the explorers' findings into a single report for the user. Make the report easy to correct.

Include:

- which `createDataSet()` inputs appear to be available now
- which inputs are missing or ambiguous
- which gaps are simple reshaping or renaming problems
- which gaps require genuinely missing analyses rather than formatting
- whether those missing analyses appear feasible with dexplorer functions and what additional inputs would be needed
- which files you relied on for each conclusion

Before any transformation, ask the user for permission to proceed. Explicitly invite them to correct missed or misinterpreted files before you touch anything.

If required artifacts are missing but seem derivable with dexplorer helpers, ask for separate, explicit approval for analysis work. Make the distinction clear between:

- reshaping existing analyzed outputs
- running dexplorer preprocessing or downstream analysis functions to generate missing artifacts

Do not start the second category without the user's approval.

If the user does not approve, stop after the report.

### 3. Transform only after permission

After permission, spawn one worker agent to own the transformation. Tell that worker:

- create new outputs in a separate location and do not overwrite original user data
- preserve provenance so each generated slot can be traced back to its source file(s)
- standardize names and shapes to the dexplorer contract from the reference file
- retry after debugging if `createDataSet()` reveals a fixable mismatch
- stop and report back if success would require analyses the user has not approved

Prefer deriving valid inputs from existing results over rerunning analysis. Renaming columns, reshaping long or wide tables, joining annotation tables, harmonizing sample IDs, and splitting combined outputs are acceptable transformations.

If the user has explicitly approved analysis work, the worker may also try dexplorer helpers when the needed source inputs exist. Typical examples include:

- `createRawCountsWithStats()`
- `addGeneSymbols()`
- `prepareDfs()`
- `calculateDEG()`
- `calculateGSEA()`

Only run these when the required source inputs are available and the step is a reasonable way to fill a missing `createDataSet()` artifact. Keep the user informed about which missing slots will be generated this way.

Do not fabricate results when the necessary source inputs are absent. If dexplorer-based generation is impossible because raw counts, metadata, contrasts, species, or pathway-collection choices are missing, stop and report the exact blocker.

When testing the transformed data, prefer an actual `dexplorer::createDataSet(..., data_path = NULL)` call so the S4 validity checks run. Use `dexplorer::printDataSetReqs()` or the reference file to interpret failures precisely.

If the attempted `createDataSet()` call fails, classify the blocker:

- formatting bug: fix the transformation and retry
- ambiguous source mapping: pause and ask the user
- approved analysis needed: run the relevant dexplorer helper if approval and inputs are both present
- missing upstream analysis: report the exact missing artifact and why it cannot be inferred safely

### 4. Run a final QC pass

After the worker produces a candidate data set, launch one more read-only QC agent. This final QC step is reasoning-first and does not need new code unless a fix is required afterward.

Ask the QC agent to compare the transformed dexplorer inputs against the original user-supplied data and verify:

- sample coverage and sample naming consistency
- group assignments and contrast labels
- gene identifier consistency across counts, annotations, DGEA, and GSEA
- row counts and obvious filtering losses
- pathway and gene-set member plausibility
- whether any transformation changed meaning rather than format

Have the QC agent report anomalies, caveats, and residual assumptions clearly.

## Guardrails

- Do not claim a slot is present unless you can point to the source file or object.
- Do not overwrite or mutate the user's original data in place.
- Do not silently fabricate missing biological analyses.
- Do not run dexplorer preprocessing or analysis functions unless the user approved that scope.
- Do not hide uncertainty. Surface ambiguous mappings early.
- Keep transformed outputs and a short provenance manifest together.
- Try to use the exact slot and column names from the reference file except where different names are required by dexplorer.

## Important DExploreR-specific notes

- `QualityControl` and `RawCounts` must contain `RunID` before `createDataSet()` is called.
- `createDataSet()` adds `SampleNameUser` and `Group` to `QualityControl` and `RawCounts` by joining `SamplesGroups` on `RunID`. A source QC or raw-count table may therefore still be usable before that join if `RunID` is present and `SamplesGroups` is reliable.
- `GeneSets` must end up with a `PVal` column. Some upstream GSEA outputs use `Pval`; standardize that carefully.
- `NormalizedCounts` is wide and must contain gene annotation columns plus one column per sample.
- `PCA` must contain at least `PC1`, `PC2`, `RunID`, `SampleNameUser`, and `Group`.
- If the user wants `data_path` output, the dataset-overview metadata fields are mandatory as described in the reference.

## Expected response shape

During the inventory phase, present:

- available slots
- missing or ambiguous slots
- proposed transformations
- any missing analyses that would still be required
- whether dexplorer functions could likely generate missing artifacts
- a direct permission request to proceed, clearly separating transformation approval from analysis approval

After transformation, present:

- what was generated
- whether `createDataSet()` succeeded
- any remaining caveats
- QC findings from the final reviewer
