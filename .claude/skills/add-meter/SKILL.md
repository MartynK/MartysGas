---
name: add-meter
description: "Add a new gas meter reading to the MartysGas project and rerun the analysis pipeline. Use this skill whenever the user provides a gas meter value (a number around 10000-15000 m3), mentions a meter reading, says they read the meter, or wants to log gas consumption. Trigger phrases include: 'meter is at', 'add reading', 'new reading', 'gas meter', 'log meter', or just a large number with a date that looks like a meter value."
---

# Add Gas Meter Reading

Append a new gas meter reading to `inst/extdata/gaz.xlsx` and rerun the MartysGas analysis pipeline (iter1 through iter5).

## How to parse the user's input

Extract two values from the user's message:

1. **Meter value** (required): A number, typically 10000-15000 range. This is the cumulative m3 shown on the physical gas meter.
2. **Date/time** (optional): When the reading was taken. Accept any reasonable format (e.g. "2026.04.09 16:15", "april 9", "today", "yesterday 3pm"). Default to the current date/time if not provided.

## Steps

### 1. Write the reading specification

Write a two-line file to `inst/extdata/_add_reading.txt`:
- Line 1: meter value (numeric, e.g. `11449.205`)
- Line 2: datetime in ISO format (e.g. `2026-04-09 16:15:00`)

Example:
```
11449.205
2026-04-09 16:15:00
```

### 2. Run the R script

Execute the bundled R script which reads the txt, appends to gaz.xlsx, and reruns the pipeline:

```bash
"C:/Program Files/R/R-4.5.2/bin/Rscript.exe" .claude/skills/add-meter/scripts/add_and_rerun.r
```

Run this from the project root (`C:\Users\mrkma\OneDrive\DKM\Stats_R\R\MartysGas`). The script:
- Reads `_add_reading.txt` for the meter value and datetime
- Loads the current gaz.xlsx, computes Gaz (delta), Nap (days), Rate (m3/day)
- Appends the new row and saves
- Reruns iter1 through iter5
- Prints the decision tool output (budget status + recommendation)
- Cleans up the txt file

### 3. Report to the user

After the script completes, relay the key information:
- Confirmation that the reading was added (meter value, date, computed daily rate)
- The decision tool output (gas consumed, remaining, recommendation)
- Any warnings or errors from the pipeline

Timeout should be generous (~10 minutes) since the pipeline includes GLS model fitting in iter2 if weather models need rebuilding, though typically it skips that and runs in under 30 seconds.
