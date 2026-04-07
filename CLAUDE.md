# Schengen Counter

A single-file R Shiny app (`app.R`) for tracking compliance with the Schengen **90/180 rule** — no more than 90 days in any rolling 180-day window.

> Limited to 90 days within any rolling 180-day period. This is computed by looking back 180 days from any date of presence to ensure cumulative stay does not exceed 90 days. The 90/180-day rule requires that on any given day you are in the Schengen Area, you look back at the previous 180 days and count your total days of presence.

## What it does

- Loads travel history from CSV upload or public Google Sheet (unauthenticated via `googlesheets4`)
- Supports multiple travellers; selector at top of main panel
- Anchor date controls the reference point for the 180-day window
- Detects overlapping trips (modal warning); days are deduplicated so never double-counted
- Add/remove trips, export CSV

## Layout

**Sidebar (width=3):** Trip History → Import Data → Add Trip → Remove Trip

**Main panel (width=9):** Traveller selector + load status → Schengen Usage + Anchor Date → Trip emoji legend → Calendar heatmap

## Calendar heatmap

- Always 16 months (4×4), square tiles via `coord_equal()`
- Weekday labels on every row: `axes = "all_x"` + `scale_x_discrete(position = "top")`
- Each trip gets a stable random emoji (seeded from trip names); ⛔ replaces emoji on overstay days
- Gradient fill on presence days: green → yellow → orange → red (red around 70 days used)
- Dotted grey border marks the 180-day window on non-presence days

## Main panel background color

| State | Color |
|---|---|
| Sample data (no upload) | White |
| Loaded, person 1 | Subtle olive `#f5f7ee` |
| Loaded, person 2 | Subtle azure `#eef6ff` |
| Any overstay detected | Muted red `#ffc8c8` |

## Data format

| Column | Type | Example |
|---|---|---|
| `who` | string | `John` |
| `entry_date` | date | `2026-05-01` |
| `exit_date` | date | `2026-05-15` |
| `trip` | string | `Italy Vacay` |

## Dependencies for R shiny implimentation

`shiny`, `dplyr`, `lubridate`, `ggplot2`, `purrr`, `googlesheets4`, `grid` — plus `ragg` (install separately) for emoji rendering.

## Run

```sh
Rscript -e "shiny::runApp(port = 3838)" app.R
```

## Key implementation notes

- `user_data()` expands trips to daily rows then `distinct(date)` — prevents double-counting overlaps
- `rolling_count` computed rowwise per date — correct 90/180 implementation
- `load_msg` reactiveVal drives load status indicator and per-person background color
- Everything in `app.R`, no modules
