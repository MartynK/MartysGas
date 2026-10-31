# JOURNAL

## 2026-10-31 CO logger: first model

- Exports committed as received (USB2, USB3, v4, v43, v5, until_2026.07.16.1806). Logging interval 10 s in mid-July, 30 s late July to early August, 60 s from 2026-09-17. Gap 2026-08-05 to 2026-09-17 (battery failure).
- Marton confirmed the two large events as real (2026-09-25 peak 526 ppm, 2026-10-09 peak 884 ppm per-minute max), both with the kitchen hood (páraelszívó) running, so depressurisation spillage of the open-flue heater is the working mechanism. Heater serviced in between with no meaningful corrective step.
- 34 events >= 10 ppm. Rate 2.1 per day in 2026-07/08 against 0.25 per day in 2026-09/10, with July thinned to 60 s instantaneous samples so the drop is not a sampling artefact.
- Worst-case COHb (adult beside the logger throughout, CFK, textbook parameters): 1.9 % at rest and 3.0 % in light activity for 2026-09-25; 3.8 % and 6.2 % for 2026-10-09. Every small event stays under 1 %.
- Decay: half-life about 10 to 11 min in the two incidents (k about 4 per hour, windows open), 2 to 7 min in small events, which is faster than any room air exchange, so the small ones read as a passing plume at the logger rather than a mixed room.
- Baseline outside events: zero at night in July; from about 2026-09-19 a slow 1 to 2 ppm level, flat across hours, lag-60 ACF 0.75, not raised after events, weakly higher on colder nights (r -0.55 with tmin over 37 days). Inside the stated zero offset of this sensor family (±2 ppm on the EL-USB-CO300 datasheet), so the level cannot be read as real CO; the shape can only be told apart from drift with an outdoor reference (Gilice tér station) or a zero check in clean air. Open.
