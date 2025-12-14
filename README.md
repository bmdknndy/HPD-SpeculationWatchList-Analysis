# NYC HPD Speculation Watchlist Analysis

This project is an applied policy analysis of speculative purchases of rent-regulated housing in New York City. I compare the official HPD Speculation Watchlist to a neighborhood-relative (NTA-level) alternative and evaluate how each relates to post-sale eviction activity and housing code violations.

The goal is not just to test whether speculation matters, but to show how *measurement choices* affect what policy tools actually capture.

## Methods
I use building-level panel data and a Sun & Abraham (2020) event-study difference-in-differences design with building and year fixed effects.

## Repo structure
- `analysis/` – Quarto analysis and figures (portfolio-facing)
- `src/` – data prep and modeling scripts
- `exports/` – model outputs and intermediate results
- `docs/` – rendered figures / pages

## Author
Brady Kennedy
