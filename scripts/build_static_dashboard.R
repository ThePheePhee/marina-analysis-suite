# Build a sanitized, static GitHub Pages snapshot of the Marina dashboard.
#
# Privacy rule: this script publishes aggregate tables and aggregate charts only.
# It must not copy raw data, processed participant-level data, free-text AE fields,
# row-level cleaning flags, or participant IDs into docs/.

source("R/utils.R")
use_project_library()
load_required_packages(c("readr", "dplyr", "tidyr", "stringr", "jsonlite"))

site_dir <- "docs"
dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)

read_output <- function(name) {
  path <- file.path("outputs", "tables", name)
  if (!file.exists(path)) {
    stop("Missing required output table: ", path, "\nRun scripts/run_all.R first.", call. = FALSE)
  }
  readr::read_csv(path, show_col_types = FALSE)
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "", paste0(round(100 * x, digits), "%"))
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

clean_label <- function(x) {
  x |>
    stringr::str_replace_all("^ae_", "") |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_sentence()
}

table1 <- read_output("table_1_sample_characteristics.csv")
ae_prev <- read_output("ae_prevalence.csv")
baseline <- read_output("table_2_pre_item_tests_by_ae.csv")
prepost <- read_output("table_3_prepost_full_sample_tests.csv")
change_by_ae <- read_output("table_4_prepost_change_by_ae_tests.csv")
ae_types <- read_output("ae_type_frequencies.csv")
verification <- read_output("verification_method_frequencies.csv")
age_corr <- read_output("age_correlations.csv")
missingness <- read_output("missingness_by_item.csv")
sensitivity_baseline <- read_output("sensitivity_baseline_pre_items_unknown_recode.csv")
sensitivity_change <- read_output("sensitivity_prepost_change_unknown_recode.csv")
missing_ids <- read_output("missing_participant_ids_for_review.csv")
composite_ae <- read_output("composite_score_ae_tests.csv")
composite_full_change <- read_output("composite_score_full_change.csv")
composite_age <- read_output("composite_score_age_correlations.csv")
composite_reliability <- read_output("composite_score_reliability.csv")

read_private_processed <- function(name) {
  path <- file.path("data", "processed", name)
  if (!file.exists(path)) {
    stop("Missing required processed file: ", path, "\nRun scripts/run_all.R first.", call. = FALSE)
  }
  readr::read_csv(path, show_col_types = FALSE)
}

participants_private <- read_private_processed("participant_analysis.csv")
item_long_private <- read_private_processed("item_long.csv")
composite_private <- read_private_processed("composite_scores_joint.csv")

box_summary <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(tibble::tibble(n = 0, min = NA_real_, q1 = NA_real_, median = NA_real_, q3 = NA_real_, max = NA_real_))
  }

  qs <- stats::quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), names = FALSE, type = 2)
  tibble::tibble(
    n = length(x),
    min = qs[1],
    q1 = qs[2],
    median = qs[3],
    q3 = qs[4],
    max = qs[5]
  )
}

pre_item6_box <- item_long_private |>
  dplyr::filter(timepoint == "pre", item == 6) |>
  dplyr::left_join(
    participants_private |> dplyr::select(participant_id, ae_status_primary),
    by = "participant_id"
  ) |>
  dplyr::filter(ae_status_primary %in% c("yes", "no")) |>
  dplyr::group_by(ae_status_primary) |>
  dplyr::summarise(box_summary(score), .groups = "drop") |>
  dplyr::transmute(
    label = paste("AE", ae_status_primary),
    n,
    min,
    q1,
    median,
    q3,
    max
  )

pre_wellbeing_box <- participants_private |>
  dplyr::filter(ae_status_primary %in% c("yes", "no")) |>
  dplyr::group_by(ae_status_primary) |>
  dplyr::summarise(box_summary(wellbeing_composite_pre), .groups = "drop") |>
  dplyr::transmute(
    label = paste("AE", ae_status_primary),
    n,
    min,
    q1,
    median,
    q3,
    max
  )

composite_baseline_box <- composite_private |>
  dplyr::filter(ae_status_primary %in% c("yes", "no")) |>
  dplyr::group_by(ae_status_primary) |>
  dplyr::summarise(box_summary(composite_pre), .groups = "drop") |>
  dplyr::transmute(label = paste("AE", ae_status_primary), n, min, q1, median, q3, max)

composite_change_box <- composite_private |>
  dplyr::filter(ae_status_primary %in% c("yes", "no")) |>
  dplyr::group_by(ae_status_primary) |>
  dplyr::summarise(box_summary(composite_change), .groups = "drop") |>
  dplyr::transmute(label = paste("AE", ae_status_primary), n, min, q1, median, q3, max)

total_n <- sum(ae_prev$n, na.rm = TRUE)
yes_n <- ae_prev$n[match("yes", ae_prev$ae_status)]
no_n <- ae_prev$n[match("no", ae_prev$ae_status)]
unknown_n <- ae_prev$n[match("unknown", ae_prev$ae_status)]
age_flags <- sum(table1$age_uncertain_n, na.rm = TRUE)
age_tentative <- sum(table1$age_tentatively_resolved_n, na.rm = TRUE)

kpis <- tibble::tibble(
  label = c("Participants", "AE yes", "AE no", "AE unknown", "Age review flags", "Tentative age resolutions", "Missing row numbers"),
  value = c(total_n, yes_n, no_n, unknown_n, age_flags, age_tentative, nrow(missing_ids)),
  note = c(
    "Cleaned rows included in analysis",
    fmt_pct(yes_n / total_n),
    fmt_pct(no_n / total_n),
    fmt_pct(unknown_n / total_n),
    "Approximate or uncertain ages",
    "8.5 floored to 8; grades use midpoint expected age",
    "Count only; exact IDs withheld"
  )
)

sample_table <- table1 |>
  dplyr::mutate(
    age_mean = fmt_num(age_mean, 2),
    age_sd = fmt_num(age_sd, 2)
  ) |>
  dplyr::rename(
    "AE status" = ae_status,
    "N" = n,
    "Age mean" = age_mean,
    "Age SD" = age_sd,
    "Age min" = age_min,
    "Age max" = age_max,
    "Age flags" = age_uncertain_n,
    "Tentatively resolved ages" = age_tentatively_resolved_n
  )

ae_prev_table <- ae_prev |>
  dplyr::mutate(pct = fmt_pct(pct)) |>
  dplyr::rename("AE status" = ae_status, "N" = n, "Percent" = pct)

baseline_table <- baseline |>
  dplyr::transmute(
    Item = item,
    Outcome = label,
    Focus = ifelse(primary_focus, "Core", "Secondary"),
    `N yes` = n_yes,
    `N no` = n_no,
    `Median IQR yes` = median_iqr_yes,
    `Median IQR no` = median_iqr_no,
    `p` = fmt_num(p_value, 4),
    `FDR p` = fmt_num(p_fdr, 4),
    `Rank-biserial r` = fmt_num(rank_biserial_r, 3),
    `Effect size` = effect_size_interpretation
  )

prepost_table <- prepost |>
  dplyr::transmute(
    Item = item,
    Outcome = label,
    `Complete pairs` = n_pairs,
    `Median IQR change` = median_iqr_change,
    `p` = fmt_num(p_value, 4),
    `FDR p` = fmt_num(p_fdr, 4),
    `Rank-biserial r` = fmt_num(rank_biserial_r, 3)
  )

change_by_ae_table <- change_by_ae |>
  dplyr::transmute(
    Item = item,
    Outcome = label,
    `N yes` = n_yes,
    `N no` = n_no,
    `Change yes` = median_iqr_yes,
    `Change no` = median_iqr_no,
    `p` = fmt_num(p_value, 4),
    `FDR p` = fmt_num(p_fdr, 4),
    `Rank-biserial r` = fmt_num(rank_biserial_r, 3)
  )

ae_type_table <- ae_types |>
  dplyr::group_by(ae_type) |>
  dplyr::summarise(N = sum(n), .groups = "drop") |>
  dplyr::mutate(`AE type` = clean_label(ae_type)) |>
  dplyr::select(`AE type`, N) |>
  dplyr::arrange(dplyr::desc(N))

verification_table <- verification |>
  dplyr::mutate(
    `Verification method` = clean_label(verification_method),
    Percent = fmt_pct(pct)
  ) |>
  dplyr::select(`Verification method`, N = n, Percent)

age_table <- age_corr |>
  dplyr::transmute(
    Outcome = outcome,
    N = n,
    `Spearman rho` = fmt_num(rho, 3),
    `p` = fmt_num(p_value, 4),
    `Bonferroni p` = fmt_num(p_bonferroni, 4)
  )

missingness_table <- missingness |>
  dplyr::mutate(
    Timepoint = stringr::str_to_upper(timepoint),
    `Missing percent` = fmt_pct(pct_missing)
  ) |>
  dplyr::transmute(
    Timepoint,
    Item = item,
    Outcome = label,
    `Available N` = n_available,
    `Missing N` = n_missing,
    `Missing percent`
  )

sensitivity_baseline_table <- sensitivity_baseline |>
  dplyr::transmute(
    Scenario = scenario,
    Item = item,
    Outcome = label,
    `N yes` = n_yes,
    `N no` = n_no,
    `p` = fmt_num(p_value, 4),
    `FDR p` = fmt_num(p_fdr, 4),
    `Rank-biserial r` = fmt_num(rank_biserial_r, 3)
  )

sensitivity_change_table <- sensitivity_change |>
  dplyr::transmute(
    Scenario = scenario,
    Item = item,
    Outcome = label,
    `N yes` = n_yes,
    `N no` = n_no,
    `p` = fmt_num(p_value, 4),
    `FDR p` = fmt_num(p_fdr, 4),
    `Rank-biserial r` = fmt_num(rank_biserial_r, 3)
  )

composite_ae_table <- composite_ae |>
  dplyr::transmute(
    Analysis = analysis,
    `N yes` = n_yes,
    `N no` = n_no,
    `Median IQR yes` = median_iqr_yes,
    `Median IQR no` = median_iqr_no,
    `p` = fmt_num(p_value, 4),
    `FDR p` = fmt_num(p_fdr, 4),
    `Rank-biserial r` = fmt_num(rank_biserial_r, 3),
    Magnitude = effect_magnitude
  )

composite_age_table <- composite_age |>
  dplyr::transmute(
    Analysis = analysis,
    Group = group,
    N = n,
    `Spearman rho` = fmt_num(spearman_rho, 3),
    `p` = fmt_num(p_value, 4),
    `FDR p` = fmt_num(p_fdr, 4)
  )

composite_reliability_table <- composite_reliability |>
  dplyr::transmute(
    Timepoint = timepoint,
    `Complete all 9` = n_complete_all_9,
    `Items` = n_items,
    `Cronbach alpha` = fmt_num(cronbach_alpha, 3)
  )

chart_data <- list(
  aeStatus = ae_prev |>
    dplyr::transmute(label = stringr::str_to_sentence(ae_status), value = n),
  aeTypes = ae_type_table |>
    dplyr::rename(label = `AE type`, value = N),
  verification = verification_table |>
    dplyr::rename(label = `Verification method`, value = N),
  prepost = prepost |>
    dplyr::transmute(label = paste0("Item ", item), value = rank_biserial_r),
  preItem6Box = pre_item6_box,
  preWellbeingBox = pre_wellbeing_box,
  compositeBaselineBox = composite_baseline_box,
  compositeChangeBox = composite_change_box
)

dashboard_data <- list(
  generatedAt = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  kpis = kpis,
  charts = chart_data,
  tables = list(
    sample = sample_table,
    aePrevalence = ae_prev_table,
    baseline = baseline_table,
    prepost = prepost_table,
    changeByAe = change_by_ae_table,
    aeTypes = ae_type_table,
    verification = verification_table,
    age = age_table,
    missingness = missingness_table,
    sensitivityBaseline = sensitivity_baseline_table,
    sensitivityChange = sensitivity_change_table,
    compositeAe = composite_ae_table,
    compositeAge = composite_age_table,
    compositeReliability = composite_reliability_table
  )
)

json <- jsonlite::toJSON(dashboard_data, dataframe = "rows", auto_unbox = TRUE, na = "null", pretty = FALSE)

css <- "
:root {
  color-scheme: light;
  --ink: #1f2933;
  --muted: #64748b;
  --line: #d8dee8;
  --panel: #ffffff;
  --bg: #f4f7fb;
  --accent: #16697a;
  --accent-2: #db7f67;
  --accent-3: #4f7cac;
  --good: #2a9d8f;
  --warn: #b7791f;
}
* { box-sizing: border-box; }
body {
  margin: 0;
  font-family: Inter, Segoe UI, Roboto, Arial, sans-serif;
  color: var(--ink);
  background: var(--bg);
}
header {
  background: linear-gradient(135deg, #0f3340, #16697a 58%, #4f7cac);
  color: white;
  padding: 34px clamp(20px, 5vw, 72px) 28px;
}
.eyebrow { text-transform: uppercase; letter-spacing: .08em; font-size: 12px; opacity: .8; }
h1 { margin: 8px 0 8px; font-size: clamp(30px, 5vw, 54px); line-height: 1.02; font-weight: 760; }
.subtitle { max-width: 920px; color: #dbeafe; font-size: 17px; line-height: 1.5; }
.privacy {
  margin-top: 18px;
  display: inline-flex;
  gap: 8px;
  align-items: center;
  padding: 9px 12px;
  border: 1px solid rgba(255,255,255,.28);
  border-radius: 6px;
  background: rgba(255,255,255,.1);
  color: #eff6ff;
  font-size: 13px;
}
main { padding: 22px clamp(16px, 4vw, 58px) 46px; }
.tabs {
  display: flex;
  gap: 8px;
  flex-wrap: wrap;
  margin-bottom: 18px;
}
.tab {
  border: 1px solid var(--line);
  background: white;
  color: var(--ink);
  padding: 9px 12px;
  border-radius: 6px;
  cursor: pointer;
  font-weight: 650;
}
.tab.active { background: var(--accent); border-color: var(--accent); color: white; }
.section { display: none; }
.section.active { display: block; }
.grid { display: grid; gap: 14px; }
.kpis { grid-template-columns: repeat(6, minmax(130px, 1fr)); }
.cards-2 { grid-template-columns: repeat(2, minmax(260px, 1fr)); }
.card {
  background: var(--panel);
  border: 1px solid var(--line);
  border-radius: 8px;
  padding: 16px;
  box-shadow: 0 8px 24px rgba(31, 41, 51, .06);
}
.kpi-value { font-size: 30px; line-height: 1; font-weight: 780; color: var(--accent); }
.kpi-label { margin-top: 8px; font-weight: 700; }
.kpi-note { margin-top: 4px; color: var(--muted); font-size: 12px; }
h2 { margin: 24px 0 12px; font-size: 24px; }
h3 { margin: 0 0 12px; font-size: 17px; }
.note { color: var(--muted); line-height: 1.55; max-width: 980px; }
.chart { width: 100%; min-height: 270px; }
.box-chart { width: 100%; min-height: 280px; }
.table-tools { display: flex; justify-content: space-between; align-items: center; gap: 12px; margin-bottom: 8px; }
.search {
  width: min(100%, 310px);
  padding: 9px 10px;
  border: 1px solid var(--line);
  border-radius: 6px;
}
.table-wrap { overflow-x: auto; border: 1px solid var(--line); border-radius: 8px; }
table { border-collapse: collapse; width: 100%; background: white; font-size: 13px; }
th, td { padding: 10px 11px; border-bottom: 1px solid #edf1f7; text-align: left; vertical-align: top; }
th { background: #eef4f8; cursor: pointer; white-space: nowrap; color: #243b53; }
tr:hover td { background: #f8fafc; }
.footer { margin-top: 28px; color: var(--muted); font-size: 12px; }
@media (max-width: 1000px) {
  .kpis, .cards-2 { grid-template-columns: repeat(2, minmax(160px, 1fr)); }
}
@media (max-width: 640px) {
  .kpis, .cards-2 { grid-template-columns: 1fr; }
  header { padding-top: 26px; }
}
"

js <- "
const data = DASHBOARD_DATA;
const fmt = new Intl.NumberFormat();

function el(tag, attrs = {}, children = []) {
  const node = document.createElement(tag);
  Object.entries(attrs).forEach(([k, v]) => {
    if (k === 'class') node.className = v;
    else if (k === 'html') node.innerHTML = v;
    else node.setAttribute(k, v);
  });
  [].concat(children).forEach(child => node.append(child));
  return node;
}

function renderKpis() {
  const root = document.getElementById('kpis');
  data.kpis.forEach(k => {
    root.append(el('div', { class: 'card' }, [
      el('div', { class: 'kpi-value' }, [fmt.format(k.value)]),
      el('div', { class: 'kpi-label' }, [k.label]),
      el('div', { class: 'kpi-note' }, [k.note])
    ]));
  });
}

function renderBarChart(targetId, rows, opts = {}) {
  const target = document.getElementById(targetId);
  const width = 760;
  const rowH = opts.rowH || 34;
  const left = opts.left || 190;
  const right = 36;
  const top = 18;
  const height = top * 2 + rows.length * rowH;
  const max = Math.max(...rows.map(d => Math.abs(Number(d.value) || 0)), 1);
  const color = opts.color || '#16697a';
  let svg = `<svg viewBox='0 0 ${width} ${height}' class='chart' role='img'>`;
  rows.forEach((d, i) => {
    const y = top + i * rowH;
    const val = Number(d.value) || 0;
    const barW = Math.max(2, Math.abs(val) / max * (width - left - right));
    svg += `<text x='12' y='${y + 20}' font-size='13' fill='#334155'>${escapeHtml(d.label)}</text>`;
    svg += `<rect x='${left}' y='${y + 5}' width='${barW}' height='18' rx='4' fill='${color}'></rect>`;
    svg += `<text x='${left + barW + 8}' y='${y + 20}' font-size='13' fill='#334155'>${val.toFixed(opts.decimals ?? 0)}</text>`;
  });
  svg += '</svg>';
  target.innerHTML = svg;
}

function renderDivergingChart(targetId, rows) {
  const target = document.getElementById(targetId);
  const width = 760, height = 360, left = 92, right = 40, top = 24, rowH = 34;
  const mid = left + (width - left - right) / 2;
  const max = Math.max(...rows.map(d => Math.abs(Number(d.value) || 0)), .1);
  let svg = `<svg viewBox='0 0 ${width} ${height}' class='chart' role='img'>`;
  svg += `<line x1='${mid}' x2='${mid}' y1='12' y2='${height - 20}' stroke='#94a3b8'></line>`;
  rows.forEach((d, i) => {
    const y = top + i * rowH;
    const val = Number(d.value) || 0;
    const barW = Math.abs(val) / max * ((width - left - right) / 2);
    const x = val >= 0 ? mid : mid - barW;
    const color = val >= 0 ? '#2a9d8f' : '#db7f67';
    svg += `<text x='12' y='${y + 20}' font-size='13' fill='#334155'>${escapeHtml(d.label)}</text>`;
    svg += `<rect x='${x}' y='${y + 5}' width='${Math.max(2, barW)}' height='18' rx='4' fill='${color}'></rect>`;
    svg += `<text x='${val >= 0 ? x + barW + 6 : x - 46}' y='${y + 20}' font-size='13' fill='#334155'>${val.toFixed(2)}</text>`;
  });
  svg += '</svg>';
  target.innerHTML = svg;
}

function renderBoxChart(targetId, rows, opts = {}) {
  const target = document.getElementById(targetId);
  const width = 760, height = 300;
  const margin = { top: 24, right: 34, bottom: 44, left: 54 };
  const innerW = width - margin.left - margin.right;
  const innerH = height - margin.top - margin.bottom;
  const values = rows.flatMap(d => [d.min, d.q1, d.median, d.q3, d.max].map(Number).filter(Number.isFinite));
  const minVal = opts.min ?? Math.floor(Math.min(...values, 1));
  const maxVal = opts.max ?? Math.ceil(Math.max(...values, 10));
  const scaleY = v => margin.top + (maxVal - v) / (maxVal - minVal) * innerH;
  const groupW = innerW / rows.length;
  const boxW = Math.min(100, groupW * 0.46);
  let svg = `<svg viewBox='0 0 ${width} ${height}' class='box-chart' role='img'>`;
  svg += `<line x1='${margin.left}' x2='${width - margin.right}' y1='${height - margin.bottom}' y2='${height - margin.bottom}' stroke='#94a3b8'></line>`;
  svg += `<line x1='${margin.left}' x2='${margin.left}' y1='${margin.top}' y2='${height - margin.bottom}' stroke='#94a3b8'></line>`;
  [minVal, (minVal + maxVal) / 2, maxVal].forEach(tick => {
    const y = scaleY(tick);
    svg += `<line x1='${margin.left - 5}' x2='${width - margin.right}' y1='${y}' y2='${y}' stroke='#e2e8f0'></line>`;
    svg += `<text x='${margin.left - 10}' y='${y + 4}' text-anchor='end' font-size='12' fill='#64748b'>${tick.toFixed(1)}</text>`;
  });
  rows.forEach((d, i) => {
    const cx = margin.left + groupW * i + groupW / 2;
    const yMin = scaleY(Number(d.min));
    const yQ1 = scaleY(Number(d.q1));
    const yMed = scaleY(Number(d.median));
    const yQ3 = scaleY(Number(d.q3));
    const yMax = scaleY(Number(d.max));
    const color = i === 0 ? '#16697a' : '#db7f67';
    svg += `<line x1='${cx}' x2='${cx}' y1='${yMax}' y2='${yMin}' stroke='${color}' stroke-width='2'></line>`;
    svg += `<line x1='${cx - boxW * .28}' x2='${cx + boxW * .28}' y1='${yMax}' y2='${yMax}' stroke='${color}' stroke-width='2'></line>`;
    svg += `<line x1='${cx - boxW * .28}' x2='${cx + boxW * .28}' y1='${yMin}' y2='${yMin}' stroke='${color}' stroke-width='2'></line>`;
    svg += `<rect x='${cx - boxW / 2}' y='${yQ3}' width='${boxW}' height='${Math.max(1, yQ1 - yQ3)}' rx='5' fill='${color}' opacity='.22' stroke='${color}' stroke-width='2'></rect>`;
    svg += `<line x1='${cx - boxW / 2}' x2='${cx + boxW / 2}' y1='${yMed}' y2='${yMed}' stroke='${color}' stroke-width='3'></line>`;
    svg += `<text x='${cx}' y='${height - 18}' text-anchor='middle' font-size='13' fill='#334155'>${escapeHtml(d.label)} (n=${d.n})</text>`;
  });
  svg += `<text x='${width / 2}' y='${height - 2}' text-anchor='middle' font-size='12' fill='#64748b'>${escapeHtml(opts.caption || '')}</text>`;
  svg += '</svg>';
  target.innerHTML = svg;
}

function escapeHtml(value) {
  return String(value ?? '').replace(/[&<>'\"]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;',\"'\":'&#39;','\"':'&quot;'}[c]));
}

function renderTable(targetId, rows) {
  const target = document.getElementById(targetId);
  const columns = rows.length ? Object.keys(rows[0]) : [];
  const input = el('input', { class: 'search', placeholder: 'Search table' });
  const count = el('span', { class: 'note' });
  const table = el('table');
  const thead = el('thead');
  const tbody = el('tbody');
  let sortKey = columns[0];
  let sortDir = 1;

  const headerRow = el('tr');
  columns.forEach(col => {
    const th = el('th', {}, [col]);
    th.addEventListener('click', () => {
      if (sortKey === col) sortDir *= -1;
      else { sortKey = col; sortDir = 1; }
      draw();
    });
    headerRow.append(th);
  });
  thead.append(headerRow);
  table.append(thead, tbody);
  target.append(el('div', { class: 'table-tools' }, [input, count]), el('div', { class: 'table-wrap' }, [table]));

  input.addEventListener('input', draw);

  function draw() {
    const q = input.value.toLowerCase();
    let filtered = rows.filter(row => Object.values(row).join(' ').toLowerCase().includes(q));
    filtered = filtered.sort((a, b) => {
      const av = a[sortKey], bv = b[sortKey];
      const an = Number(av), bn = Number(bv);
      if (!Number.isNaN(an) && !Number.isNaN(bn)) return (an - bn) * sortDir;
      return String(av ?? '').localeCompare(String(bv ?? '')) * sortDir;
    });
    tbody.innerHTML = '';
    filtered.forEach(row => {
      const tr = el('tr');
      columns.forEach(col => tr.append(el('td', {}, [escapeHtml(row[col])])));
      tbody.append(tr);
    });
    count.textContent = `${filtered.length} rows`;
  }
  draw();
}

function setupTabs() {
  document.querySelectorAll('.tab').forEach(button => {
    button.addEventListener('click', () => {
      document.querySelectorAll('.tab, .section').forEach(x => x.classList.remove('active'));
      button.classList.add('active');
      document.getElementById(button.dataset.tab).classList.add('active');
    });
  });
}

renderKpis();
renderBarChart('aeStatusChart', data.charts.aeStatus, { color: '#16697a', left: 120 });
renderBarChart('aeTypeChart', data.charts.aeTypes, { color: '#4f7cac', left: 230 });
renderBarChart('verificationChart', data.charts.verification, { color: '#db7f67', left: 245 });
renderDivergingChart('prepostChart', data.charts.prepost);
renderBoxChart('preItem6BoxChart', data.charts.preItem6Box, { min: 1, max: 10, caption: 'PRE Item 6 score' });
renderBoxChart('preWellbeingBoxChart', data.charts.preWellbeingBox, { min: 1, max: 10, caption: 'PRE wellbeing composite' });
renderBoxChart('compositeBaselineBoxChart', data.charts.compositeBaselineBox, { min: 1, max: 10, caption: 'Baseline joint composite' });
renderBoxChart('compositeChangeBoxChart', data.charts.compositeChangeBox, { caption: 'Joint composite change' });
Object.entries(data.tables).forEach(([key, rows]) => renderTable(`table-${key}`, rows));
setupTabs();
document.getElementById('generatedAt').textContent = data.generatedAt;
"

html <- paste0(
"<!doctype html>
<html lang='en'>
<head>
<meta charset='utf-8'>
<meta name='viewport' content='width=device-width, initial-scale=1'>
<title>Epicenter Analysis Dashboard Snapshot</title>
<style>", css, "</style>
</head>
<body>
<header>
  <div class='eyebrow'>Static dashboard snapshot</div>
  <h1>Epicenter Program Analysis</h1>
  <p class='subtitle'>Aggregate analysis summary for wellbeing, anxiety/fear, pre-post change, and anomalous-experience group comparisons. This public snapshot is generated from de-identified aggregate outputs only.</p>
  <div class='privacy'>Privacy gate: no raw workbook, participant IDs, names, free-text AE descriptions, or row-level cleaning records are included.</div>
</header>
<main>
  <nav class='tabs' aria-label='Dashboard sections'>
    <button class='tab active' data-tab='overview'>Overview</button>
    <button class='tab' data-tab='baseline'>Baseline</button>
    <button class='tab' data-tab='prepost'>Pre-post</button>
    <button class='tab' data-tab='composites'>Composite scores</button>
    <button class='tab' data-tab='ae'>AE summaries</button>
    <button class='tab' data-tab='quality'>Data quality</button>
    <button class='tab' data-tab='sensitivity'>Sensitivity</button>
  </nav>

  <section id='overview' class='section active'>
    <div id='kpis' class='grid kpis'></div>
    <div class='grid cards-2'>
      <div class='card'><h3>AE status</h3><div id='aeStatusChart'></div></div>
      <div class='card'><h3>Pre-post effect sizes</h3><p class='note'>Rank-biserial effect sizes for item-wise full-sample pre-post tests. Positive values indicate improvement-coded change.</p><div id='prepostChart'></div></div>
    </div>
    <h2>Sample Characteristics</h2><div class='card' id='table-sample'></div>
    <h2>AE Prevalence</h2><div class='card' id='table-aePrevalence'></div>
  </section>

  <section id='baseline' class='section'>
    <h2>Baseline AE Comparisons</h2>
    <p class='note'>Primary analyses compare AE yes vs. AE no only. The AE unknown group is handled separately in sensitivity analyses.</p>
    <div class='grid cards-2'>
      <div class='card'><h3>PRE Item 6 anxiety/fear: box-and-whisker summary</h3><p class='note'>Static privacy-preserving version of the local dashboard plot. Whiskers and boxes are computed from aggregate five-number summaries; no participant-level points are published.</p><div id='preItem6BoxChart'></div></div>
      <div class='card'><h3>PRE composite wellbeing: box-and-whisker summary</h3><p class='note'>Higher composite values indicate better wellbeing after reverse-scoring negatively valenced items.</p><div id='preWellbeingBoxChart'></div></div>
    </div>
    <div class='card' id='table-baseline'></div>
  </section>

  <section id='prepost' class='section'>
    <h2>Full Sample Pre-post Change</h2><div class='card' id='table-prepost'></div>
    <h2>Pre-post Change by AE Group</h2><div class='card' id='table-changeByAe'></div>
  </section>

  <section id='composites' class='section'>
    <h2>Exploratory Joint Composite</h2>
    <p class='note'>All nine paired items are combined after reversing Items 5 and 6 so higher values consistently indicate a more favourable response. At least 7 of 9 items are required, and change uses at least 7 identical PRE/POST item pairs. This broad index is exploratory, not a validated unidimensional scale.</p>
    <div class='grid cards-2'>
      <div class='card'><h3>Baseline composite by AE group</h3><div id='compositeBaselineBoxChart'></div></div>
      <div class='card'><h3>Composite change by AE group</h3><div id='compositeChangeBoxChart'></div></div>
    </div>
    <h2>AE-Group Comparisons</h2><div class='card' id='table-compositeAe'></div>
    <h2>Age Correlations</h2><div class='card' id='table-compositeAge'></div>
    <h2>Internal Consistency</h2><div class='card' id='table-compositeReliability'></div>
    <p class='note'>The public snapshot displays only aggregate summaries. Participant-level age scatterplots and adjusted models remain in the private local Shiny dashboard.</p>
  </section>

  <section id='ae' class='section'>
    <div class='grid cards-2'>
      <div class='card'><h3>AE type frequencies</h3><div id='aeTypeChart'></div></div>
      <div class='card'><h3>Verification methods</h3><div id='verificationChart'></div></div>
    </div>
    <h2>AE Type Table</h2><div class='card' id='table-aeTypes'></div>
    <h2>Verification Table</h2><div class='card' id='table-verification'></div>
  </section>

  <section id='quality' class='section'>
    <h2>Age Correlations</h2><div class='card' id='table-age'></div>
    <h2>Missingness by Item</h2><div class='card' id='table-missingness'></div>
    <p class='note'>Tentative age rules used throughout every calculation: the 8.5 entry is floored to age 8; grade entries use expected midpoint ages (1st = 6.5, 2nd = 7.5, 4th = 9.5). These cases remain flagged for source confirmation. Row IDs and row-level cleaning records are withheld from the public snapshot.</p>
  </section>

  <section id='sensitivity' class='section'>
    <h2>AE Unknown Recoding: Baseline Items</h2><div class='card' id='table-sensitivityBaseline'></div>
    <h2>AE Unknown Recoding: Pre-post Change</h2><div class='card' id='table-sensitivityChange'></div>
  </section>

  <div class='footer'>Generated at <span id='generatedAt'></span>. Static HTML snapshot for GitHub Pages. Local Shiny dashboard remains available for private, participant-level review.</div>
</main>
<script>const DASHBOARD_DATA = ", json, ";</script>
<script>", js, "</script>
</body>
</html>"
)

readr::write_file(html, file.path(site_dir, "index.html"))
readr::write_file("", file.path(site_dir, ".nojekyll"))

public_files <- list.files(site_dir, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE)
blocked_names <- c(
  "participant_id", "participant_name", "age_raw", "ae_raw", "ae_type_raw",
  "verified_by_raw", "valence_raw", "Data for Marina", "Analysis plan.docx"
)
text_files <- public_files[grepl("\\.(html|css|js|json|csv|txt|md)$", public_files, ignore.case = TRUE)]
hits <- purrr::map_chr(text_files, readr::read_file) |>
  purrr::map_lgl(~ any(stringr::str_detect(.x, stringr::fixed(blocked_names, ignore_case = TRUE))))

if (any(hits)) {
  stop("Privacy audit failed for public docs files: ", paste(text_files[hits], collapse = ", "), call. = FALSE)
}

bad_ext <- public_files[grepl("\\.(xlsx|xls|docx|rds|rdata|csv)$", public_files, ignore.case = TRUE)]
if (length(bad_ext) > 0) {
  stop("Privacy audit failed: public docs contains data/document files: ", paste(bad_ext, collapse = ", "), call. = FALSE)
}

message("Static dashboard written to ", file.path(site_dir, "index.html"))
message("Privacy audit passed: docs/ contains no raw/processed data files or blocked identifiers.")
