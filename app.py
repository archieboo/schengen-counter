from __future__ import annotations

import io
import random
import re
from datetime import date, timedelta

import pandas as pd
from shiny import App, reactive, render, ui
from shinywidgets import output_widget, render_widget
import plotly.graph_objects as go

# ─── Constants ────────────────────────────────────────────────────────────────

TRIP_EMOJIS = [
    "🌍", "🏖️", "🗼", "🏔️", "🎡", "🏛️", "🌊", "🍕", "🎭", "🛤️",
    "🚂", "⛷️", "🎪", "🌺", "🏝️", "🎠", "🦁", "🌋", "🏄", "🎯",
]
PERSON_COLORS = ["#f5f7ee", "#eef6ff"]   # olive, azure
OVERSTAY_BG   = "#ffc8c8"
WD_LABELS     = ["M", "T", "W", "T", "F", "S", "S"]


# ─── Pure helpers ─────────────────────────────────────────────────────────────

def _sample_df() -> pd.DataFrame:
    today = date.today()
    return pd.DataFrame({
        "who":        ["Alice", "Alice", "Bob"],
        "entry_date": [today - timedelta(days=40), today - timedelta(days=10), today - timedelta(days=20)],
        "exit_date":  [today - timedelta(days=25), today - timedelta(days=3),  today - timedelta(days=5)],
        "trip":       ["Paris Spring", "Amsterdam", "Lisbon"],
    })


def _expand(df_u: pd.DataFrame) -> pd.DataFrame:
    """Expand trip rows → one row per day, deduplicated."""
    rows: list[dict] = []
    for _, r in df_u.iterrows():
        d = r["entry_date"]
        while d <= r["exit_date"]:
            rows.append({"date": d, "trip_info": r["trip"]})
            d += timedelta(days=1)
    if not rows:
        return pd.DataFrame(columns=["date", "trip_info"])
    return (
        pd.DataFrame(rows)
        .drop_duplicates("date")
        .sort_values("date")
        .reset_index(drop=True)
    )


def _emoji_map(trips: list[str]) -> dict[str, str]:
    seed = sum(ord(c) for c in "".join(sorted(trips)))
    rng  = random.Random(seed)
    pool = TRIP_EMOJIS * ((len(trips) // len(TRIP_EMOJIS)) + 1)
    return dict(zip(trips, rng.sample(pool, len(trips))))


def _check_overlaps(df: pd.DataFrame) -> list[str]:
    msgs: list[str] = []
    for person in df["who"].unique():
        t = df[df["who"] == person].reset_index(drop=True)
        for i in range(len(t)):
            for j in range(i + 1, len(t)):
                if (t.at[i, "entry_date"] <= t.at[j, "exit_date"] and
                        t.at[j, "entry_date"] <= t.at[i, "exit_date"]):
                    msgs.append(
                        f'{person}: \u201c{t.at[i,"trip"]}\u201d overlaps '
                        f'with \u201c{t.at[j,"trip"]}\u201d'
                    )
    return msgs


def _add_months(d: date, n: int) -> date:
    m = d.month - 1 + n
    return d.replace(year=d.year + m // 12, month=m % 12 + 1, day=1)


def _month_end(d: date) -> date:
    return _add_months(d, 1) - timedelta(days=1)


def _rolling_counts(dates: list[date]) -> dict[date, int]:
    ds = sorted(dates)
    return {d: sum(1 for x in ds if (d - timedelta(days=179)) <= x <= d) for d in ds}


# ─── Calendar (Plotly) ────────────────────────────────────────────────────────

# Layout constants
CELL    = 1.0
COL_GAP = 1.4
ROW_GAP = 2.4    # space for month header + weekday labels
MONTH_W = 7 * CELL + COL_GAP
MONTH_H = 6 * CELL + ROW_GAP

# Colorscale trick: z ∈ [-10, 90] (100 units)
#   -10 → grey  (non-trip, outside window)
#   -5  → light blue (non-trip, inside 180-day window)
#    0  → green  (0 days used)
#   50  → yellow
#   60  → orange
#   70  → red
#   90  → dark red
_COLORSCALE = [
    [0.00, "#E0E0E0"],   # z = -10
    [0.04, "#E0E0E0"],
    [0.05, "#DBEAFE"],   # z = -5  (in-window, no trip)
    [0.09, "#DBEAFE"],
    [0.10, "#A5D6A7"],   # z = 0
    [0.60, "#FFF176"],   # z = 50
    [0.70, "#FFB74D"],   # z = 60
    [0.80, "#E53935"],   # z = 70
    [1.00, "#B71C1C"],   # z = 90
]


def build_calendar(
    daily: pd.DataFrame,
    emap: dict[str, str],
    anchor: date,
) -> go.Figure:
    plot_start = (anchor - timedelta(days=180)).replace(day=1)
    months     = [_add_months(plot_start, i) for i in range(16)]
    win_start  = anchor - timedelta(days=179)
    win_end    = anchor

    date_set     = set(daily["date"].tolist())
    trip_by_date = dict(zip(daily["date"], daily["trip_info"])) if len(daily) else {}
    rolling      = _rolling_counts(list(date_set))

    # Collectors
    xs:      list[float] = []
    ys:      list[float] = []
    zs:      list[float] = []
    hovers:  list[str]   = []
    daytext: list[str]   = []
    ex:      list[float] = []
    ey:      list[float] = []
    emojis:  list[str]   = []
    annotations: list[dict] = []

    for idx, m_start in enumerate(months):
        pc = idx % 4
        pr = idx // 4
        bx = pc * MONTH_W
        by = pr * MONTH_H

        # Month header
        annotations.append(dict(
            x=bx + 3.0, y=-(by - 0.45),
            xref="x", yref="y",
            text=f"<b>{m_start.strftime('%b %Y')}</b>",
            showarrow=False,
            font=dict(size=11, color="white"),
            bgcolor="#2c3e50",
            borderpad=4,
            xanchor="center", yanchor="bottom",
        ))
        # Weekday labels
        for wd_i, lbl in enumerate(WD_LABELS):
            annotations.append(dict(
                x=bx + wd_i, y=-(by + 0.65),
                xref="x", yref="y",
                text=f"<b>{lbl}</b>",
                showarrow=False,
                font=dict(size=9, color="#999"),
                xanchor="center", yanchor="middle",
            ))

        m_end = _month_end(m_start)

        # week_in_month
        week_of: dict[date, int] = {}
        d, wnum, prev_iso = m_start, 0, None
        while d <= m_end:
            iso_w = d.isocalendar()[1]
            if iso_w != prev_iso and prev_iso is not None:
                wnum += 1
            prev_iso = iso_w
            week_of[d] = wnum
            d += timedelta(days=1)

        d = m_start
        while d <= m_end:
            wd  = d.weekday()    # 0 = Mon
            wim = week_of[d]
            px  = bx + wd * CELL
            py  = by + ROW_GAP / 2 + 0.65 + wim * CELL

            in_win   = win_start <= d <= win_end
            has_trip = d in date_set
            rc       = rolling.get(d, 0) if has_trip else 0
            trip     = trip_by_date.get(d)

            z     = float(rc) if has_trip else (-5.0 if in_win else -10.0)
            hover = f"<b>{d.strftime('%d %b %Y')}</b><br>{trip}<br>{rc} days used" if has_trip else ""

            xs.append(px);     ys.append(-py)
            zs.append(z);      hovers.append(hover)
            daytext.append(str(d.day))

            if trip:
                raw   = emap.get(trip, "")
                emoji = "⛔" if rc > 90 else raw
                if emoji:
                    ex.append(px);    ey.append(-py - 0.2)
                    emojis.append(emoji)

            d += timedelta(days=1)

    fig = go.Figure()

    # ── Tile layer (colored squares) ──────────────────────────────────────
    fig.add_trace(go.Scatter(
        x=xs, y=ys,
        mode="markers",
        marker=dict(
            symbol="square",
            size=26,
            color=zs,
            colorscale=_COLORSCALE,
            cmin=-10, cmax=90,
            colorbar=dict(
                title=dict(text="Days used", side="right"),
                thickness=14,
                len=0.5,
                tickvals=[0, 30, 50, 60, 70, 90],
                ticktext=["0", "30", "50", "60", "70", "90"],
                x=1.01, y=0.5, yanchor="middle",
            ),
            line=dict(color="white", width=1.2),
        ),
        hovertext=hovers,
        hovertemplate="%{hovertext}<extra></extra>",
        showlegend=False,
    ))

    # ── Day-number text ───────────────────────────────────────────────────
    fig.add_trace(go.Scatter(
        x=xs, y=ys,
        mode="text",
        text=daytext,
        textposition="top center",
        textfont=dict(size=8, color="rgba(0,0,0,0.38)"),
        hoverinfo="skip",
        showlegend=False,
    ))

    # ── Emoji text ────────────────────────────────────────────────────────
    if ex:
        fig.add_trace(go.Scatter(
            x=ex, y=ey,
            mode="text",
            text=emojis,
            textfont=dict(size=14),
            hoverinfo="skip",
            showlegend=False,
        ))

    # Overstay callout
    annotations.append(dict(
        x=1, y=1, xref="paper", yref="paper",
        text="<b>⛔ = OVERSTAYING!!!</b>",
        showarrow=False,
        font=dict(size=11, color="#E53935"),
        bgcolor="rgba(255,255,255,0.85)",
        borderpad=4,
        xanchor="right", yanchor="top",
    ))

    fig.update_layout(
        annotations=annotations,
        showlegend=False,
        plot_bgcolor="rgba(0,0,0,0)",
        paper_bgcolor="rgba(0,0,0,0)",
        margin=dict(l=5, r=90, t=15, b=10),
        xaxis=dict(
            visible=False,
            range=[-0.8, 4 * MONTH_W - 0.5],
            fixedrange=True,
        ),
        yaxis=dict(
            visible=False,
            range=[-(4 * MONTH_H - 0.5), 0.9],
            scaleanchor="x",
            scaleratio=1,
            fixedrange=True,
        ),
        height=920,
        dragmode=False,
    )

    return fig


# ─── UI ───────────────────────────────────────────────────────────────────────

_CSS = """
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }
#trip_summary_table table { font-size: 12px; }
#main_pane { border-radius: 6px; padding: 8px; transition: background 0.4s ease; }
.modebar { display: none !important; }
"""

app_ui = ui.page_fluid(
    ui.tags.head(ui.tags.style(_CSS)),
    ui.h2("Schengen 90/180 Rule Calculator", style="margin-bottom:12px;"),
    ui.layout_sidebar(
        ui.sidebar(
            # ── Trip History ──────────────────────────────────────────────
            ui.h4("Trip History"),
            ui.div({"style": "overflow-x:auto;"}, ui.output_table("trip_summary_table")),
            ui.hr(),

            # ── Import ────────────────────────────────────────────────────
            ui.panel_well(
                ui.h4("Import Data"),
                ui.input_file("upload_csv", "Upload CSV", accept=[".csv"]),
                ui.hr(),
                ui.input_text("gs_url", "Google Sheet URL:", ""),
                ui.input_action_button(
                    "load_gs", "Load Sheet",
                    class_="btn btn-info",
                    style="width:100%;",
                ),
                ui.br(), ui.br(),
                ui.tags.small("Required columns:"),
                ui.tags.table(
                    {"style": "width:100%;font-size:11px;border-collapse:collapse;margin-top:4px;"},
                    ui.tags.thead(ui.tags.tr(
                        *[ui.tags.th({
                            "style": "text-align:left;padding:3px 5px;"
                                     "background:#ddd;border:1px solid #ccc;"
                          }, c)
                          for c in ["who", "entry_date", "exit_date", "trip"]]
                    )),
                    ui.tags.tbody(ui.tags.tr(
                        *[ui.tags.td({
                            "style": "padding:3px 5px;border:1px solid #ccc;color:#555;"
                          }, v)
                          for v in ["John", "2026-05-01", "2026-05-15", "Italy Vacay"]]
                    )),
                ),
            ),
            ui.hr(),

            # ── Add Trip ──────────────────────────────────────────────────
            ui.h4("Add Trip"),
            ui.panel_well(
                ui.input_text("new_who", "Traveller:", ""),
                ui.input_text("new_trip_name", "Description:", ""),
                ui.input_date_range("new_trip_dates", "Entry / Exit:"),
                ui.input_action_button(
                    "add_trip", "Add Trip",
                    class_="btn btn-primary",
                    style="width:100%;",
                ),
                ui.download_button(
                    "download_csv", "Export CSV",
                    style="width:100%;margin-top:8px;",
                ),
            ),
            ui.hr(),

            # ── Remove Trip ───────────────────────────────────────────────
            ui.h4("Remove Trip"),
            ui.panel_well(
                ui.input_select("remove_trip_select", "Trip:", {}),
                ui.input_action_button(
                    "remove_trip_btn", "Remove",
                    class_="btn btn-danger",
                    style="width:100%;",
                ),
            ),
            width=320,
        ),

        # ── Main panel ────────────────────────────────────────────────────
        ui.output_ui("person_bg_css"),
        ui.div(
            {"id": "main_pane"},

            # Row 1: Traveller + load status
            ui.div(
                {"style": "display:flex;align-items:center;gap:12px;margin-bottom:8px;"},
                ui.input_select("selected_user", "Traveller:", {}, width="220px"),
                ui.output_ui("load_status"),
            ),

            # Row 2: Usage summary + anchor date
            ui.div(
                {
                    "style": (
                        "display:flex;align-items:center;justify-content:space-between;"
                        "padding:10px 16px;margin-bottom:8px;"
                        "background:#fafafa;border-radius:4px;"
                    )
                },
                ui.output_ui("usage_summary"),
                ui.input_date("anchor_date", "Anchor Date:", value=date.today()),
            ),

            # Row 3: Emoji legend
            ui.output_ui("emoji_legend"),

            # Row 4: Calendar
            output_widget("calendar_plot"),
        ),
    ),
)


# ─── Server ───────────────────────────────────────────────────────────────────

def server(input, output, session):
    df_val   = reactive.Value(_sample_df())
    load_msg = reactive.Value(None)

    # ── Computed ──────────────────────────────────────────────────────────

    @reactive.calc
    def daily():
        user = input.selected_user()
        if not user:
            return pd.DataFrame(columns=["date", "trip_info"])
        return _expand(df_val()[df_val()["who"] == user])

    @reactive.calc
    def emap():
        trips = daily()["trip_info"].dropna().unique().tolist()
        return _emoji_map(trips) if trips else {}

    @reactive.calc
    def has_overstay():
        rc = _rolling_counts(daily()["date"].tolist())
        return any(v > 90 for v in rc.values())

    # ── Sync dropdowns ────────────────────────────────────────────────────

    @reactive.effect
    def _sync_users():
        users = df_val()["who"].unique().tolist()
        cur   = input.selected_user()
        sel   = cur if cur in users else (users[0] if users else None)
        ui.update_select("selected_user", choices={u: u for u in users}, selected=sel)
        if not input.new_who() and users:
            ui.update_text("new_who", value=users[0])

    @reactive.effect
    def _sync_remove():
        user = input.selected_user()
        if not user:
            return
        sub = df_val()[df_val()["who"] == user].copy()
        sub["label"] = sub["trip"] + " (" + sub["entry_date"].astype(str) + ")"
        ui.update_select("remove_trip_select", choices={l: l for l in sub["label"]})

    # ── Helpers ───────────────────────────────────────────────────────────

    def _warn(msgs: list[str]) -> None:
        ui.modal_show(ui.modal(
            ui.p(
                {"style": "color:#555;"},
                "Trips overlap for the same traveller. "
                "Days will not be double-counted, but please review:",
            ),
            ui.tags.ul(*[ui.tags.li(m) for m in msgs]),
            title=ui.span(
                {"style": "color:#E53935;font-weight:700;"},
                "⚠️  Overlapping Trips Detected",
            ),
            easy_close=True,
            footer=ui.modal_button("Dismiss"),
        ))

    def _load(new_df: pd.DataFrame, source: str = "file") -> None:
        required = {"who", "entry_date", "exit_date", "trip"}
        if not required.issubset(set(new_df.columns)):
            load_msg.set({"ok": False, "text": "Invalid format — missing required columns"})
            return
        new_df = new_df.copy()
        new_df["entry_date"] = pd.to_datetime(new_df["entry_date"]).dt.date
        new_df["exit_date"]  = pd.to_datetime(new_df["exit_date"]).dt.date
        df_val.set(new_df)
        label = "Google Sheet" if source == "gs" else "CSV file"
        load_msg.set({"ok": True, "text": f"{len(new_df)} trips loaded from {label}"})
        overlaps = _check_overlaps(new_df)
        if overlaps:
            _warn(overlaps)

    # ── Events ────────────────────────────────────────────────────────────

    @reactive.effect
    @reactive.event(input.upload_csv)
    def _upload():
        f = input.upload_csv()
        if f is None:
            return
        _load(pd.read_csv(f[0]["datapath"]))

    @reactive.effect
    @reactive.event(input.load_gs)
    def _gs():
        url = input.gs_url()
        if not url:
            return
        m = re.search(r"/spreadsheets/d/([a-zA-Z0-9_-]+)", url)
        if not m:
            load_msg.set({"ok": False, "text": "Invalid Google Sheet URL"})
            return
        try:
            import requests
            csv_url = (
                f"https://docs.google.com/spreadsheets/d/{m.group(1)}"
                "/export?format=csv"
            )
            resp = requests.get(csv_url, timeout=10)
            resp.raise_for_status()
            _load(pd.read_csv(io.StringIO(resp.text)), source="gs")
        except Exception as e:
            load_msg.set({"ok": False, "text": f"Error: {str(e)[:60]}"})

    @reactive.effect
    @reactive.event(input.add_trip)
    def _add():
        who   = input.new_who()
        name  = input.new_trip_name() or "Trip"
        dates = input.new_trip_dates()
        if not who or not dates or dates[0] is None or dates[1] is None:
            return
        new_row = pd.DataFrame({
            "who":        [who],
            "entry_date": [dates[0]],
            "exit_date":  [dates[1]],
            "trip":       [name],
        })
        updated  = pd.concat([df_val(), new_row], ignore_index=True)
        overlaps = _check_overlaps(updated[updated["who"] == who])
        if overlaps:
            _warn(overlaps)
        df_val.set(updated)

    @reactive.effect
    @reactive.event(input.remove_trip_btn)
    def _remove():
        label = input.remove_trip_select()
        user  = input.selected_user()
        if not label or not user:
            return
        cur = df_val().copy()
        cur["_lbl"] = cur["trip"] + " (" + cur["entry_date"].astype(str) + ")"
        mask = ~((cur["who"] == user) & (cur["_lbl"] == label))
        df_val.set(cur[mask].drop(columns=["_lbl"]).reset_index(drop=True))

    # ── Outputs ───────────────────────────────────────────────────────────

    @render.download(filename=lambda: f"travel-history-{date.today()}.csv")
    def download_csv():
        return df_val().to_csv(index=False)

    @render.ui
    def person_bg_css():
        users  = df_val()["who"].unique().tolist()
        user   = input.selected_user()
        msg    = load_msg()
        loaded = msg is not None and msg.get("ok", False)

        if has_overstay():
            color = OVERSTAY_BG
        elif not loaded or user not in users:
            color = "#ffffff"
        else:
            idx   = users.index(user)
            color = PERSON_COLORS[idx % len(PERSON_COLORS)]

        return ui.tags.style(f"#main_pane {{ background:{color}; }}")

    @render.ui
    def load_status():
        msg = load_msg()
        if msg is None:
            return ui.span()
        col  = "#388E3C" if msg["ok"] else "#E53935"
        icon = "✓" if msg["ok"] else "✗"
        return ui.span(
            {"style": f"font-size:12px;font-weight:600;color:{col};"},
            f"{icon} {msg['text']}",
        )

    @render.ui
    def usage_summary():
        d      = daily()
        anchor = input.anchor_date()
        cnt    = 0
        if len(d):
            w0  = anchor - timedelta(days=179)
            cnt = sum(1 for x in d["date"].tolist() if w0 <= x <= anchor)
        remaining = max(90 - cnt, 0)
        color = (
            "#E53935" if cnt > 90 else
            "#FFB74D" if cnt > 85 else
            "#FBC02D" if cnt > 75 else
            "#388E3C"
        )
        return ui.div(
            ui.tags.h4(
                {"style": "margin:0 0 4px 0;font-size:13px;"
                          "text-transform:uppercase;letter-spacing:0.05em;color:#888;"},
                "Schengen Usage",
            ),
            ui.p(
                {"style": "margin:0 0 8px 0;font-size:12px;color:#aaa;"},
                f"180-day window ending {anchor.strftime('%d %b %Y')}",
            ),
            ui.span(
                {"style": f"font-size:32px;font-weight:700;color:{color};"},
                f"{cnt} / 90",
            ),
            ui.span({"style": "font-size:14px;color:#888;margin-left:6px;"}, "days used"),
            ui.span(
                {"style": f"margin-left:16px;font-size:14px;font-weight:600;color:{color};"},
                f"{remaining} remaining",
            ),
        )

    @render.ui
    def emoji_legend():
        em = emap()
        if not em:
            return ui.span()
        items = [
            ui.span({"style": "white-space:nowrap;"}, f"{emoji} {trip}")
            for trip, emoji in em.items()
        ]
        return ui.div(
            {"style": "display:flex;flex-wrap:wrap;gap:14px;padding:6px 4px;font-size:14px;"},
            *items,
        )

    @render.table
    def trip_summary_table():
        user = input.selected_user()
        if not user:
            return pd.DataFrame()
        sub = df_val()[df_val()["who"] == user].copy().sort_values("entry_date")
        if len(sub) == 0:
            return pd.DataFrame()
        sub["Entry"] = sub["entry_date"].apply(lambda d: d.strftime("%y-%m-%d"))
        sub["Exit"]  = sub["exit_date"].apply(lambda d: d.strftime("%y-%m-%d"))
        sub["Days"]  = sub.apply(
            lambda r: int((r["exit_date"] - r["entry_date"]).days + 1), axis=1
        )
        return sub[["trip", "Entry", "Exit", "Days"]].rename(columns={"trip": "Trip"})

    @render_widget
    def calendar_plot():
        return build_calendar(daily(), emap(), input.anchor_date())


app = App(app_ui, server)
