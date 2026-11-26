# ── CSS PROFESSIONNEL OMS/UNICEF ──────────────────────────────────────────────
CSS <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap');

* { box-sizing: border-box; }
body, .content-wrapper { font-family: 'Inter', 'Source Sans Pro', 'Segoe UI', sans-serif !important; }
.content-wrapper, .right-side { background: #F4F6FB !important; }

/* ══════ COLOR SYSTEM ══════
   Primary:   #0B3954  (deep navy — UNHCR authority)
   Accent:    #0077B6  (UN institutional blue)
   Teal:      #00838F  (WHO teal accent)
   Gold:      #D4A843  (UN/WFP warm gold)
   Success:   #2E7D52  (refined green)
   Warning:   #D98B14  (warm amber)
   Danger:    #BF3636  (deep red)
   Surface:   #FFFFFF / #F4F6FB
*/

/* ── Sidebar — Dark Executive ── */
.main-sidebar, .left-side {
  background: linear-gradient(180deg, #0B3954 0%, #082740 60%, #061C2F 100%) !important;
  box-shadow: 3px 0 16px rgba(0,0,0,.2);
}
.sidebar-menu > li > a {
  color: rgba(180,210,235,.8) !important;
  font-size: 13px;
  font-weight: 500;
  padding: 11px 15px 11px 20px;
  transition: all .25s ease;
  border-left: 3px solid transparent;
}
.sidebar-menu > li.active > a,
.sidebar-menu > li > a:hover {
  background: rgba(0,119,182,.2) !important;
  color: #FFFFFF !important;
  border-left: 3px solid #D4A843 !important;
  padding-left: 17px !important;
}
.sidebar-menu .treeview-menu { background: rgba(0,0,0,.18) !important; }
.sidebar-menu .treeview-menu > li > a { color: rgba(150,190,220,.75) !important; font-size: 12px; padding: 7px 15px 7px 30px; }
.sidebar-menu .treeview-menu > li.active > a,
.sidebar-menu .treeview-menu > li > a:hover { color: white !important; background: rgba(212,168,67,.15) !important; }

/* ── Header — Institutional Blue ── */
.main-header .navbar {
  background: linear-gradient(90deg, #0B3954 0%, #0C4A6E 100%) !important;
  box-shadow: 0 2px 12px rgba(0,0,0,.2);
  border-bottom: 2px solid #D4A843;
}
.main-header .logo {
  background: #0B3954 !important;
  color: white !important;
  font-weight: 700;
  font-size: 13px;
  letter-spacing: .3px;
  border-right: 1px solid rgba(212,168,67,.3);
}
.main-header .logo:hover { background: #0E4468 !important; }
.main-header .navbar .nav > li > a { color: rgba(255,255,255,.92) !important; }
.main-header .sidebar-toggle { color: rgba(255,255,255,.8) !important; }
.main-header .sidebar-toggle:hover { color: #D4A843 !important; background: rgba(255,255,255,.08) !important; }

/* ── Value Boxes ── */
.small-box {
  border-radius: 12px !important;
  box-shadow: 0 4px 16px rgba(11,57,84,.08) !important;
  transition: transform .25s, box-shadow .25s;
  overflow: hidden;
  border: 1px solid rgba(0,0,0,.04);
}
.small-box:hover { transform: translateY(-3px); box-shadow: 0 8px 28px rgba(11,57,84,.14) !important; }
.small-box .icon { font-size: 55px !important; transition: transform .3s; }
.small-box:hover .icon { transform: scale(1.08); }
.small-box h3 { font-size: 28px !important; font-weight: 800 !important; }
.small-box p  { font-size: 13px !important; font-weight: 600 !important; opacity: .92; }
.small-box.bg-aqua, .small-box.bg-light-blue { background: linear-gradient(135deg, #0077B6, #0C4A6E) !important; }
.small-box.bg-green { background: linear-gradient(135deg, #2E7D52, #1B5E3B) !important; }
.small-box.bg-yellow { background: linear-gradient(135deg, #D98B14, #B87008) !important; }
.small-box.bg-red { background: linear-gradient(135deg, #BF3636, #8B1A1A) !important; }

/* ── Cards / Boxes ── */
.box {
  border-radius: 12px !important;
  box-shadow: 0 2px 12px rgba(11,57,84,.06) !important;
  border: 1px solid rgba(11,57,84,.06) !important;
  margin-bottom: 16px;
  background: #FFFFFF;
}
.box-header {
  border-radius: 12px 12px 0 0 !important;
  padding: 13px 18px;
  font-weight: 600;
  font-size: 14px;
  letter-spacing: .2px;
}
.box.box-primary .box-header { background: linear-gradient(135deg, #0077B6, #0C4A6E) !important; color: white !important; }
.box.box-success .box-header { background: linear-gradient(135deg, #2E7D52, #1B5E3B) !important; color: white !important; }
.box.box-warning .box-header { background: linear-gradient(135deg, #D98B14, #B87008) !important; color: white !important; }
.box.box-danger  .box-header { background: linear-gradient(135deg, #BF3636, #8B1A1A) !important; color: white !important; }
.box.box-info    .box-header { background: linear-gradient(135deg, #00838F, #005F6B) !important; color: white !important; }
.box.box-purple  .box-header { background: linear-gradient(135deg, #6B3FA0, #4A2872) !important; color: white !important; }

/* ── TabBox ── */
.nav-tabs-custom { box-shadow: 0 2px 12px rgba(11,57,84,.06) !important; border-radius: 12px !important; border: 1px solid rgba(11,57,84,.06); }
.nav-tabs-custom > .nav-tabs { border-bottom: 2px solid #E8ECF2; }
.nav-tabs-custom > .nav-tabs > li.active { border-top-color: #0077B6; }
.nav-tabs-custom > .nav-tabs > li.active > a { font-weight: 600; color: #0B3954; }
.nav-tabs-custom > .nav-tabs > li > a { color: #6B7B8D; transition: color .2s; }
.nav-tabs-custom > .nav-tabs > li > a:hover { color: #0077B6; }

/* ── Tables ── */
table.dataTable thead th {
  background: linear-gradient(135deg, #0B3954, #0C4A6E) !important;
  color: white !important;
  font-weight: 600;
  font-size: 12px;
  padding: 11px 10px !important;
  border: none !important;
  text-transform: uppercase;
  letter-spacing: .4px;
}
table.dataTable thead th:first-child { border-radius: 8px 0 0 0 !important; }
table.dataTable thead th:last-child  { border-radius: 0 8px 0 0 !important; }
table.dataTable tbody tr { transition: background .2s; }
table.dataTable tbody tr:hover { background: rgba(0,119,182,.06) !important; }
table.dataTable.stripe tbody tr.odd { background: #F8FAFC; }
.dataTables_filter input { border: 1.5px solid #CBD5E1 !important; border-radius: 8px !important; padding: 6px 10px; transition: border-color .2s; }
.dataTables_filter input:focus { border-color: #0077B6 !important; box-shadow: 0 0 0 3px rgba(0,119,182,.1); }
.dataTables_length select { border: 1.5px solid #CBD5E1 !important; border-radius: 6px; }

/* ── Selectize ── */
.selectize-input { border: 1.5px solid #CBD5E1 !important; border-radius: 8px !important; font-size: 13px !important; background: #FAFBFC !important; }
.selectize-input.focus { border-color: #0077B6 !important; box-shadow: 0 0 0 3px rgba(0,119,182,.12) !important; background: white !important; }
.selectize-dropdown { border-radius: 8px !important; box-shadow: 0 6px 20px rgba(11,57,84,.12) !important; border: 1px solid #E2E8F0; }

/* ── Sliders ── */
.irs--shiny .irs-bar { background: #0077B6 !important; border-color: #0077B6 !important; }
.irs--shiny .irs-handle { border-color: #0077B6 !important; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  background: #0B3954 !important; border-radius: 4px;
}

/* ── Boutons ── */
.btn-oms {
  background: linear-gradient(135deg, #0077B6, #0C4A6E) !important; color: white !important;
  border: none !important; border-radius: 8px !important; font-weight: 600; padding: 8px 16px;
  transition: all .25s; box-shadow: 0 2px 8px rgba(0,119,182,.2);
}
.btn-oms:hover { transform: translateY(-1px); box-shadow: 0 4px 14px rgba(0,119,182,.35); filter: brightness(1.08); }
.btn-success-oms { background: linear-gradient(135deg, #2E7D52, #1B5E3B) !important; color: white !important; border: none !important; border-radius: 8px !important; font-weight: 600; padding: 8px 16px; }
.btn-danger-oms  { background: linear-gradient(135deg, #BF3636, #8B1A1A) !important; color: white !important; border: none !important; border-radius: 8px !important; font-weight: 600; padding: 8px 16px; }

/* ── Badges & Pills ── */
.badge-ok   { background:#2E7D52; color:white; padding:3px 10px; border-radius:12px; font-size:11px; font-weight:600; display:inline-block; }
.badge-warn { background:#D98B14; color:white; padding:3px 10px; border-radius:12px; font-size:11px; font-weight:600; display:inline-block; }
.badge-err  { background:#BF3636; color:white; padding:3px 10px; border-radius:12px; font-size:11px; font-weight:600; display:inline-block; }
.badge-info { background:#0077B6; color:white; padding:3px 10px; border-radius:12px; font-size:11px; font-weight:600; display:inline-block; }

/* ── Alertes OMS ── */
.alerte-box {
  border-radius: 10px;
  padding: 13px 18px;
  margin-bottom: 12px;
  border-left: 4px solid;
  font-size: 13px;
}
.alerte-ok     { background:#EFF8F1; border-color:#2E7D52; color:#1B5E3B; }
.alerte-alerte { background:#FFF9E6; border-color:#D4A843; color:#8B6914; }
.alerte-serieux{ background:#FFF4E6; border-color:#D98B14; color:#8B5A0B; }
.alerte-crit   { background:#FDF0F0; border-color:#BF3636; color:#8B1A1A; }

/* ── Cards stat ── */
.stat-card {
  background: white;
  border-radius: 10px;
  padding: 16px 18px;
  border: 1px solid #E2E8F0;
  margin-bottom: 8px;
  transition: all .25s;
}
.stat-card:hover { box-shadow: 0 4px 16px rgba(11,57,84,.08); border-color: rgba(0,119,182,.2); }
.stat-card .sc-val { font-size: 26px; font-weight: 800; color: #0B3954; margin: 0; }
.stat-card .sc-lbl { font-size: 11px; color: #6B7B8D; text-transform: uppercase; letter-spacing: .8px; margin: 0; font-weight: 500; }
.stat-card .sc-interp { font-size: 12px; color: #4A5568; margin-top: 4px; }

/* ── Z-Score KPI Cards (Expert OMS) ── */
.zs-kpi-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
  gap: 16px;
  margin-bottom: 20px;
}
.zs-kpi-card {
  background: white;
  border-radius: 14px;
  padding: 20px 22px 18px;
  position: relative;
  overflow: hidden;
  box-shadow: 0 2px 16px rgba(11,57,84,.06);
  border: 1px solid rgba(11,57,84,.08);
  transition: all .3s cubic-bezier(.4,0,.2,1);
}
.zs-kpi-card:hover {
  box-shadow: 0 8px 32px rgba(11,57,84,.12);
  transform: translateY(-3px);
  border-color: rgba(0,119,182,.15);
}
.zs-kpi-card .zs-icon-bg {
  position: absolute;
  top: -8px;
  right: -8px;
  font-size: 72px;
  opacity: .05;
  color: #0B3954;
  pointer-events: none;
}
.zs-kpi-card .zs-header {
  display: flex;
  align-items: center;
  gap: 8px;
  margin-bottom: 10px;
}
.zs-kpi-card .zs-header-icon {
  width: 36px;
  height: 36px;
  border-radius: 10px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 16px;
  color: white;
  flex-shrink: 0;
}
.zs-kpi-card .zs-title {
  font-size: 11px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1.2px;
  color: #5a6a7a;
  line-height: 1.2;
}
.zs-kpi-card .zs-subtitle {
  font-size: 10px;
  color: #8a9aaa;
  font-weight: 400;
  letter-spacing: 0;
  text-transform: none;
}
.zs-kpi-card .zs-value {
  font-size: 36px;
  font-weight: 800;
  line-height: 1;
  margin: 6px 0 4px;
  font-feature-settings: 'tnum';
}
.zs-kpi-card .zs-unit {
  font-size: 16px;
  font-weight: 400;
  opacity: .6;
}
.zs-severity-badge {
  display: inline-block;
  padding: 3px 10px;
  border-radius: 20px;
  font-size: 10px;
  font-weight: 700;
  letter-spacing: .5px;
  text-transform: uppercase;
  margin-top: 4px;
}
.zs-severity-acceptable { background: #EFF8F1; color: #1B5E3B; }
.zs-severity-pauvre      { background: #FFF9E6; color: #8B6914; }
.zs-severity-serieux     { background: #FFF4E6; color: #8B5A0B; }
.zs-severity-critique    { background: #FDF0F0; color: #8B1A1A; }

.zs-decomp {
  margin-top: 12px;
  padding-top: 10px;
  border-top: 1px solid #edf2f7;
}
.zs-decomp-row {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 6px;
  font-size: 12px;
}
.zs-decomp-label { color: #5a6a7a; font-weight: 500; }
.zs-decomp-val   { font-weight: 700; font-feature-settings: 'tnum'; }
.zs-mini-bar-track {
  height: 6px;
  background: #edf2f7;
  border-radius: 3px;
  flex: 1;
  margin: 0 10px;
  overflow: hidden;
}
.zs-mini-bar-fill {
  height: 100%;
  border-radius: 3px;
  transition: width .6s ease;
}
.zs-stat-row {
  display: flex;
  gap: 16px;
  margin-top: 8px;
  padding-top: 8px;
  border-top: 1px dashed #e8edf2;
}
.zs-stat-item {
  font-size: 11px;
  color: #6a7a8a;
}
.zs-stat-item strong {
  color: #2a3a4a;
  font-weight: 700;
}
.zs-quality-dot {
  width: 10px;
  height: 10px;
  border-radius: 50%;
  display: inline-block;
  margin-right: 4px;
  vertical-align: middle;
}

/* ── Upload Zone ── */
.upload-zone {
  border: 2px dashed rgba(212,168,67,.5);
  border-radius: 10px;
  padding: 10px 10px;
  text-align: center;
  background: rgba(212,168,67,.06);
  transition: all .25s;
}
.upload-zone:hover { background: rgba(212,168,67,.12); border-color: rgba(212,168,67,.8); }
.upload-zone .shiny-input-container { margin: 0; }
.upload-zone label { display: none !important; }
.upload-zone .btn { 
  width: 100% !important; 
  background: linear-gradient(135deg, #D4A843, #B8922F) !important; 
  color: #0B3954 !important;
  border: none !important;
  border-radius: 8px !important;
  font-weight: 700 !important;
  padding: 8px 12px !important;
  transition: all .2s;
}
.upload-zone .btn:hover { filter: brightness(1.08); box-shadow: 0 3px 10px rgba(212,168,67,.3); }
.upload-zone input[type=\'text\'] { display: none !important; }
.upload-zone .input-group { display: block !important; }

/* ── Wizard steps ── */
.wizard-step {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 9px 14px;
  border-radius: 10px;
  border: 1px solid #E2E8F0;
  background: white;
  margin-bottom: 8px;
  transition: all .25s;
}
.wizard-step.active  { border-color: #0077B6; background: #F0F7FF; box-shadow: 0 2px 8px rgba(0,119,182,.08); }
.wizard-step.done    { border-color: #2E7D52; background: #EFF8F1; }
.wizard-step .ws-num { width: 28px; height: 28px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 12px; font-weight: 700; flex-shrink: 0; }
.wizard-step.active  .ws-num { background: linear-gradient(135deg, #0077B6, #0C4A6E); color: white; }
.wizard-step.done    .ws-num { background: #2E7D52; color: white; }
.wizard-step.pending .ws-num { background: #E2E8F0; color: #6B7B8D; }

/* ── Scrollbar ── */
::-webkit-scrollbar { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: #F4F6FB; }
::-webkit-scrollbar-thumb { background: #0B3954; border-radius: 3px; opacity: .6; }
::-webkit-scrollbar-thumb:hover { background: #0077B6; }

/* ── Progress bar ── */
.progress-bar { background: linear-gradient(90deg, #0077B6, #D4A843) !important; }

/* ── Section titre ── */
.section-header {
  font-size: 15px;
  font-weight: 700;
  color: #0B3954;
  border-bottom: 3px solid;
  border-image: linear-gradient(90deg, #0077B6, #D4A843) 1;
  padding-bottom: 8px;
  margin-bottom: 14px;
}

/* ── Loading overlay ── */
#loading-overlay {
  position: fixed; top: 0; left: 0; right: 0; bottom: 0;
  background: linear-gradient(135deg, #0B3954 0%, #0C4A6E 50%, #00838F 100%);
  z-index: 99999;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  color: white;
  font-family: 'Inter', 'Source Sans Pro', sans-serif;
}

/* ── i18n — Boutons de langue ── */
.btn-lang {
  border-radius: 4px !important;
  font-weight: 600 !important;
  font-size: 11px !important;
  padding: 2px 8px !important;
  border: none !important;
  cursor: pointer !important;
  transition: background .2s, opacity .2s;
  color: white !important;
}
.btn-lang:hover { opacity: .85 !important; }

/* ═══════════════════════════════════════════════════════════════════════ */
/* DASHBOARD PREMIUM — KPI Cards Power BI+                              */
/* ═══════════════════════════════════════════════════════════════════════ */

/* Grid responsive pour KPI */
.nutri-kpi-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  gap: 16px;
  margin-bottom: 20px;
}

/* KPI Card glassmorphism */
.nutri-kpi-card {
  background: white;
  border-radius: 14px;
  padding: 20px 22px 16px;
  position: relative;
  overflow: hidden;
  box-shadow: 0 2px 16px rgba(11,57,84,.06);
  border: 1px solid rgba(11,57,84,.06);
  border-left: 5px solid #0077B6;
  transition: all .3s cubic-bezier(.4,0,.2,1);
  animation: nutri-fadeUp .5s ease both;
}
.nutri-kpi-card:nth-child(2) { animation-delay: .08s; }
.nutri-kpi-card:nth-child(3) { animation-delay: .16s; }
.nutri-kpi-card:nth-child(4) { animation-delay: .24s; }
.nutri-kpi-card:nth-child(5) { animation-delay: .32s; }
.nutri-kpi-card:nth-child(6) { animation-delay: .40s; }

.nutri-kpi-card:hover {
  box-shadow: 0 8px 32px rgba(11,57,84,.12);
  transform: translateY(-3px);
  border-left-color: #D4A843;
}

/* Icône de fond */
.nutri-kpi-card .kpi-bg-icon {
  position: absolute;
  top: -5px;
  right: -5px;
  font-size: 68px;
  opacity: .05;
  pointer-events: none;
}

/* Header : icône + label */
.nutri-kpi-card .kpi-header {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 10px;
}
.nutri-kpi-card .kpi-icon {
  width: 40px;
  height: 40px;
  border-radius: 12px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 18px;
  color: white;
  flex-shrink: 0;
}
.nutri-kpi-card .kpi-label {
  font-size: 12px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: .6px;
  color: #64748b;
  line-height: 1.2;
}

/* Valeur XXL */
.nutri-kpi-card .kpi-value {
  font-size: 34px;
  font-weight: 800;
  line-height: 1.1;
  margin: 2px 0 6px;
  letter-spacing: -0.5px;
}

/* Sous-texte (IC, seuil, etc.) */
.nutri-kpi-card .kpi-sub {
  font-size: 11px;
  color: #94a3b8;
  margin: 0;
  line-height: 1.4;
}

/* Badge sévérité */
.nutri-kpi-card .kpi-badge {
  display: inline-block;
  padding: 2px 10px;
  border-radius: 20px;
  font-size: 10px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: .5px;
  margin-top: 8px;
}
.kpi-badge-acceptable { background: #EFF8F1; color: #1B5E3B; }
.kpi-badge-alerte     { background: #FFF9E6; color: #8B6914; }
.kpi-badge-serieux    { background: #FFF4E6; color: #8B5A0B; }
.kpi-badge-critique   { background: #FDF0F0; color: #8B1A1A; }
.kpi-badge-urgence    { background: #5C1A1A; color: white; }

/* Mini progress bar dans les KPI */
.nutri-kpi-card .kpi-progress {
  width: 100%;
  height: 6px;
  background: #f1f5f9;
  border-radius: 3px;
  margin-top: 10px;
  overflow: hidden;
}
.nutri-kpi-card .kpi-progress-fill {
  height: 100%;
  border-radius: 3px;
  transition: width 1s cubic-bezier(.4,0,.2,1);
}

/* Couleurs de bordure par sévérité */
.nutri-kpi-card.sev-acceptable { border-left-color: #2E7D52; }
.nutri-kpi-card.sev-alerte     { border-left-color: #D4A843; }
.nutri-kpi-card.sev-serieux    { border-left-color: #D98B14; }
.nutri-kpi-card.sev-critique   { border-left-color: #BF3636; }
.nutri-kpi-card.sev-urgence    { border-left-color: #5C1A1A; }

/* Animation fadeUp */
@keyframes nutri-fadeUp {
  from { opacity: 0; transform: translateY(16px); }
  to   { opacity: 1; transform: translateY(0); }
}

/* Gauge semicirculaire HTML */
.nutri-gauge {
  position: relative;
  width: 180px;
  height: 100px;
  margin: 0 auto;
}
.nutri-gauge-bg {
  width: 180px;
  height: 90px;
  border-radius: 90px 90px 0 0;
  background: #e2e8f0;
  overflow: hidden;
  position: relative;
}
.nutri-gauge-fill {
  position: absolute;
  bottom: 0;
  left: 0;
  width: 180px;
  height: 90px;
  border-radius: 90px 90px 0 0;
  transform-origin: bottom center;
  transition: transform 1.2s cubic-bezier(.4,0,.2,1);
}
.nutri-gauge-value {
  position: absolute;
  bottom: 2px;
  left: 50%;
  transform: translateX(-50%);
  font-size: 26px;
  font-weight: 800;
  text-align: center;
  line-height: 1;
}
.nutri-gauge-label {
  text-align: center;
  font-size: 11px;
  font-weight: 600;
  color: #4A5568;
  margin-top: 4px;
  text-transform: uppercase;
  letter-spacing: .5px;
}

/* Score barre horizontale */
.nutri-score-bar-container {
  background: white;
  border-radius: 14px;
  padding: 20px 24px;
  box-shadow: 0 2px 16px rgba(11,57,84,.06);
  border: 1px solid rgba(11,57,84,.06);
  margin-bottom: 20px;
}
.nutri-score-bar {
  width: 100%;
  height: 18px;
  border-radius: 9px;
  background: linear-gradient(90deg,
    #2E7D52 0%,
    #2E7D52 20%,
    #D4A843 20%,
    #D4A843 40%,
    #D98B14 40%,
    #D98B14 60%,
    #BF3636 60%,
    #BF3636 80%,
    #5C1A1A 80%,
    #5C1A1A 100%
  );
  position: relative;
  overflow: visible;
}
.nutri-score-marker {
  position: absolute;
  top: -6px;
  width: 4px;
  height: 30px;
  background: #1e293b;
  border-radius: 2px;
  transition: left 1s cubic-bezier(.4,0,.2,1);
  box-shadow: 0 2px 6px rgba(0,0,0,.3);
}
.nutri-score-labels {
  display: flex;
  justify-content: space-between;
  margin-top: 6px;
  font-size: 10px;
  color: #94a3b8;
  font-weight: 600;
}

/* Section headers dashboard */
.nutri-section-title {
  font-size: 15px;
  font-weight: 700;
  color: #0B3954;
  margin: 24px 0 14px;
  padding-bottom: 8px;
  border-bottom: 3px solid;
  border-image: linear-gradient(90deg, #0077B6, #D4A843) 1;
  display: flex;
  align-items: center;
  gap: 8px;
}
.nutri-section-title .fa { color: #0077B6; font-size: 16px; }

/* Chart containers premium */
.nutri-chart-card {
  background: white;
  border-radius: 14px;
  padding: 16px 20px 14px;
  box-shadow: 0 2px 16px rgba(11,57,84,.06);
  border: 1px solid rgba(11,57,84,.06);
  margin-bottom: 16px;
}
.nutri-chart-card .chart-title {
  font-size: 13px;
  font-weight: 700;
  color: #0B3954;
  margin-bottom: 10px;
  display: flex;
  align-items: center;
  gap: 6px;
}
.nutri-chart-card .chart-title .fa { color: #0077B6; }

/* Alertes modernes (toast-style) */
.nutri-alert {
  border-radius: 10px;
  padding: 12px 16px;
  margin-bottom: 10px;
  border-left: 5px solid;
  display: flex;
  align-items: flex-start;
  gap: 10px;
  font-size: 12px;
  animation: nutri-fadeUp .3s ease both;
}
.nutri-alert-icon {
  width: 28px;
  height: 28px;
  border-radius: 8px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 13px;
  color: white;
  flex-shrink: 0;
}
.nutri-alert .alert-title {
  font-weight: 700;
  font-size: 12px;
  margin-bottom: 2px;
}
.nutri-alert .alert-text {
  font-size: 11px;
  opacity: .8;
  line-height: 1.4;
}

/* Priority cards */
.nutri-priority-card {
  background: white;
  border-radius: 10px;
  padding: 12px 14px;
  border-left: 4px solid;
  margin-bottom: 8px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  font-size: 12px;
  box-shadow: 0 1px 4px rgba(0,0,0,.04);
}

/* Empty state */
.nutri-empty-state {
  text-align: center;
  padding: 60px 30px;
  background: white;
  border-radius: 14px;
  border: 2px dashed #e2e8f0;
}
.nutri-empty-state .empty-icon { font-size: 48px; color: #CBD5E1; margin-bottom: 12px; }
.nutri-empty-state h3 { font-size: 18px; font-weight: 700; color: #0B3954; margin-bottom: 8px; }
.nutri-empty-state p { font-size: 13px; color: #6B7B8D; }

/* Gauges grid */
.nutri-gauges-grid {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 14px;
}
.nutri-gauge-card {
  background: white;
  border-radius: 12px;
  padding: 16px 12px 12px;
  text-align: center;
  box-shadow: 0 2px 12px rgba(0,0,0,.05);
  border: 1px solid rgba(0,0,0,.06);
}

/* Panneau de filtres du dashboard */
#dashboard_filter_details summary::-webkit-details-marker { display: none; }
#dashboard_filter_details summary { list-style: none; }
#dashboard_filter_details summary:hover { color: #0077B6 !important; }
#dashboard_filter_details summary .fa-chevron-down { transition: transform .2s; }
#dashboard_filter_details[open] summary .fa-chevron-down { transform: rotate(180deg); }
#dashboard_filters .control-label, [id$='-dashboard_filters'] .control-label { color: #334155 !important; font-weight: 600; }
#dashboard_filters .f-section-hdr, [id$='-dashboard_filters'] .f-section-hdr {
  color: #0077B6 !important;
  border-bottom-color: #e2e8f0 !important;
}
#dashboard_filters .checkbox label, [id$='-dashboard_filters'] .checkbox label,
#dashboard_filters .radio label, [id$='-dashboard_filters'] .radio label { color: #334155; }
"
