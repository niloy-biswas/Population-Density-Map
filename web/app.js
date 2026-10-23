/* 3D population density explorer: deck.gl H3HexagonLayer over a MapLibre basemap (OpenFreeMap).
   data/countries.json lists the exported countries; each has data/{key}/hexes.json (hexagons,
   palette, labels, camera presets) and data/{key}/boundaries.geojson, all written by
   scripts/export_web_data.R. countries.json also lists each country's city and region views, so the
   search box can reach any of them before that country loads. URL hash: #{country} or #{country}/{view}. */
(() => {
  const BASEMAP = 'https://tiles.openfreemap.org/styles/bright';
  // English names only (the style default prints Latin + native script on two lines).
  const ENGLISH_NAME = ['coalesce', ['get', 'name:en'], ['get', 'name:latin'], ['get', 'name']];
  // Basemap layers hidden for a calmer map: admin-region names duplicate our own labels,
  // and road shields / POIs crowd the columns at city zoom.
  const HIDDEN_BASEMAP_LAYERS = [
    'label_state',
    'highway-shield-non-us', 'highway-shield-us-interstate', 'road_shield_us',
    'poi_r20', 'poi_r7', 'poi_r1', 'poi_transit',
  ];
  const ACCENT = [11, 206, 175];
  const INK = [15, 23, 42];
  // Tallest column = this share of the view's diagonal, so city views get shorter columns than the country.
  const HEIGHT_PER_DIAGONAL = 0.07;
  // Region labels can show at any zoom; district labels once zoomed this far past the country's default view
  // (relative, because a fixed zoom shows every Bhutan gewog at once yet none of Finland's sub-regions).
  // Within that, MapLibre's collision detection keeps only labels that fit, most populous first.
  const DISTRICT_LABEL_ZOOM_STEP = 1.2;
  // Every view opens at this tilt; the 3D button toggles between it and flat.
  const DEFAULT_PITCH = 30;
  // Our labels are MapLibre symbol layers, not a deck.gl TextLayer: deck's CollisionFilterExtension
  // hides every label in interleaved mode, while MapLibre thins labels like the basemap's own.
  // Region layer last, so regions are placed (and win collisions) before districts.
  const LABEL_SOURCE = 'pdm-labels';
  const LABEL_LAYERS = { district: 'pdm-labels-district', region: 'pdm-labels-region' };
  // Same look as the basemap's place names (label_city / label_town in the OpenFreeMap style).
  const LABEL_FONT = ['Noto Sans Regular'];
  // Admin suffixes dropped from labels and buttons ("Dhaka Division" → "Dhaka", "Aichi Prefecture" → "Aichi").
  const ADMIN_SUFFIX = / (Hill )?District$| Division$| Province$| Pradesh$| Region$| Union Territory$| Gewog$| sub-region$| Prefecture$| Voivodeship$| State$/;
  // Draw on top of the columns regardless of depth, so outlines stay readable.
  const ALWAYS_ON_TOP = { depthCompare: 'always', depthWriteEnabled: false };
  const PANEL_STATE_KEY = 'pdm-panel-collapsed';
  const SEARCH_LIMIT = 8;
  // Countries first, then city presets, then regions, when matches score the same.
  const KIND_ORDER = { country: 0, city: 1, region: 2 };
  const KIND_NAME = { country: 'Country', city: 'City', region: 'Region' };
  const KIND_ICON = {
    country: '<path d="M3.5 12h17M12 3.5c2.5 2.6 3.6 5.5 3.6 8.5s-1.1 5.9-3.6 8.5c-2.5-2.6-3.6-5.5-3.6-8.5s1.1-5.9 3.6-8.5z"/><circle cx="12" cy="12" r="8.5"/>',
    city: '<path d="M12 21s-6.5-5.6-6.5-11a6.5 6.5 0 0 1 13 0c0 5.4-6.5 11-6.5 11z"/><circle cx="12" cy="10" r="2.3"/>',
    region: '<path d="M9 4L3.5 6v14L9 18l6 2 5.5-2V4L15 6 9 4zM9 4v14M15 6v14"/>',
  };
  // Opened by the bare link and unknown hashes. Explicit, because countries.json is alphabetical and a
  // new country (Austria) once quietly took over the landing page. Matches og.jpg.
  const DEFAULT_COUNTRY = 'bangladesh';
  const SITE_TITLE = 'Niloy Biswas';
  const FALLBACK = 'The rendered maps are on <a href="https://github.com/niloy-biswas/Population-Density-Map">GitHub</a>.';

  const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  const fmt = new Intl.NumberFormat('en');
  const fmtShort = new Intl.NumberFormat('en', { notation: 'compact', maximumFractionDigits: 1 });

  const el = {
    map: document.getElementById('map'),
    status: document.querySelector('[data-status]'),
    panel: document.querySelector('[data-panel]'),
    search: document.querySelector('[data-search]'),
    searchClear: document.querySelector('[data-search-clear]'),
    results: document.querySelector('[data-search-results]'),
    menuToggle: document.querySelector('[data-menu-toggle]'),
    menu: document.querySelector('[data-menu]'),
    details: document.querySelector('[data-details]'),
    views: document.querySelector('[data-views]'),
    panelCollapse: document.querySelector('[data-panel-collapse]'),
    panelExpand: document.querySelector('[data-panel-expand]'),
    expandLabel: document.querySelector('[data-expand-label]'),
    name: document.querySelector('[data-name]'),
    stat: document.querySelector('[data-stat]'),
    legend: document.querySelector('[data-legend]'),
    viewCtrl: document.querySelector('[data-view-ctrl]'),
    layersToggle: document.querySelector('[data-layers-toggle]'),
    layers: document.querySelector('[data-layers]'),
    tiltToggle: document.querySelector('[data-tilt-toggle]'),
    height: document.querySelector('[data-height]'),
    heightValue: document.querySelector('[data-height-value]'),
    pitch: document.querySelector('[data-pitch]'),
    bearing: document.querySelector('[data-bearing]'),
    pitchValue: document.querySelector('[data-pitch-value]'),
    bearingValue: document.querySelector('[data-bearing-value]'),
    reset: document.querySelector('[data-reset]'),
  };

  let map;
  let overlay;
  let firstSymbolId;
  let countries = [];
  let country; // current data/{key}/hexes.json
  let hexes = [];
  let boundaries;
  let view;
  let loadToken = 0;
  let searchIndex = [];
  let results = [];
  let activeResult = -1;
  let searchTyped = false; // false while the box just shows the current place

  initPanelToggle();
  const setMenuOpen = initPopover(el.menuToggle, el.menu);

  if (!document.createElement('canvas').getContext('webgl2')) {
    setStatus(`Your browser can’t draw WebGL 2, so the 3D map is off. ${FALLBACK}`);
    return;
  }
  if (typeof maplibregl === 'undefined' || typeof deck === 'undefined' || typeof h3 === 'undefined') {
    setStatus(`The map libraries couldn’t load. ${FALLBACK}`);
    return;
  }

  getJson('data/countries.json')
    .then((list) => {
      if (!list.length) throw new Error('data/countries.json lists no countries');
      countries = list;
      buildSearchIndex();
      initMap();
    })
    .catch(showLoadError);

  // ---- Loading ----

  function getJson(url) {
    return fetch(url).then((r) => {
      if (!r.ok) throw new Error(`${url} request failed (${r.status})`);
      return r.json();
    });
  }

  function showLoadError(err) {
    console.error(err);
    setStatus(`The map couldn’t load. ${FALLBACK}`);
  }

  function setStatus(html) {
    el.status.innerHTML = html;
    el.status.hidden = !html;
  }

  function parseHash() {
    const [key, viewId] = decodeURIComponent(location.hash.slice(1)).split('/');
    const known = (k) => countries.some((c) => c.key === k);
    const fallback = known(DEFAULT_COUNTRY) ? DEFAULT_COUNTRY : countries[0].key;
    return { key: known(key) ? key : fallback, viewId };
  }

  // Swap in another country's data, labels and presets; the map itself stays.
  function loadCountry(key, viewId) {
    const token = ++loadToken;
    setStatus(`Loading ${escapeHtml(countries.find((c) => c.key === key).name)}&hellip;`);
    Promise.all([getJson(`data/${key}/hexes.json`), getJson(`data/${key}/boundaries.geojson`)])
      .then(([data, outlines]) => {
        if (token !== loadToken) return; // a newer selection won
        setCountry(data, outlines);
        setStatus('');
        goTo(country.views.find((v) => v.id === viewId) || country.views[0]);
      })
      .catch(showLoadError);
  }

  function setCountry(data, outlines) {
    country = data;
    boundaries = outlines;
    const palette = data.palette.map(hexToRgb);
    hexes = data.h3.map((id, i) => {
      const p = data.population[i];
      return { h3: id, p, place: data.place[i], color: colorAt(palette, p / data.max) };
    });

    map.getSource(LABEL_SOURCE).setData(labelFeatures());
    const districtLabelZoom = cameraFor(data.views[0]).zoom + DISTRICT_LABEL_ZOOM_STEP;
    map.setLayerZoomRange(LABEL_LAYERS.district, districtLabelZoom, 24);

    el.name.textContent = data.name;
    el.stat.textContent = `${fmtShort.format(data.total)} people · ${fmtShort.format(hexes.length)} hexagons`;
    el.expandLabel.textContent = `${data.name} · ${fmtShort.format(data.total)} people`;
    el.map.setAttribute('aria-label', `Interactive 3D population density map of ${data.name}`);
    document.title = `${data.name} 3D Population Density Map | ${SITE_TITLE}`;
    renderViews();
    renderLegend(data.palette);
  }

  // ---- Map ----

  function initMap() {
    map = new maplibregl.Map({
      container: el.map,
      style: BASEMAP,
      center: [0, 20],
      zoom: 1.5,
      maxPitch: 70,
      attributionControl: { compact: true },
    });
    map.addControl(new maplibregl.NavigationControl({ visualizePitch: true }), 'bottom-right');
    // Bottom positions stack upwards, so this lands above zoom and compass.
    map.addControl({ onAdd: () => { el.viewCtrl.hidden = false; return el.viewCtrl; }, onRemove() {} }, 'bottom-right');

    map.once('load', () => {
      for (const id of HIDDEN_BASEMAP_LAYERS) {
        if (map.getLayer(id)) map.setLayoutProperty(id, 'visibility', 'none');
      }
      useEnglishLabels();
      // Columns go under the basemap's place labels, so city and town names stay on top.
      firstSymbolId = map.getStyle().layers.find((l) => l.type === 'symbol')?.id;
      addLabelLayers();
      overlay = new deck.MapboxOverlay({ interleaved: true, layers: [], getTooltip });
      map.addControl(overlay);

      bindControls();
      const { key, viewId } = parseHash();
      loadCountry(key, viewId);
    });
  }

  function useEnglishLabels() {
    for (const layer of map.getStyle().layers) {
      const field = layer.layout?.['text-field'];
      if (field && JSON.stringify(field).includes('name:latin')) {
        map.setLayoutProperty(layer.id, 'text-field', ENGLISH_NAME);
      }
    }
  }

  function addLabelLayers() {
    map.addSource(LABEL_SOURCE, { type: 'geojson', data: { type: 'FeatureCollection', features: [] } });
    for (const [kind, id] of Object.entries(LABEL_LAYERS)) {
      const region = kind === 'region';
      map.addLayer({
        id,
        type: 'symbol',
        source: LABEL_SOURCE,
        filter: ['==', ['get', 'kind'], kind],
        layout: {
          'text-field': ['get', 'text'],
          'text-font': LABEL_FONT,
          'text-size': region ? 14 : 12,
          'text-max-width': 8,
          'text-padding': 6,
          'symbol-sort-key': ['get', 'rank'],
        },
        paint: {
          'text-color': '#000',
          'text-halo-color': '#fff',
          'text-halo-width': 1,
          'text-halo-blur': 1,
        },
      });
    }
  }

  // Label points ranked by the people living in each area (lower rank is placed first),
  // so where names collide the bigger region or district keeps its label.
  function labelFeatures() {
    const people = { region: new Map(), district: new Map() };
    hexes.forEach(({ p, place }) => {
      const [, district, region] = country.places[place] || [];
      if (region) people.region.set(region, (people.region.get(region) || 0) + p);
      if (district) people.district.set(district, (people.district.get(district) || 0) + p);
    });
    return {
      type: 'FeatureCollection',
      features: country.labels.map((l) => ({
        type: 'Feature',
        geometry: { type: 'Point', coordinates: l.position },
        properties: { kind: l.kind, text: shortName(l.name), rank: -(people[l.kind]?.get(l.name) || 0) },
      })),
    };
  }

  function bindControls() {
    bindSearch();
    initPopover(el.layersToggle, el.layers, el.viewCtrl); // the 3D button leaves it open
    syncHeightValue();
    el.height.addEventListener('input', () => {
      syncHeightValue();
      renderLayers(false);
    });
    el.pitch.addEventListener('input', () => map.jumpTo({ pitch: Number(el.pitch.value) }));
    el.bearing.addEventListener('input', () => map.jumpTo({ bearing: Number(el.bearing.value) }));
    el.tiltToggle.addEventListener('click', () => {
      map.easeTo({ pitch: map.getPitch() > 0 ? 0 : DEFAULT_PITCH, duration: reduceMotion ? 0 : 500 });
    });
    el.reset.addEventListener('click', () => view && goTo(view));
    el.views.addEventListener('wheel', scrollChips, { passive: false });
    map.on('move', syncCameraControls);
    window.addEventListener('hashchange', () => {
      const { key, viewId } = parseHash();
      if (key !== country?.key) loadCountry(key, viewId);
      else {
        const v = country.views.find((x) => x.id === viewId) || country.views[0];
        if (v !== view) goTo(v);
        else syncHash(); // unknown hash on the current view: put the canonical one back
      }
    });
  }

  function syncHash() {
    const hash = view === country.views[0] ? country.key : `${country.key}/${view.id}`;
    if (location.hash.slice(1) !== hash) history.replaceState(null, '', `#${hash}`);
  }

  function syncCameraControls() {
    const pitch = Math.round(map.getPitch());
    const bearing = Math.round(map.getBearing());
    el.pitch.value = pitch;
    el.bearing.value = bearing;
    el.pitchValue.textContent = `${pitch}°`;
    el.bearingValue.textContent = `${bearing}°`;
    el.tiltToggle.setAttribute('aria-pressed', String(pitch > 0));
  }

  function syncHeightValue() {
    el.heightValue.textContent = `${Number(el.height.value)}×`;
  }

  function goTo(v) {
    view = v;
    for (const btn of el.views.querySelectorAll('button')) {
      const active = btn.dataset.view === v.id;
      btn.setAttribute('aria-pressed', String(active));
      if (active) centerChip(btn);
    }
    if (document.activeElement !== el.search) showCurrentPlace();
    syncHash();
    const cam = cameraFor(v);
    if (reduceMotion) map.jumpTo(cam);
    else map.flyTo({ ...cam, duration: 2200, essential: false });
    renderLayers(true);
  }

  // Fit bounds with plain Web Mercator maths. map.cameraForBounds() factors in the current pitch,
  // so the same button would frame differently depending on the previous view.
  function cameraFor(v) {
    const [west, south, east, north] = v.bounds;
    const mercY = (lat) => Math.log(Math.tan(Math.PI / 4 + (lat * Math.PI) / 360));
    const dx = (east - west) / 360;
    const dy = (mercY(north) - mercY(south)) / (2 * Math.PI);
    const padding = framePadding();
    const width = el.map.clientWidth - padding.left - padding.right;
    const height = el.map.clientHeight - padding.top - padding.bottom;
    // A pitched camera squashes the bounds vertically but magnifies the near edge sideways,
    // so a height-limited fit can zoom in and a width-limited fit must zoom out.
    const zoom = Math.min(Math.log2(width / (dx * 512)) - 0.3, Math.log2(height / (dy * 512)) + 0.3);
    const centerLat = (Math.atan(Math.exp((mercY(north) + mercY(south)) / 2)) * 360) / Math.PI - 90;
    return {
      center: [(west + east) / 2, centerLat],
      zoom,
      bearing: 0,
      pitch: DEFAULT_PITCH,
      padding,
    };
  }

  // Keep the framed area clear of the search column: right of it on wide screens, below it on phones.
  function framePadding() {
    const rect = el.panel.getBoundingClientRect();
    const wide = el.map.clientWidth >= 900;
    return {
      top: wide ? 24 : rect.bottom + 12,
      bottom: 24,
      left: wide ? rect.right + 24 : 16,
      right: 24,
    };
  }

  // ---- Layers ----

  function diagonalMetres([west, south, east, north]) {
    const midLat = ((south + north) / 2) * (Math.PI / 180);
    const dx = (east - west) * 111320 * Math.cos(midLat);
    const dy = (north - south) * 110570;
    return Math.hypot(dx, dy);
  }

  function elevationScale() {
    const tallest = diagonalMetres(view.bounds) * HEIGHT_PER_DIAGONAL;
    return (tallest / country.max) * Number(el.height.value);
  }

  // Rebuild the columns (new country, view or height) and the outlines over them.
  function renderLayers(animate) {
    overlay.setProps({ layers: [hexLayer(animate), boundaryLayer()] });
  }

  function hexLayer(animate) {
    return new deck.H3HexagonLayer({
      id: 'population',
      beforeId: firstSymbolId,
      data: hexes,
      getHexagon: (d) => d.h3,
      getElevation: (d) => d.p,
      getFillColor: (d) => d.color,
      extruded: true,
      coverage: 1,
      elevationScale: elevationScale(),
      pickable: true,
      autoHighlight: true,
      highlightColor: [...ACCENT, 255],
      material: { ambient: 0.55, diffuse: 0.55, shininess: 32, specularColor: [40, 40, 40] },
      transitions: animate && !reduceMotion
        ? { elevationScale: { duration: 1200, easing: (t) => 1 - (1 - t) ** 3 } }
        : {},
    });
  }

  function boundaryLayer() {
    return new deck.GeoJsonLayer({
      id: 'boundaries',
      beforeId: LABEL_LAYERS.district, // under our labels, so lines don't cross the names
      data: boundaries,
      getLineColor: (f) => [...INK, f.properties.kind === 'region' ? 200 : 90],
      getLineWidth: (f) => (f.properties.kind === 'region' ? 2 : 1),
      lineWidthUnits: 'pixels',
      parameters: ALWAYS_ON_TOP,
    });
  }

  function getTooltip({ object, layer }) {
    if (!object || layer?.id !== 'population') return null;
    const density = object.p / h3.cellArea(object.h3, h3.UNITS.km2);
    // places[i] = [subdistrict, district, region]; any part can be null.
    const [title, ...rest] = (country.places[object.place] || []).filter(Boolean);
    const where = title
      ? `<div style="font-weight:600">${escapeHtml(title)}</div><div style="opacity:.7;margin-bottom:4px">${escapeHtml(rest.join(' · '))}</div>`
      : '';
    return {
      html: `${where}<strong>${fmt.format(object.p)}</strong> people &middot; ${fmt.format(Math.round(density))} per km²`,
      style: {
        background: 'rgba(10, 15, 30, 0.92)',
        color: 'rgba(255, 255, 255, 0.95)',
        border: '1px solid rgba(255, 255, 255, 0.12)',
        borderRadius: '8px',
        padding: '8px 10px',
        font: '500 13px/1.45 Inter, system-ui, sans-serif',
        maxWidth: '280px',
      },
    };
  }

  // ---- Panel ----

  // Hide the chips and card, leaving the search box and a pill that brings them back.
  // Open by default; the choice survives reloads.
  function initPanelToggle() {
    const apply = (collapsed) => {
      el.details.hidden = collapsed;
      el.panelExpand.hidden = !collapsed;
      el.panelCollapse.setAttribute('aria-expanded', String(!collapsed));
      el.panelExpand.setAttribute('aria-expanded', String(!collapsed));
    };
    const toggle = (collapsed) => {
      apply(collapsed);
      localStorage.setItem(PANEL_STATE_KEY, collapsed ? '1' : '0');
      (collapsed ? el.panelExpand : el.panelCollapse).focus();
      // On phones the panel sits above the framed area; re-centre for the space it frees or takes.
      if (overlay) map.easeTo({ padding: framePadding(), duration: reduceMotion ? 0 : 400 });
    };
    apply(localStorage.getItem(PANEL_STATE_KEY) === '1');
    el.panelCollapse.addEventListener('click', () => toggle(true));
    el.panelExpand.addEventListener('click', () => toggle(false));
  }

  // A toggle button and the panel it opens; clicks outside `scope` and Escape close it. Returns a setter.
  function initPopover(toggle, popover, scope = toggle) {
    const setOpen = (open) => {
      popover.hidden = !open;
      toggle.setAttribute('aria-expanded', String(open));
    };
    toggle.addEventListener('click', () => setOpen(popover.hidden));
    document.addEventListener('pointerdown', (e) => {
      if (!popover.hidden && !popover.contains(e.target) && !scope.contains(e.target)) setOpen(false);
    });
    document.addEventListener('keydown', (e) => {
      if (e.key !== 'Escape' || popover.hidden) return;
      setOpen(false);
      toggle.focus();
    });
    return setOpen;
  }

  function renderViews() {
    el.views.replaceChildren(...country.views.map((v) => {
      const btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'chips__chip';
      btn.dataset.view = v.id;
      btn.textContent = shortName(v.label);
      btn.addEventListener('click', () => goTo(v));
      return btn;
    }));
    el.views.scrollLeft = 0;
  }

  function centerChip(btn) {
    const left = btn.offsetLeft - (el.views.clientWidth - btn.offsetWidth) / 2;
    el.views.scrollTo({ left, behavior: reduceMotion ? 'auto' : 'smooth' });
  }

  // The chip row only scrolls sideways; let a plain mouse wheel do that.
  function scrollChips(e) {
    if (Math.abs(e.deltaY) <= Math.abs(e.deltaX)) return;
    e.preventDefault();
    el.views.scrollLeft += e.deltaY;
  }

  // ---- Search ----

  // Every country plus its city presets and regions (from countries.json). Picking one loads that
  // country if needed and flies to the view, the same as its chip.
  function buildSearchIndex() {
    searchIndex = countries.flatMap((c) => [
      { key: c.key, label: c.name, kind: 'country', detail: `${fmtShort.format(c.total)} people` },
      ...(c.views || []).map((v) => ({ key: c.key, viewId: v.id, label: shortName(v.label), kind: v.kind, detail: c.name })),
    ]).map((item) => ({ ...item, norm: normalize(item.label) }));
  }

  function bindSearch() {
    el.search.disabled = false;
    let selectOnMouseUp = false;
    el.search.addEventListener('pointerdown', () => {
      selectOnMouseUp = document.activeElement !== el.search;
    });
    el.search.addEventListener('focus', () => {
      setMenuOpen(false);
      searchTyped = false;
      el.search.select();
      openResults();
    });
    // A click's mouseup would undo the select() above, so typing would append to the place name.
    el.search.addEventListener('mouseup', (e) => {
      if (selectOnMouseUp) e.preventDefault();
      selectOnMouseUp = false;
    });
    el.search.addEventListener('blur', () => {
      closeResults();
      showCurrentPlace();
    });
    el.search.addEventListener('input', () => {
      searchTyped = true;
      syncClearButton();
      openResults();
    });
    el.search.addEventListener('keydown', onSearchKey);
    // mousedown would blur the input (closing the list) before the click lands.
    el.searchClear.addEventListener('mousedown', (e) => e.preventDefault());
    el.searchClear.addEventListener('click', () => {
      el.search.value = '';
      searchTyped = true;
      syncClearButton();
      el.search.focus();
      openResults();
    });
    el.results.addEventListener('mousedown', (e) => e.preventDefault());
  }

  function onSearchKey(e) {
    if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
      e.preventDefault();
      if (el.results.hidden) return openResults();
      if (!results.length) return;
      const step = e.key === 'ArrowDown' ? 1 : -1;
      setActiveResult(activeResult < 0 && step < 0 ? results.length - 1 : (activeResult + step + results.length) % results.length);
    } else if (e.key === 'Enter') {
      e.preventDefault();
      const item = results[activeResult];
      if (item) pickResult(item);
    } else if (e.key === 'Escape') {
      e.preventDefault();
      if (!el.results.hidden) closeResults();
      else el.search.blur();
    }
  }

  // Until the visitor types, the box holds the current place's name and the list offers every country,
  // so it doubles as the country dropdown.
  function openResults() {
    const q = searchTyped ? normalize(el.search.value.trim()) : '';
    results = searchPlaces(q);
    activeResult = q && results.length ? 0 : -1;
    el.results.replaceChildren(...(results.length
      ? results.map((item, i) => resultOption(item, i, q))
      : [emptyOption()]));
    el.results.hidden = false;
    el.search.setAttribute('aria-expanded', 'true');
    setActiveResult(activeResult);
    if (activeResult < 0) el.results.querySelector('.search__result--current')?.scrollIntoView({ block: 'nearest' });
  }

  function closeResults() {
    el.results.hidden = true;
    el.search.setAttribute('aria-expanded', 'false');
    el.search.removeAttribute('aria-activedescendant');
  }

  // q is already normalized; empty lists every country.
  function searchPlaces(q) {
    if (!q) return searchIndex.filter((item) => item.kind === 'country');
    return searchIndex
      .map((item) => ({ item, score: matchScore(item.norm, q) }))
      .filter((m) => m.score < 3)
      .sort((a, b) => a.score - b.score
        || KIND_ORDER[a.item.kind] - KIND_ORDER[b.item.kind]
        || a.item.label.length - b.item.label.length // "Tokyo" before "Tokushima"
        || a.item.label.localeCompare(b.item.label))
      .slice(0, SEARCH_LIMIT)
      .map((m) => m.item);
  }

  // 0: the name starts with the query, 1: a later word does, 2: it appears mid-word, 3: no match.
  function matchScore(name, q) {
    const at = name.indexOf(q);
    if (at < 0) return 3;
    if (at === 0) return 0;
    return /[\s\-'(/]/.test(name[at - 1]) ? 1 : 2;
  }

  function resultOption(item, i, q) {
    const li = document.createElement('li');
    li.id = `search-result-${i}`;
    li.className = 'search__result';
    li.setAttribute('role', 'option');
    if (item.key === country?.key && (item.viewId || country.views[0].id) === view?.id) {
      li.classList.add('search__result--current');
    }
    li.innerHTML = `<svg class="search__result-icon" viewBox="0 0 24 24" width="18" height="18" aria-hidden="true">${KIND_ICON[item.kind]}</svg>
      <span class="search__result-text"><span class="search__result-label"></span><span class="search__result-detail"></span></span>`;
    li.querySelector('.search__result-label').append(...highlight(item, q));
    li.querySelector('.search__result-detail').textContent = `${KIND_NAME[item.kind]} · ${item.detail}`;
    li.addEventListener('click', () => pickResult(item));
    li.addEventListener('pointermove', () => activeResult !== i && setActiveResult(i));
    return li;
  }

  function emptyOption() {
    const li = document.createElement('li');
    li.className = 'search__empty';
    li.setAttribute('role', 'option');
    li.setAttribute('aria-disabled', 'true');
    li.textContent = 'No country, city or region by that name yet.';
    return li;
  }

  // Bold the matched part. Skipped when accents were stripped, since positions no longer line up.
  function highlight(item, q) {
    const at = q ? item.norm.indexOf(q) : -1;
    if (at < 0 || item.norm.length !== item.label.length) return [item.label];
    const mark = document.createElement('mark');
    mark.textContent = item.label.slice(at, at + q.length);
    return [item.label.slice(0, at), mark, item.label.slice(at + q.length)];
  }

  function setActiveResult(i) {
    activeResult = i;
    el.results.querySelectorAll('.search__result').forEach((li, k) => {
      li.setAttribute('aria-selected', String(k === i));
    });
    const li = document.getElementById(`search-result-${i}`);
    if (li) {
      el.search.setAttribute('aria-activedescendant', li.id);
      li.scrollIntoView({ block: 'nearest' });
    } else {
      el.search.removeAttribute('aria-activedescendant');
    }
  }

  function pickResult(item) {
    el.search.blur();
    el.search.value = item.label;
    syncClearButton();
    if (item.key !== country?.key) loadCountry(item.key, item.viewId);
    else goTo(country.views.find((v) => v.id === item.viewId) || country.views[0]);
  }

  function showCurrentPlace() {
    el.search.value = view ? shortName(view.label) : '';
    syncClearButton();
  }

  function syncClearButton() {
    el.searchClear.hidden = !el.search.value;
  }

  function renderLegend(palette) {
    const stops = palette.map((c, i) => `${c} ${((i / (palette.length - 1)) * 100).toFixed(1)}%`).join(', ');
    // Ticks sit where sqrt(p / max) = 0, 0.25, 0.5, 0.75, 1, matching the colour scale.
    const ticks = [0, 0.25, 0.5, 0.75, 1]
      .map((t) => `<span>${fmtShort.format(Math.round(t * t * country.max))}</span>`)
      .join('');
    el.legend.innerHTML = `
      <p class="legend__title">People per hexagon (~0.74 km²)</p>
      <div class="legend__bar" style="background: linear-gradient(90deg, ${stops})"></div>
      <div class="legend__ticks">${ticks}</div>
      <p class="legend__source">Data: Kontur Population 2023</p>`;
    el.legend.hidden = false;
  }

  // ---- Helpers ----

  // Colour by sqrt(population / max): the data is heavily skewed (median ~600, max ~38k per hexagon).
  function colorAt(palette, t) {
    const pos = Math.sqrt(Math.min(Math.max(t, 0), 1)) * (palette.length - 1);
    const i = Math.min(Math.floor(pos), palette.length - 2);
    const f = pos - i;
    return palette[i].map((v, k) => Math.round(v + (palette[i + 1][k] - v) * f));
  }

  function hexToRgb(hex) {
    const n = parseInt(hex.slice(1), 16);
    return [(n >> 16) & 255, (n >> 8) & 255, n & 255];
  }

  function escapeHtml(s) {
    return s.replace(/[&<>"']/g, (c) => `&#${c.charCodeAt(0)};`);
  }

  // Lower case without accents, so "zurich" finds Zürich.
  function normalize(s) {
    return s.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase();
  }

  function shortName(name) {
    return name.replace(ADMIN_SUFFIX, '');
  }
})();
