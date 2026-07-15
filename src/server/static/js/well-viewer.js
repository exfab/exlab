/* src/server/static/js/well-viewer.js */

async function loadWellViewerPage(mainElement, plateId) {
  const backLinkHtml = generateBackArrow('/plates');

  // --- 1. STATE ---
  const state = {
    plateId: plateId,
    plate: null,
    wells: [],
    samples: [],
    resultDefinitions: [],
    plateResults: [],
    currentResultUid: null,
    isPatchMode: false,
    selectedWellCoordinate: null,
    plateResultsTable: null
  };

  // --- 2. HTML SKELETON ---
  mainElement.innerHTML = `
    <div class="page-header">
      <div>
        ${backLinkHtml}
        <h2 id="plate-title">Well Viewer</h2>
      </div>
      <div class="header-actions">
        <a href="/plates/${plateId}/plan" class="button" style="text-decoration: none;">Launch Planner</a>
      </div>
    </div>

    <div id="collapsible-bar" class="collapsible-bar">
      <span class="collapsible-chevron">▸</span> <span>Show actions</span>
    </div>
    <div id="collapsible-actions" class="collapsible-content">
      <button onclick="UploadService.openWizard('plate-layout', '${plateId}')" class="button secondary-button" style="display: inline-flex; align-items: center; justify-content: center; gap: 8px;">
          <span style="font-size: 1.1em;">📄</span> Upload Layout
      </button>
      <button id="add-result-button" class="button">Add Plate Result</button>
      <button id="export-csv-button" class="secondary-button">Export CSV</button>
    </div>

    <div class="tab-bar" style="margin-top: 1rem; margin-bottom: 1rem;">
        <button id="tab-btn-layout" class="tab-btn active">Plate Layout</button>
        <button id="tab-btn-dashboard" class="tab-btn">Results Matrix</button>
        <button id="tab-btn-results" class="tab-btn">Plate Level Results</button>
    </div>

    <div id="view-layout" class="tab-content" style="display: block;">
      <div class="plate-stage">
        <div class="plate-frame">
          <div id="well-grid">Loading...</div>
        </div>
      </div>
    </div>

    <div id="view-dashboard" class="tab-content" style="display: none;">
      <div class="table-header">
          <h3>Plate Results Matrix</h3>
      </div>
      <table id="dashboard-table" class="display nowrap" style="width:100%">
      </table>
    </div>

    <div id="view-results" class="tab-content" style="display: none;">
      <div class="plate-results">
        <div style="display:flex; align-items:center; gap:15px; margin-bottom: 1rem;">
          <h3>Plate Level Results</h3>
          <button id="export-results-button" class="secondary-button" style="padding: 0.25rem 0.5rem; font-size: 0.85rem;">Export CSV</button>
        </div>
        <table id="plate-results-table" class="display" style="width:100%;">
            <thead><tr><th>Name</th><th>Value</th><th>Updated At</th></tr></thead>
            <tbody></tbody>
        </table>
      </div>
    </div>
    
    <div id="well-modal" class="modal" style="display:none;">
      <div class="modal-content">
        <span class="close-button">&times;</span>
        <h3 id="modal-well-coordinate"></h3>
        <div id="modal-sample-info"></div>
        <div class="input-group">
          <div class="select-wrapper">
            <select id="modal-sample-select"></select>
          </div>
          <button id="modal-assign-button" class="button">Assign</button>
        </div>
        <button id="modal-unassign-button" class="button" style="display:none; background-color: #dc3545; border-color: #dc3545;">Unassign</button>
      </div>
    </div>

    <div id="add-result-modal" class="modal" style="display:none;">
      <div class="modal-content">
        <span class="close-button">&times;</span>
        <h3 id="resultModalTitle">Add Result to Plate</h3>
        <div class="form-container">
            <div class="input-group">
                <label for="result-definition-select">Result Type</label>
                <div class="select-wrapper">
                    <select id="result-definition-select"><option value="">Loading...</option></select>
                </div>
            </div>
            
            <div id="scalarInputGroup" class="input-group">
                <label for="result-value-input">Value</label>
                <input type="text" id="result-value-input" />
            </div>
            
            <div id="timeSeriesInputGroup" class="input-group" style="display:none;">
                <label>Value to Append:</label>
                <input type="text" id="tsValueInput" placeholder="Measurement">
                <small class="text-secondary" style="margin-top:5px; display:block;">Step number (Time) will be auto-assigned</small>
            </div>
            
            <button id="save-result-btn" class="button">Save Result</button>
        </div>
      </div>
    </div>

    <div id="export-modal" class="modal" style="display:none;">
      <div class="modal-content">
        <span class="close-button">&times;</span>
        <h3>Export Plate Layout</h3>
        <p>Select the format for the CSV export:</p>
        <div class="input-group">
            <div class="select-wrapper">
              <select id="export-format-select-modal">
                <option value="alphanumeric">Alphanumeric</option>
                <option value="numeric">Numeric</option>
                <option value="matrix">Matrix</option>
              </select>
            </div>
        </div>
        <button id="confirm-export-button" class="button">Download</button>
      </div>
    </div>
  `;

  // --- 3. DOM ELEMENTS ---
  const dom = {
    wellGrid: document.getElementById('well-grid'),
    plateTitle: document.getElementById('plate-title'),
    exportCsvButton: document.getElementById('export-csv-button'),
    exportResultsButton: document.getElementById('export-results-button'),
    addResultButton: document.getElementById('add-result-button'),
    
    // Tabs
    btnLayout: document.getElementById('tab-btn-layout'),
    btnDashboard: document.getElementById('tab-btn-dashboard'),
    btnResults: document.getElementById('tab-btn-results'),
    
    exportModal: document.getElementById('export-modal'),
    exportFormatSelectModal: document.getElementById('export-format-select-modal'),
    confirmExportButton: document.getElementById('confirm-export-button'),
    
    wellModal: document.getElementById('well-modal'),
    modalWellCoordinate: document.getElementById('modal-well-coordinate'),
    modalSampleInfo: document.getElementById('modal-sample-info'),
    modalSampleSelect: document.getElementById('modal-sample-select'),
    modalAssignButton: document.getElementById('modal-assign-button'),
    modalUnassignButton: document.getElementById('modal-unassign-button'),
    
    addResultModal: document.getElementById('add-result-modal'),
    resultModalTitle: document.getElementById('resultModalTitle'),
    resultDefinitionSelect: document.getElementById('result-definition-select'),
    resultValueInput: document.getElementById('result-value-input'),
    scalarInputGroup: document.getElementById('scalarInputGroup'),
    timeSeriesInputGroup: document.getElementById('timeSeriesInputGroup'),
    tsValueInput: document.getElementById('tsValueInput'),
    saveResultButton: document.getElementById('save-result-btn'),
    
    plateResultsTableJq: $('#plate-results-table'),

    // Collapsible
    collapsibleBar: document.getElementById('collapsible-bar'),
    collapsibleContent: document.getElementById('collapsible-actions'),
    collapsibleChevron: document.querySelector('.collapsible-chevron')
  };

  // Bind close buttons
  mainElement.querySelectorAll('.close-button').forEach(btn => {
    btn.addEventListener('click', (e) => {
        e.target.closest('.modal').style.display = 'none';
    });
  });

  // --- 4. CORE FUNCTIONS ---
  
  async function init() {
    try {
        await fetchData();
        renderHeader();
        renderWells();
        renderResultDefinitionsDropdown();
        renderPlateResults();
        setupEventListeners();
        
        switchTab('layout');
        renderDashboard();
    } catch (error) {
        UIUtils.handleError(error, "Failed to load plate data.");
    }
  }

  async function fetchData() {
    const [plateRes, resultDefsRes, resultsRes] = await Promise.all([
      ApiUtils.request(`/api/v1/plates/${state.plateId}`),
      ApiUtils.request('/api/v1/result-definitions'),
      ApiUtils.request(`/api/v1/plates/${state.plateId}/results`)
    ]);

    state.plate = plateRes.plate;
    state.wells = plateRes.wells;
    state.resultDefinitions = resultDefsRes.data;
    state.plateResults = resultsRes.data;

    // Fetch samples after plate is loaded
    const samplesRes = await ApiUtils.request(`/api/v1/projects/${state.plate.project_id}/samples`);
    state.samples = samplesRes.data;
  }

  function renderHeader() {
    dom.plateTitle.textContent = `[${state.plate.short_id || state.plate.id}] ${state.plate.name}`;
  }

  function renderResultDefinitionsDropdown() {
    if (state.resultDefinitions.length > 0) {
      dom.resultDefinitionSelect.innerHTML = '<option value="">Select a definition...</option>' + 
        state.resultDefinitions.map(def => `<option value="${def.id}">${def.name} (${def.data_type})</option>`).join('');
    } else {
      dom.resultDefinitionSelect.innerHTML = '<option value="">No result definitions found</option>';
    }
  }

  function renderWells() {
    const plate_format = state.plate.plate_format;
    let rows, cols;
    dom.wellGrid.className = '';
    
    if (plate_format.startsWith('24')) { [rows, cols] = [4, 6]; dom.wellGrid.classList.add('density-low'); }
    else if (plate_format.startsWith('48')) { [rows, cols] = [6, 8]; dom.wellGrid.classList.add('density-low'); }
    else if (plate_format.startsWith('96')) { [rows, cols] = [8, 12]; dom.wellGrid.classList.add('density-medium'); }
    else if (plate_format.startsWith('384')) { [rows, cols] = [16, 24]; dom.wellGrid.classList.add('density-high'); }
    else { [rows, cols] = [0, 0]; }
    
    dom.wellGrid.style.gridTemplateRows = `repeat(${rows}, 1fr)`;
    dom.wellGrid.style.gridTemplateColumns = `repeat(${cols}, 1fr)`;
    dom.wellGrid.style.gridAutoFlow = 'column';

    dom.wellGrid.innerHTML = state.wells.map(item => {
      const well = item.well;
      const isEdge = item.is_edge;
      const sample = well.sample_id ? state.samples.find(s => s.id === well.sample_id) : null;
      const sampleDisplay = sample ? (sample.short_id || sample.id) : '';

      return `
            <div class="well ${well.sample_id ? 'assigned' : ''} ${isEdge ? 'edge-well' : ''}" 
                 data-coordinate="${well.coordinate}" 
                 title="${well.coordinate}${isEdge ? ' (Edge Well)' : ''}${sample ? ': ' + (sample.short_id || sample.id) : ''}">
              <div class="well-coord">${well.coordinate}</div>
              ${sample ? `<div class="well-sample-id">${sampleDisplay}</div>` : ''}
            </div>`;
    }).join('');
  }

  
  async function renderDashboard() {
    try {
      const res = await ApiUtils.request(`/api/v1/plates/${state.plateId}/dashboard`);
      if (res.columns.length === 0) return;
      
      res.columns.forEach(col => { col.defaultContent = ""; });
      
      // Inject Well coordinate into data based on sample ID
      res.data.forEach(row => {
          if (row.entity_type === "Sample") {
              // state.wells contains objects like { well: { sample_id: 5, coordinate: "A1" } }
              const wellObj = state.wells.find(w => w.well.sample_id === row.id);
              row.well_coordinate = wellObj ? wellObj.well.coordinate : "";
          }
      });

      // Insert Well column right after Short ID
      res.columns.splice(2, 0, { title: "Well", data: "well_coordinate", visible: true, defaultContent: "" });

      if ($.fn.DataTable.isDataTable('#dashboard-table')) {
          $('#dashboard-table').DataTable().destroy();
          $('#dashboard-table').empty();
      }
      
      $('#dashboard-table').DataTable({
          data: res.data,
          columns: res.columns,
          pageLength: 15,
          scrollX: true,
          dom: 'Bfrtip',
          buttons: [
              'colvis',
              {
                  extend: 'csv',
                  text: 'Export Displayed CSV (Wide)',
                  filename: `plate_${state.plateId}_matrix`
              }
          ]
      });
    } catch (e) {
      console.error(e);
      UIUtils.handleError(e, "Failed to load plate dashboard");
    }
  }

  function renderPlateResults() {
    const resultDefMap = Object.fromEntries(state.resultDefinitions.map(def => [def.id, def]));
    
    if ($.fn.DataTable.isDataTable('#plate-results-table')) {
      dom.plateResultsTableJq.DataTable().destroy();
    }

    if (state.plateResults.length === 0) {
      dom.plateResultsTableJq.find('thead').html('<tr><th>Name</th><th>Value</th><th>Updated At</th><th>Actions</th></tr>');
      dom.plateResultsTableJq.find('tbody').html('<tr><td colspan="4" class="text-secondary" style="text-align:center;">No results have been added to this plate yet.</td></tr>');
      return;
    }

    dom.plateResultsTableJq.find('thead').html('<tr><th></th><th>Name</th><th>Value</th><th>Updated At</th><th>Actions</th></tr>');

    const tableBody = state.plateResults.map(res => {
      const def = resultDefMap[res.result_definition_id];
      const defName = def ? def.name : `Def #${res.result_definition_id}`;
      
      let isTimeSeries = false;
      let value = 'N/A';
      let btnHtml = '';
      let rowClass = '';
      
      if (!res.value) {
          value = `<span class="badge" style="background-color:var(--bg-dark-2); color:var(--text-secondary); border: 1px solid var(--border-color);">Pending</span>`;
          btnHtml = `<button class="button action-fill" data-uid="${res.uid}" data-defid="${res.result_definition_id}">Fill</button>`;
      } else if (res.value.type && res.value.type.endsWith('Series')) {
        isTimeSeries = true;
        rowClass = 'has-timeseries';
        value = `[TimeSeries: ${res.value.value ? res.value.value.length : 0} points]`;
        btnHtml = `<button class="button action-append" data-uid="${res.uid}" data-defid="${res.result_definition_id}">Append</button>`;
      } else if (res.value.type && res.value.type.includes('Date')) {
        value = new Date(res.value.value * 1000).toLocaleString();
        btnHtml = `<button class="secondary-button action-edit" data-uid="${res.uid}" data-defid="${res.result_definition_id}">Edit</button>`;
      } else if (res.value.type === 'FileLink') {
        value = `<a href="${res.value.value}" target="_blank">${res.value.value}</a>`;
        btnHtml = `<button class="secondary-button action-edit" data-uid="${res.uid}" data-defid="${res.result_definition_id}">Edit</button>`;
      } else {
        if (res.value.type === 'Float' && typeof res.value.value === 'number') {
            value = Number.isInteger(res.value.value) ? res.value.value.toFixed(1) : res.value.value;
        } else {
            value = res.value.value;
        }
        btnHtml = `<button class="secondary-button action-edit" data-uid="${res.uid}" data-defid="${res.result_definition_id}">Edit</button>`;
      }
      
      const updatedDate = new Date(res.updated_at * 1000).toLocaleString();
      const rawData = encodeURIComponent(JSON.stringify(res.value?.value || []));
      
      return `<tr class="${rowClass}" data-type="${res.value ? res.value.type : ''}" data-raw='${rawData}'>
                <td class="${isTimeSeries ? 'dt-control' : ''}" style="cursor:pointer; width:30px; text-align:center;">${isTimeSeries ? '<span class="dt-caret">&#9656;</span>' : ''}</td>
                <td>${defName}</td>
                <td>${value}</td>
                <td>${updatedDate}</td>
                <td>${btnHtml}</td>
              </tr>`;
    }).join('');
    
    dom.plateResultsTableJq.find('tbody').html(tableBody);
    state.plateResultsTable = dom.plateResultsTableJq.DataTable({ 
        pageLength: 5, 
        lengthChange: false,
        columns: [
            { orderable: false },
            { orderable: true },
            { orderable: true },
            { orderable: true },
            { orderable: false }
        ]
    });
  }

  async function renderDashboard() {
    try {
      const res = await ApiUtils.request(`/api/v1/plates/${state.plateId}/dashboard`);
      if (res.columns.length === 0) return;
      
      res.columns.forEach(col => { col.defaultContent = ""; });
      
      // Inject Well coordinate into data based on sample ID
      res.data.forEach(row => {
          if (row.entity_type === "Sample") {
              // state.wells contains objects like { well: { sample_id: 5, coordinate: "A1" } }
              const wellObj = state.wells.find(w => w.well.sample_id === row.id);
              row.well_coordinate = wellObj ? wellObj.well.coordinate : "";
          }
      });

      // Insert Well column right after Short ID
      res.columns.splice(2, 0, { title: "Well", data: "well_coordinate", visible: true, defaultContent: "" });

      if ($.fn.DataTable.isDataTable('#dashboard-table')) {
          $('#dashboard-table').DataTable().destroy();
          $('#dashboard-table').empty();
      }
      
      $('#dashboard-table').DataTable({
          data: res.data,
          columns: res.columns,
          pageLength: 15,
          scrollX: true,
          dom: 'Bfrtip',
          buttons: [
              'colvis',
              {
                  extend: 'csv',
                  text: 'Export Displayed CSV (Wide)',
                  filename: `plate_${state.plateId}_matrix`
              }
          ]
      });
    } catch (e) {
      console.error(e);
      UIUtils.handleError(e, "Failed to load plate dashboard");
    }
  }

  function switchTab(tabName) {
    document.querySelectorAll('.tab-content').forEach(el => el.style.display = 'none');
    document.querySelectorAll('.tab-btn').forEach(btn => btn.classList.remove('active'));

    document.getElementById(`tab-btn-${tabName}`).classList.add('active');
    document.getElementById(`view-${tabName}`).style.display = 'block';

    if (tabName === 'dashboard' && $.fn.DataTable.isDataTable('#dashboard-table')) {
        $('#dashboard-table').DataTable().columns.adjust().draw();
    }
  }

  // --- 5. EVENT LISTENERS ---

  function toggleCollapsible(expand) {
    const isOpen = dom.collapsibleContent.classList.toggle('open', expand);
    dom.collapsibleChevron.textContent = isOpen ? '▾' : '▸';
    dom.collapsibleBar.querySelector('span:last-child').textContent = isOpen ? 'Hide actions' : 'Show actions';
  }

  function setupEventListeners() {
    dom.collapsibleBar.addEventListener('click', () => toggleCollapsible());

    dom.btnLayout.addEventListener('click', () => switchTab('layout'));
    dom.btnDashboard.addEventListener('click', () => switchTab('dashboard'));
    dom.btnResults.addEventListener('click', () => switchTab('results'));

    // --- Export ---
    dom.exportCsvButton.addEventListener('click', () => dom.exportModal.style.display = 'block');
    dom.confirmExportButton.addEventListener('click', () => {
        const format = dom.exportFormatSelectModal.value;
        window.location.href = `/api/v1/plates/${state.plateId}/export?export_format=${format}&t=${new Date().getTime()}`;
        dom.exportModal.style.display = 'none';
    });
    dom.exportResultsButton.addEventListener('click', () => {
        window.location.href = `/api/v1/plates/${state.plateId}/results?format=csv&t=${new Date().getTime()}`;
    });

    // --- Well Grid Clicks ---
    dom.wellGrid.addEventListener('click', e => {
      const wellDiv = e.target.closest('.well');
      if (!wellDiv) return;

      state.selectedWellCoordinate = wellDiv.dataset.coordinate;
      const wellItem = state.wells.find(w => w.well.coordinate === state.selectedWellCoordinate);
      if (!wellItem) return;
      const well = wellItem.well;

      dom.modalWellCoordinate.textContent = `Well: ${state.selectedWellCoordinate}`;
      const inputGroup = dom.wellModal.querySelector('.input-group');

      if (well.sample_id) {
        const sample = state.samples.find(s => s.id === well.sample_id);
        if (sample) {
          const sampleDisplay = sample.short_id || sample.id;
          dom.modalSampleInfo.innerHTML = `Sample: <a href="/samples/${sample.id}?from=plate&context_id=${state.plateId}&context_name=${state.plate.name}" style="color: var(--color-accent);">${sampleDisplay}</a>`;
        } else {
          dom.modalSampleInfo.textContent = `Sample ID: ${well.sample_id} (not in this project)`;
        }
        inputGroup.style.display = 'none';
        dom.modalUnassignButton.style.display = 'block';
      } else {
        if (state.samples.length === 0) {
          dom.modalSampleSelect.innerHTML = '<option value="">No samples in project</option>';
        } else {
          dom.modalSampleSelect.innerHTML = state.samples.map(s =>
            `<option value="${s.id}">${s.short_id || s.id}</option>`
          ).join('');
        }
        dom.modalSampleInfo.textContent = 'No sample assigned.';
        inputGroup.style.display = 'flex';
        dom.modalUnassignButton.style.display = 'none';
      }
      dom.wellModal.style.display = 'block';
    });

    dom.modalAssignButton.addEventListener('click', handleAssignSample);
    dom.modalUnassignButton.addEventListener('click', handleUnassignSample);

    // --- Add/Edit Results ---
    dom.addResultButton.addEventListener('click', () => {
      state.currentResultUid = null;
      state.isPatchMode = false;
      dom.resultDefinitionSelect.disabled = false;
      dom.resultDefinitionSelect.value = '';
      dom.resultValueInput.value = '';
      dom.tsValueInput.value = '';
      dom.resultModalTitle.textContent = "Add Plate Result";
      dom.resultDefinitionSelect.dispatchEvent(new Event('change'));
      dom.addResultModal.style.display = 'block';
    });

    dom.resultDefinitionSelect.addEventListener('change', handleResultTypeChange);
    dom.saveResultButton.addEventListener('click', handleSaveResult);

    // --- DataTable Events ---
    $('#plate-results-table tbody').on('click', 'td.dt-control', handleDataTableRowExpand);
    $('#plate-results-table tbody').on('click', '.action-fill, .action-edit, .action-append', handleDataTableActionClick);

    // --- Global Window Clicks ---
    // Ensure we don't bind this multiple times if loadWellViewerPage is called repeatedly
    window.onclick = function(event) {
        if (event.target == dom.wellModal) dom.wellModal.style.display = "none";
        if (event.target == dom.addResultModal) dom.addResultModal.style.display = "none";
        if (event.target == dom.exportModal) dom.exportModal.style.display = "none";
    }
  }

  // --- 6. EVENT HANDLERS ---

  async function handleAssignSample() {
    const sampleId = dom.modalSampleSelect.value;
    if (!sampleId) return;
    try {
      await ApiUtils.request(`/api/v1/plates/${state.plateId}/wells/${state.selectedWellCoordinate}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ sample_id: parseInt(sampleId, 10) })
      });
      // Refresh well data
      const data = await ApiUtils.request(`/api/v1/plates/${state.plateId}`);
      state.wells = data.wells;
      renderWells();
      dom.wellModal.style.display = 'none';
      UIUtils.showToast("Sample assigned successfully", "success");
    } catch (error) {
      UIUtils.handleError(error, "Error assigning sample");
    }
  }

  async function handleUnassignSample() {
    try {
      await ApiUtils.request(`/api/v1/plates/${state.plateId}/wells/${state.selectedWellCoordinate}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ sample_id: null })
      });
      // Refresh well data
      const data = await ApiUtils.request(`/api/v1/plates/${state.plateId}`);
      state.wells = data.wells;
      renderWells();
      dom.wellModal.style.display = 'none';
      UIUtils.showToast("Sample unassigned", "info");
    } catch (error) {
      UIUtils.handleError(error, "Error unassigning sample");
    }
  }

  function handleResultTypeChange() {
    const defId = dom.resultDefinitionSelect.value;
    if (!defId) {
        dom.scalarInputGroup.style.display = 'block';
        dom.timeSeriesInputGroup.style.display = 'none';
        return;
    }
    
    const def = state.resultDefinitions.find(d => d.id === parseInt(defId, 10));
    if (!def) return;
    
    const type = def.data_type;

    if (type.endsWith('Series') && state.isPatchMode) {
        dom.scalarInputGroup.style.display = 'none';
        dom.timeSeriesInputGroup.style.display = 'block';
        const inner = type.replace('Series', '');
        if (inner === 'Integer') { dom.tsValueInput.type = 'text'; dom.tsValueInput.inputMode = 'numeric'; }
        else if (inner === 'Float') { dom.tsValueInput.type = 'text'; dom.tsValueInput.inputMode = 'decimal'; }
        else if (inner.includes('Date')) dom.tsValueInput.type = inner === 'Date' ? 'date' : 'datetime-local';
        else dom.tsValueInput.type = 'text';
    } else {
        dom.scalarInputGroup.style.display = 'block';
        dom.timeSeriesInputGroup.style.display = 'none';
        if (type === 'Integer') { dom.resultValueInput.type = 'text'; dom.resultValueInput.inputMode = 'numeric'; }
        else if (type === 'Float') { dom.resultValueInput.type = 'text'; dom.resultValueInput.inputMode = 'decimal'; }
        else if (type === 'Date') dom.resultValueInput.type = 'date';
        else if (type === 'Datetime') dom.resultValueInput.type = 'datetime-local';
        else dom.resultValueInput.type = 'text';
    }
  }

  function handleDataTableRowExpand() {
    const tr = $(this).closest('tr');
    const row = state.plateResultsTable.row(tr);
    
    if (row.child.isShown()) {
        row.child.hide();
        tr.removeClass('shown');
    } else if (tr.hasClass('has-timeseries')) {
        const rawDataStr = tr.attr('data-raw');
        const dataType = tr.attr('data-type');
        const innerType = dataType.replace('Series', '');
        const data = JSON.parse(decodeURIComponent(rawDataStr));
        
        let html = '<div style="padding: 10px; background: var(--bg-dark); border-radius: 4px; display:flex; gap: 20px;">';
        html += '<div style="flex:1; max-height:200px; overflow-y:auto;">';
        html += '<table class="display" style="width:100%; font-size:0.85rem;">';
        html += '<thead><tr><th>Time</th><th>Value</th></tr></thead><tbody>';
        
        const plotData = [];
        data.forEach(pt => {
            let v = pt.value;
            if (innerType === 'Float' && typeof v === 'number') {
                v = Number.isInteger(v) ? v.toFixed(1) : v;
            }
            if (innerType.includes('Date')) v = new Date(v * 1000).toLocaleString();
            else if (innerType === 'FileLink') v = `<a href="${v}" target="_blank">${v}</a>`;
            html += `<tr><td>${pt.time}</td><td>${v}</td></tr>`;
            
            if (typeof pt.value === 'number') {
                plotData.push({ x: pt.time, y: pt.value });
            }
        });        html += '</tbody></table></div>';
        
        const chartId = 'chart-' + Math.random().toString(36).substr(2, 9);
        html += `<div style="flex:1; position:relative; min-height: 200px;"><canvas id="${chartId}"></canvas></div>`;
        html += '</div>';

        row.child(html).show();
        tr.addClass('shown');
        
        if (plotData.length > 0) {
            UIUtils.renderTimeSeriesChart(chartId, plotData);
        }
    }
  }

  function handleDataTableActionClick(e) {
    e.stopPropagation();
    const btn = $(this);
    const uid = btn.data('uid');
    const defId = btn.data('defid');
    
    state.currentResultUid = uid;
    dom.resultDefinitionSelect.value = defId;
    dom.resultDefinitionSelect.disabled = true;
    
    const def = state.resultDefinitions.find(d => d.id === parseInt(defId, 10));
    const type = def ? def.data_type : null;
    const isAppend = btn.hasClass('action-append') || (type && type.endsWith('Series'));
    
    state.isPatchMode = isAppend;
    
    dom.resultDefinitionSelect.dispatchEvent(new Event('change'));
    
    dom.resultModalTitle.textContent = isAppend ? "Append TimeSeries Point" : "Edit Plate Result";
    dom.addResultModal.style.display = 'block';
  }

  async function handleSaveResult() {
    const resultDefinitionId = dom.resultDefinitionSelect.value;
    if (!resultDefinitionId) return UIUtils.showToast('Please select a result type.', 'warning');

    const def = state.resultDefinitions.find(d => d.id === parseInt(resultDefinitionId, 10));
    if (!def) return UIUtils.showToast('Selected result definition not found.', 'error');

    let endpoint = `/api/v1/plates/${state.plateId}/results`;
    let method = 'POST';
    let payload = { result_definition_id: parseInt(resultDefinitionId, 10) };

    try {
        if (state.isPatchMode && def.data_type.endsWith('Series')) {
            const valRaw = dom.tsValueInput.value.trim();
            const innerType = def.data_type.replace('Series', '');
            
            // Use ApiUtils to strict parse the primitive value for appending
            const parsedVal = ApiUtils._parsePrimitive(valRaw, innerType);
            
            payload = { value: { type: innerType, value: parsedVal } };
            endpoint = `/api/v1/results/${state.currentResultUid}/append`;
            method = 'PATCH';
        } else {
            if (state.currentResultUid) {
                endpoint = `/api/v1/results/${state.currentResultUid}`;
                method = 'PUT';
                delete payload.result_definition_id;
            }

            const value = dom.resultValueInput.value.trim();
            if (value) {
                payload.value = ApiUtils.parseResultInput(value, def.data_type);
            }
        }

        const resData = await ApiUtils.request(endpoint, {
            method: method,
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(payload)
        });

        if (!state.currentResultUid) {
            state.plateResults.push(resData);
        } else {
            const idx = state.plateResults.findIndex(r => r.uid === resData.uid);
            if (idx !== -1) state.plateResults[idx] = resData;
        }
        
        renderPlateResults();
        dom.addResultModal.style.display = 'none';
        dom.resultValueInput.value = '';
        dom.tsValueInput.value = '';
        UIUtils.showToast("Result saved successfully", "success");

    } catch (error) {
        UIUtils.handleError(error, "Failed to save result.");
    }
  }

  // --- 7. BOOTSTRAP ---
  init();
}
