/* src/server/static/js/sample-details.js */

async function loadSampleDetailsPage(mainElement, sampleId) {
  const backLinkHtml = generateBackArrow('/samples');

  // --- 1. STATE ---
  const state = {
    sampleId: sampleId,
    sample: null,
    project: null,
    locations: [],
    resultDefs: [],
    sampleResults: [],
    plateResults: [],

    // View modes & forms
    isEditMode: false,
    siblingsCache: null,
    strainsCache: null,
    communitiesCache: null,

    // Name lookup maps
    strainMap: {},
    communityMap: {},
    sampleMap: {},

    // Results Modal
    currentResultUid: null,
    isPatchMode: false,

    // DataTable instances
    resultsTable: null
  };

  // --- 2. HTML SKELETON ---
  mainElement.innerHTML = `
    <div class="page-header">
      <div style="display:flex; align-items:center; gap:15px;">
          ${backLinkHtml}
          <h2>Sample Details</h2>
      </div>
      <div id="action-buttons">
          <button id="edit-btn" class="button">Edit</button>
      </div>
    </div>

    <div id="collapsible-bar" class="collapsible-bar">
      <span class="collapsible-chevron">▸</span> <span>Show actions</span>
    </div>
    <div id="collapsible-actions" class="collapsible-content">
      <button id="delete-btn" class="button" style="background-color: #dc3545; border-color: #dc3545; color: white;">Archive</button>
      <button id="save-btn" class="button" style="display:none; background-color: #28a745; border-color: #28a745;">Save Changes</button>
      <button id="cancel-btn" class="secondary-button" style="display:none;">Cancel</button>
      <button id="addResultBtn" class="button">+ Add Result</button>
    </div>
    
    <div id="sample-metadata-container">Loading...</div>
    <div id="sample-sub-container"></div>

    <div id="resultModal" class="modal" style="display:none;">
      <div class="modal-content">
        <span id="closeModalBtn" class="close-button">&times;</span>
        <h3 id="resultModalTitle">Add Result</h3>
        <div class="form-container">
            <div class="input-group">
                <label>Result Type:</label>
                <div id="resultDefAutocompleteContainer" style="width: 100%;"></div>
            </div>
            
            <div id="scalarInputGroup" class="input-group">
                <label>Value:</label>
                <input type="text" id="valueInput">
            </div>
            
            <div id="timeSeriesInputGroup" class="input-group" style="display:none;">
                <label>Value to Append:</label>
                <input type="text" id="tsValueInput" placeholder="Measurement">
                <small class="text-secondary" style="margin-top:5px; display:block;">Step number (Time) will be auto-assigned</small>
            </div>
            
            <button id="saveResultBtn" class="button">Save Result</button>
        </div>
      </div>
    </div>

    <!-- Replicates Dashboard (Only for Source Samples) -->
    <div id="replicates-container" style="display:none; margin-top: 40px;">
        <h3>Experimental Replicates Matrix</h3>
        <table id="dashboard-table" class="display nowrap" style="width:100%"></table>
    </div>
  `;

  // --- 3. DOM ELEMENTS ---
  const dom = {
    metadataContainer: document.getElementById('sample-metadata-container'),
    subContainer: document.getElementById('sample-sub-container'),
    
    editBtn: document.getElementById('edit-btn'),
    saveBtn: document.getElementById('save-btn'),
    cancelBtn: document.getElementById('cancel-btn'),
    deleteBtn: document.getElementById('delete-btn'),
    addResultBtn: document.getElementById('addResultBtn'),
    
    modal: document.getElementById('resultModal'),
    closeBtn: document.getElementById('closeModalBtn'),
    resultDefAutocompleteContainer: document.getElementById('resultDefAutocompleteContainer'),
    valueInput: document.getElementById('valueInput'),
    scalarInputGroup: document.getElementById('scalarInputGroup'),
    timeSeriesInputGroup: document.getElementById('timeSeriesInputGroup'),
    tsValueInput: document.getElementById('tsValueInput'),
    resultModalTitle: document.getElementById('resultModalTitle'),
    saveResultBtn: document.getElementById('saveResultBtn'),

    // Collapsible
    collapsibleBar: document.getElementById('collapsible-bar'),
    collapsibleContent: document.getElementById('collapsible-actions'),
    collapsibleChevron: document.querySelector('.collapsible-chevron')
  };

  // --- 4. CORE FUNCTIONS ---

  async function init() {
    try {
        await fetchData();
        renderViewMode();
        
        state.resultDefAutocomplete = new AutocompleteComponent(dom.resultDefAutocompleteContainer, {
          placeholder: 'Search for result definition...',
          fetchItems: async (query) => {
            const res = await fetch(`/api/v1/result-definitions?search=${encodeURIComponent(query)}`);
            const json = await res.json();
            return (json.data || []).map(d => ({
              id: d.id,
              label: `${d.name} (${d.data_type})`,
              dataType: d.data_type
            }));
          },
          onSelect: (item) => {
             state.selectedResultDefId = item ? item.id : null;
             state.selectedResultDataType = item ? item.dataType : null;
             handleResultTypeChange();
          }
        });

        renderSubContainer(); // Locations and Tables
        setupEventListeners();

        if (state.sample.category === 'Source') {
            document.getElementById('replicates-container').style.display = 'block';
            await renderDashboard();
        }

    } catch (error) {
        UIUtils.handleError(error, "Failed to load sample data.");
        dom.metadataContainer.innerHTML = `<div class="error">Error: ${error.message}</div>`;
    }
  }

  async function fetchData() {
    const [sJson, locsRes, defsRes, strainsRes, communitiesRes] = await Promise.all([
        ApiUtils.request(`/api/v1/samples/${state.sampleId}`),
        ApiUtils.request(`/api/v1/samples/${state.sampleId}/locations`),
        ApiUtils.request(`/api/v1/result-definitions`),
        ApiUtils.request('/api/v1/strains'),
        ApiUtils.request('/api/v1/communities')
    ]);

    state.sample = sJson.sample;
    state.project = sJson.project;
    state.sampleResults = sJson.results;
    state.locations = locsRes.data || [];
    state.resultDefs = defsRes.data || [];
    state.strainsCache = strainsRes.data || [];
    state.communitiesCache = communitiesRes.data || [];
    state.strainMap = Object.fromEntries((strainsRes.data || []).map(s => [s.id, s.strain_name]));
    state.communityMap = Object.fromEntries((communitiesRes.data || []).map(c => [c.id, c.name]));

    // Pre-load siblings for parent reference resolution
    const siblingsRes = await ApiUtils.request(`/api/v1/projects/${state.project.id}/samples`);
    state.siblingsCache = siblingsRes.data || [];
    state.sampleMap = Object.fromEntries(state.siblingsCache.map(s => [s.id, s.short_id]));

    // Fetch plate results if assigned to plates
    const uniquePlateIds = [...new Set(state.locations.map(l => l.plate_id))];
    const plateResultsPromises = uniquePlateIds.map(pid => 
        ApiUtils.request(`/api/v1/plates/${pid}/results`)
            .then(j => ({ plateId: pid, results: j.data || [] }))
            .catch(() => ({ plateId: pid, results: [] })) // Silently fail individual plates
    );
    state.plateResults = await Promise.all(plateResultsPromises);
  }

  
  async function renderDashboard() {
    try {
      const res = await ApiUtils.request(`/api/v1/samples/${state.sampleId}/children/dashboard`);
      if (res.columns.length === 0) return;
      
      res.columns.forEach(col => {
          col.defaultContent = "";
          if (col.title === "Well_Pos") col.title = "Well Position";
          if (col.title === "Time_Point") col.title = "Time Point";
      });
      
      if ($.fn.DataTable.isDataTable('#dashboard-table')) {
          $('#dashboard-table').DataTable().destroy();
          $('#dashboard-table').empty();
      }
      
      $('#dashboard-table').DataTable({
          data: res.data,
          columns: res.columns,
          pageLength: 10,
          scrollX: true,
          dom: 'Bfrtip',
          buttons: [
              'colvis',
              {
                  extend: 'csv',
                  text: 'Export Displayed CSV (Wide)',
                  filename: `sample_${state.sampleId}_replicates_matrix`,
                  exportOptions: {
                      format: {
                          header: function ( data, columnIdx ) {
                              if (data === "Well Position") return "Well_Pos";
                              if (data === "Time Point") return "Time_Point";
                              return data;
                          }
                      }
                  }
              }
          ]
      });
    } catch (e) {
      console.error(e);
      // We don't error out hard if this fails, just log it.
    }
  }

  function renderViewMode() {
    state.isEditMode = false;
    const s = state.sample;
    const p = state.project;

    let parentHtml = '<span class="text-secondary">None (Source Material)</span>';
    if (s.parent_sample_id) {
      const parentName = state.sampleMap[s.parent_sample_id] || 'Sample ' + s.parent_sample_id;
      parentHtml = `<a href="/samples/${s.parent_sample_id}?from=sample&context_id=${s.id}&context_name=${encodeURIComponent(s.short_id)}" class="badge"><span class="emoji">🧪</span> ${UIUtils.escapeHTML(parentName)}</a>`;
    }

    let strainHtml = '<span class="text-secondary">None</span>';
    if (s.strain_id) {
      strainHtml = `<a href="/strains/${s.strain_id}?from=sample&context_id=${s.id}&context_name=${encodeURIComponent(s.short_id)}" class="badge"><span class="emoji">🧬</span> ${UIUtils.escapeHTML(state.strainMap[s.strain_id] || 'Strain ' + s.strain_id)}</a>`;
    }

    let communityHtml = '<span class="text-secondary">None</span>';
    if (s.community_id) {
      communityHtml = `<a href="/communities/${s.community_id}?from=sample&context_id=${s.id}&context_name=${encodeURIComponent(s.short_id)}" class="badge"><span class="emoji">🦠</span> ${UIUtils.escapeHTML(state.communityMap[s.community_id] || 'Community ' + s.community_id)}</a>`;
    }

    dom.metadataContainer.innerHTML = `
        <div class="info-grid">
            <div class="info-item"><span class="info-key">Short ID</span><span class="info-value">${UIUtils.escapeHTML(s.short_id || s.id)}</span></div>
            <div class="info-item"><span class="info-key">Project</span><span class="info-value"><a href="/projects/${p.id}">${UIUtils.escapeHTML(p.name)}</a></span></div>
            <div class="info-item"><span class="info-key">Type</span><span class="info-value">${UIUtils.escapeHTML(s.sample_type)}</span></div>
            <div class="info-item"><span class="info-key">Category</span><span class="info-value">${UIUtils.escapeHTML(s.category)}</span></div>
            <div class="info-item"><span class="info-key">Parent</span><span class="info-value">${parentHtml}</span></div>
            <div class="info-item"><span class="info-key">Strain</span><span class="info-value">${strainHtml}</span></div>
            <div class="info-item"><span class="info-key">Community</span><span class="info-value">${communityHtml}</span></div>
            <div class="info-item"><span class="info-key">Status</span><span class="info-value"><span class="status-pill status-${UIUtils.escapeHTML(s.status.toLowerCase())}">${UIUtils.escapeHTML(s.status)}</span></span></div>
            <div class="info-item"><span class="info-key">Created</span><span class="info-value">${new Date(s.created_at * 1000).toLocaleString()}</span></div>
        </div>
    `;

    dom.editBtn.style.display = 'inline-block';
    dom.deleteBtn.style.display = 'inline-block';
    dom.saveBtn.style.display = 'none';
    dom.cancelBtn.style.display = 'none';
    toggleCollapsible(false);
  }

  let parentAutocomplete = null;
  let strainAutocomplete = null;
  let communityAutocomplete = null;

  async function renderEditMode() {
    state.isEditMode = true;
    const s = state.sample;
    const p = state.project;

    dom.metadataContainer.innerHTML = `
        <div class="form-container">
            <div class="input-group">
                <label>Sample Type</label>
                <input type="text" id="edit-type" value="${UIUtils.escapeHTML(s.sample_type)}">
            </div>
            <div class="input-group">
                <label>Category</label>
                <div class="select-wrapper">
                    <select id="edit-category">
                        <option value="Source" ${s.category === 'Source' ? 'selected' : ''}>Source</option>
                        <option value="Experimental" ${s.category === 'Experimental' ? 'selected' : ''}>Experimental</option>
                    </select>
                </div>
            </div>
            <div class="input-group">
                <label>Parent Sample</label>
                <div id="edit-parent-container"></div>
            </div>
            <div class="input-group">
                <label>Strain</label>
                <div id="edit-strain-container"></div>
            </div>
            <div class="input-group">
                <label>Community</label>
                <div id="edit-community-container"></div>
            </div>
        </div>
    `;

    const parentContainer = document.getElementById('edit-parent-container');
    const strainContainer = document.getElementById('edit-strain-container');
    const communityContainer = document.getElementById('edit-community-container');

    const initialParent = s.parent_sample_id ? { id: s.parent_sample_id, label: `${state.sampleMap[s.parent_sample_id] || s.parent_sample_id}` } : null;
    const initialStrain = s.strain_id ? { id: s.strain_id, label: `${state.strainMap[s.strain_id] || s.strain_id}` } : null;
    const initialCommunity = s.community_id ? { id: s.community_id, label: `${state.communityMap[s.community_id] || s.community_id}` } : null;

    parentAutocomplete = new AutocompleteComponent(parentContainer, {
      placeholder: 'Search for parent sample...',
      initialValue: initialParent,
      fetchItems: async (query) => {
        const res = await fetch(`/api/v1/projects/${p.id}/samples?search=${encodeURIComponent(query)}&category=Source`);
        const json = await res.json();
        return (json.data || []).filter(sib => sib.id !== s.id).map(sib => ({
          id: sib.id,
          label: `[${sib.short_id || sib.id}] ${sib.sample_type}`
        }));
      }
    });

    strainAutocomplete = new AutocompleteComponent(strainContainer, {
      placeholder: 'Search for strain...',
      initialValue: initialStrain,
      fetchItems: async (query) => {
        const res = await fetch(`/api/v1/strains?search=${encodeURIComponent(query)}`);
        const json = await res.json();
        return (json.data || []).map(st => ({
          id: st.id,
          label: st.strain_name
        }));
      }
    });

    communityAutocomplete = new AutocompleteComponent(communityContainer, {
      placeholder: 'Search for community...',
      initialValue: initialCommunity,
      fetchItems: async (query) => {
        const res = await fetch(`/api/v1/communities?search=${encodeURIComponent(query)}`);
        const json = await res.json();
        return (json.data || []).map(c => ({
          id: c.id,
          label: c.name
        }));
      }
    });

    const catEl = document.getElementById('edit-category');
    catEl.onchange = () => {
      const isSource = catEl.value === 'Source';
      if (parentAutocomplete && parentAutocomplete.inputElement) {
         parentAutocomplete.inputElement.disabled = isSource;
         if (isSource) {
            parentAutocomplete.selectItem(null);
            parentAutocomplete.inputElement.value = '';
         }
      }
    };
    catEl.dispatchEvent(new Event('change'));

    dom.editBtn.style.display = 'none';
    dom.deleteBtn.style.display = 'none';
    dom.saveBtn.style.display = 'inline-block';
    dom.cancelBtn.style.display = 'inline-block';
    toggleCollapsible(true);
  }

  function renderSubContainer() {
    let html = '<div class="details-grid" style="display: grid; grid-template-columns: 1fr 1fr; gap: 2rem; margin-top: 2rem;">';

    // --- Locations Column ---
    html += `<div><h3>Plate Locations</h3>`;
    if (state.locations.length === 0) html += `<p class="text-secondary">No plate assignments.</p>`;
    else {
        html += `<ul>${state.locations.map(l => 
            `<li><a href="/plates/${l.plate_id}?from=sample&context_id=${state.sampleId}&context_name=${encodeURIComponent(state.sample.short_id || state.sample.id)}">Plate #${l.plate_id}</a> (Well ${l.well_coordinate})</li>`
        ).join('')}</ul>`;
    }
    html += `
    </div>`;

    // --- Sample Results Column ---
    html += `<div><h3>Sample Results</h3>
            <table id="results-table" class="display" style="width:100%">
                <thead><tr><th></th><th>Definition</th><th>Value</th><th>Actions</th></tr></thead>
                <tbody>
                    ${state.sampleResults.map(r => {
                      const rd = state.resultDefs.find(d => d.id === r.result_definition_id);
                      let isTimeSeries = false;
                      let val = r.value?.value;
                      let btnHtml = '';
                      let rowClass = '';
                      
                      if (!r.value) {
                          val = `<span class="badge" style="background-color:var(--bg-dark-2); color:var(--text-secondary); border: 1px solid var(--border-color);">Pending</span>`;
                          btnHtml = `<button class="button action-fill" data-uid="${r.uid}" data-defid="${r.result_definition_id}">Fill</button>`;
                      } else if (r.value?.type?.endsWith('Series')) {
                        isTimeSeries = true;
                        rowClass = 'has-timeseries';
                        val = `[TimeSeries: ${val.length} points]`;
                        btnHtml = `<button class="button action-append" data-uid="${r.uid}" data-defid="${r.result_definition_id}">Append</button>`;
                      } else if (r.value?.type?.includes('Date')) {
                        val = new Date(val * 1000).toLocaleString();
                        btnHtml = `<button class="secondary-button action-edit" data-uid="${r.uid}" data-defid="${r.result_definition_id}">Edit</button>`;
                      } else if (r.value?.type === 'FileLink') {
                        val = `<a href="${val}" target="_blank">${val}</a>`;
                        btnHtml = `<button class="secondary-button action-edit" data-uid="${r.uid}" data-defid="${r.result_definition_id}">Edit</button>`;
                      } else {
                        if (r.value?.type === 'Float' && typeof val === 'number') {
                            val = Number.isInteger(val) ? val.toFixed(1) : val;
                        }
                        val = val !== undefined ? val : 'N/A';
                        btnHtml = `<button class="secondary-button action-edit" data-uid="${r.uid}" data-defid="${r.result_definition_id}">Edit</button>`;
                      }
                      
                      const rawData = encodeURIComponent(JSON.stringify(r.value?.value || []));
                      return `<tr class="${rowClass}" data-type="${r.value?.type || ''}" data-raw='${rawData}'>
                                <td class="${isTimeSeries ? 'dt-control' : ''}" style="cursor:pointer; width:30px; text-align:center;">${isTimeSeries ? '<span class="dt-caret">&#9656;</span>' : ''}</td>
                                <td>${rd?.name || 'Unknown'}</td>
                                <td>${val}</td>
                                <td>${btnHtml}</td>
                              </tr>`;
                    }).join('')}
                </tbody>
            </table></div>
    </div>`;

    // --- Plate Results Table ---
    if (state.plateResults.length > 0 && state.plateResults.some(pr => pr.results.length > 0)) {
        html += `
        <div style="margin-top: 2rem;">
            <h3>Plate Level Results</h3>
            <table id="plate-results-table" class="display" style="width:100%">
                <thead><tr><th>Plate ID</th><th>Result Type</th><th>Value</th><th>Updated At</th></tr></thead>
                <tbody>
        `;
        state.plateResults.forEach(plateRes => {
            plateRes.results.forEach(r => {
                const rd = state.resultDefs.find(d => d.id === r.result_definition_id);
                let val = r.value?.value;
                if (r.value?.type?.endsWith('Series')) {
                  val = `[TimeSeries: ${val.length} points]`;
                } else if (r.value?.type?.includes('Date')) {
                  val = new Date(val * 1000).toLocaleString();
                } else {
                  if (r.value?.type === 'Float' && typeof val === 'number') {
                      val = Number.isInteger(val) ? val.toFixed(1) : val;
                  }
                  val = val !== undefined ? val : 'N/A';
                }
                html += `<tr>
                    <td><a href="/plates/${plateRes.plateId}">Plate #${plateRes.plateId}</a></td>
                    <td>${rd?.name || 'Unknown'}</td>
                    <td>${val}</td>
                    <td>${new Date(r.updated_at * 1000).toLocaleString()}</td>
                </tr>`;
            });
        });
        html += `</tbody></table>
    </div>`;
    }

    dom.subContainer.innerHTML = html;

    // --- DataTable Initializations ---
    if (state.sampleResults.length > 0) {
      if ($.fn.DataTable.isDataTable('#results-table')) $('#results-table').DataTable().destroy();
      
      state.resultsTable = $('#results-table').DataTable({ 
        pageLength: 5, 
        lengthChange: false, 
        searching: false,
        columns: [{ orderable: false }, { orderable: true }, { orderable: true }, { orderable: false }]
      });
      
      $('#results-table tbody').on('click', 'td.dt-control', handleDataTableRowExpand);
      $('#results-table tbody').on('click', '.action-fill, .action-edit, .action-append', handleDataTableActionClick);
    }

    if (state.plateResults.some(pr => pr.results.length > 0)) {
        if ($.fn.DataTable.isDataTable('#plate-results-table')) $('#plate-results-table').DataTable().destroy();
        $('#plate-results-table').DataTable({ pageLength: 5, lengthChange: false });
    }
  }

  function toggleCollapsible(expand) {
    const isOpen = dom.collapsibleContent.classList.toggle('open', expand);
    dom.collapsibleChevron.textContent = isOpen ? '▾' : '▸';
    dom.collapsibleBar.querySelector('span:last-child').textContent = isOpen ? 'Hide actions' : 'Show actions';
  }

  // --- 5. EVENT LISTENERS ---

  function setupEventListeners() {
    dom.editBtn.onclick = renderEditMode;
    dom.cancelBtn.onclick = renderViewMode;

    dom.collapsibleBar.onclick = () => toggleCollapsible();
    
    dom.deleteBtn.onclick = handleDeleteSample;
    dom.saveBtn.onclick = handleSaveSampleEdits;

    dom.addResultBtn.onclick = () => {
        state.currentResultUid = null;
        state.isPatchMode = false;
        if (state.resultDefAutocomplete) {
            state.resultDefAutocomplete.selectItem(null);
            if (state.resultDefAutocomplete.inputElement) {
                state.resultDefAutocomplete.inputElement.disabled = false;
                state.resultDefAutocomplete.inputElement.value = '';
            }
        }
        state.selectedResultDefId = null;
        state.selectedResultDataType = null;
        dom.valueInput.value = '';
        dom.tsValueInput.value = '';
        dom.resultModalTitle.textContent = "Add Result";
        handleResultTypeChange();
        dom.modal.style.display = 'block';
    };

    dom.closeBtn.onclick = () => dom.modal.style.display = 'none';
    window.onclick = (e) => { if (e.target == dom.modal) dom.modal.style.display = 'none'; };

    dom.saveResultBtn.onclick = handleSaveResult;
  }

  // --- 6. EVENT HANDLERS ---

  async function handleDeleteSample() {
    if (!confirm("Archive this sample?")) return;
    try {
        await ApiUtils.request(`/api/v1/samples/${state.sampleId}`, { method: 'DELETE' });
        UIUtils.showToast("Sample archived", "success");
        setTimeout(() => {
            history.pushState({}, "", "/samples");
            handleRouting();
        }, 500);
    } catch (e) {
        UIUtils.handleError(e, "Failed to archive sample");
    }
  }

  async function handleSaveSampleEdits() {
    const payload = {
      sample_type: document.getElementById('edit-type').value,
      category: document.getElementById('edit-category').value,
      parent_sample_id: parentAutocomplete ? parentAutocomplete.getValue() : null,
      strain_id: strainAutocomplete ? strainAutocomplete.getValue() : null,
      community_id: communityAutocomplete ? communityAutocomplete.getValue() : null
    };

    try {
        await ApiUtils.request(`/api/v1/samples/${state.sampleId}`, {
            method: 'PUT',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(payload)
        });
        UIUtils.showToast("Sample updated successfully", "success");
        // Re-fetch and re-render
        await fetchData();
        renderViewMode();
    } catch (e) {
        UIUtils.handleError(e, "Failed to update sample");
    }
  }

  function handleResultTypeChange() {
    const type = state.selectedResultDataType;
    
    if (!type) {
        dom.scalarInputGroup.style.display = 'block';
        dom.timeSeriesInputGroup.style.display = 'none';
        return;
    }

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
        if (type === 'Integer') { dom.valueInput.type = 'text'; dom.valueInput.inputMode = 'numeric'; }
        else if (type === 'Float') { dom.valueInput.type = 'text'; dom.valueInput.inputMode = 'decimal'; }
        else if (type === 'Date') dom.valueInput.type = 'date';
        else if (type === 'Datetime') dom.valueInput.type = 'datetime-local';
        else dom.valueInput.type = 'text';
    }
  }

  function handleDataTableRowExpand() {
    const tr = $(this).closest('tr');
    const row = state.resultsTable.row(tr);
    
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
            
            // Only plot numbers
            if (typeof pt.value === 'number') {
                plotData.push({ x: pt.time, y: pt.value });
            }
        });        html += '</tbody></table></div>';
        
        const chartId = 'chart-' + Math.random().toString(36).substr(2, 9);
        html += `<div style="flex:1; position:relative; min-height: 200px;"><canvas id="${chartId}"></canvas>
    </div>`;

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
      const uid = $(this).data('uid');
      const defId = $(this).data('defid');
      
      state.currentResultUid = uid;
      
      const def = state.resultDefs.find(d => d.id === parseInt(defId, 10));
      const type = def ? def.data_type : null;
      
      if (state.resultDefAutocomplete) {
          state.resultDefAutocomplete.selectItem({
              id: defId,
              label: def ? `${def.name} (${def.data_type})` : `Unknown (${defId})`,
              dataType: type
          });
          if (state.resultDefAutocomplete.inputElement) {
              state.resultDefAutocomplete.inputElement.disabled = true;
          }
      }
      state.selectedResultDefId = defId;
      state.selectedResultDataType = type;

      const isAppend = $(this).hasClass('action-append') || (type && type.endsWith('Series'));
      
      state.isPatchMode = isAppend;
      
      handleResultTypeChange();
      
      dom.resultModalTitle.textContent = isAppend ? "Append TimeSeries Point" : "Edit Result";
      dom.modal.style.display = 'block';
  }

  async function handleSaveResult() {
    const defId = state.selectedResultDefId;
    const type = state.selectedResultDataType;
    if (!defId) return UIUtils.showToast("Result Definition required", "warning");

    let endpoint = `/api/v1/samples/${state.sampleId}/results`;
    let method = 'POST';
    let payload = { result_definition_id: parseInt(defId, 10) };

    try {
        if (state.isPatchMode && type && type.endsWith('Series')) {
            const valRaw = dom.tsValueInput.value.trim();
            const innerType = type.replace('Series', '');
            
            // Validate using API Utils
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

            const raw = dom.valueInput.value.trim();
            if (raw) {
                payload.value = ApiUtils.parseResultInput(raw, type);
            }
        }

        const resData = await ApiUtils.request(endpoint, {
          method: method,
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(payload)
        });

        UIUtils.showToast("Result saved successfully", "success");
        dom.modal.style.display = 'none';

        // Re-fetch and re-render the tables
        await fetchData();
        renderSubContainer();

    } catch (error) {
        UIUtils.handleError(error, "Failed to save result");
    }
  }

  // --- 7. BOOTSTRAP ---
  init();
}
