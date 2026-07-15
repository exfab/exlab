/* src/server/static/js/project-details.js */

async function loadProjectDetailsPage(mainElement, projectId) {
  const backLinkHtml = generateBackArrow('/projects');

  // --- 1. STATE ---
  const state = {
    projectId: projectId,
    project: null,
    metadataTemplate: [],
    allSamples: [],
    plates: [],
    users: [],
    isAdmin: false,
    strainMap: {},
    communityMap: {},
    sampleMap: {}
  };

  // --- 2. HTML SKELETON ---
  mainElement.innerHTML = `
      <div class="page-header">
        <div style="display: flex; align-items: center; gap: 1rem;">
          ${backLinkHtml}
          <h2>Project Details</h2>
        </div>
        <div id="action-buttons">
          <button id="edit-btn" class="button">Edit</button>
        </div>
      </div>

      <div id="collapsible-bar" class="collapsible-bar">
        <span class="collapsible-chevron">▸</span> <span>Show actions</span>
      </div>
      <div id="collapsible-actions" class="collapsible-content">
        <button id="save-btn" class="button" style="display:none; background-color: #28a745; border-color: #28a745;">Save</button>
        <button id="cancel-btn" class="secondary-button" style="display:none;">Cancel</button>
        <button onclick="UploadService.openWizard('results', '${projectId}')" class="button secondary-button">Bulk Import Results</button>
        <button onclick="UploadService.openWizard('bulk-samples', '${projectId}')" class="button secondary-button">Bulk Import Samples</button>
        <button onclick="UploadService.openWizard('bulk-plates', '${projectId}')" class="button secondary-button">Bulk Plate Actions</button>
        <button onclick="MultiPlatePlanner.openWizard('${projectId}')" class="button" style="background-color: #28a745; border-color: #28a745; white-space: nowrap;">Plan Multiple Plates</button>
      </div>
      
      <div id="project-info-container">Loading details...</div>
  
      <div class="tab-bar">
          <button id="tab-btn-sources" class="tab-btn">Sources</button>
          <button id="tab-btn-experimental" class="tab-btn">Experimental</button>
          <button id="tab-btn-plates" class="tab-btn">Plates</button>
          <button id="tab-btn-dashboard" class="tab-btn">Results Dashboard</button>
          <button id="tab-btn-team" class="tab-btn">Team</button>
          <button id="tab-btn-workflows" class="tab-btn">Workflows</button>
      </div>
      
      
      <div id="view-dashboard" class="tab-content" style="display: none;">
          <div class="table-header">
              <h3>Results Dashboard</h3>
              <div class="action-bar">
                <a id="export-dashboard-btn" class="button secondary-button" href="#">Export Full CSV (Long Format)</a>
              </div>
          </div>
          <table id="dashboard-table" class="display nowrap" style="width:100%">
          </table>
      </div>

      <div id="view-team" class="tab-content" style="display: none;">
          <div class="table-header">
              <h3>Project Team</h3>
              <div id="admin-team-controls" style="display: none;">
                  <form id="add-user-form" style="display: flex; gap: 0.5rem; align-items: center;">
                      <div id="user-autocomplete-container" style="width: 250px;"></div>
                      <button type="submit" class="button">Add User</button>
                  </form>
              </div>
          </div>
          <table id="team-table" class="display" style="width:100%">
            <thead>
              <tr>
                <th>Email</th>
                <th>Role</th>
                <th>Assigned At</th>
                <th id="th-team-actions" style="display: none;">Actions</th>
              </tr>
            </thead>
            <tbody></tbody>
          </table>
      </div>

      <div id="view-sources" class="tab-content" style="display: none;">
          <div class="table-header">
             <h3>Source Materials</h3>
             <div class="action-bar">
                <div class="bulk-toolbar">
                    <select id="bulk-action-sources">
                        <option value="">Bulk Actions...</option>
                        <option value="status_Pending">Set Status: Pending</option>
                        <option value="status_Active">Set Status: Active</option>
                        <option value="status_Completed">Set Status: Completed</option>
                        <option value="archive">Archive Selected</option>
                    </select>
                    <button id="btn-apply-sources" class="button secondary-button">Apply</button>
                </div>
                <div style="width: 1px; height: 24px; background: var(--border-color); margin: 0 10px;"></div>
                <button id="btn-open-add-strains" class="button" style="background-color: var(--color-accent);">Add from Strains</button>
                <a id="export-sources-btn" class="button secondary-button" href="#">Export Sources CSV</a>
             </div>
          </div>
          <table id="sources-table" class="display" style="width:100%">
            <thead>
                <tr>
                    <th style="width: 30px; text-align:center;"><input type="checkbox" id="cb-all-sources"></th>
                    <th>Short ID</th>
                    <th>Type</th>
                    <th>Strain</th>
                    <th>Created</th>
                </tr>
            </thead>
            <tbody></tbody>
          </table>
      </div>

      <div id="view-experimental" class="tab-content" style="display: none;">
          <div class="table-header">
              <h3>Experimental Samples</h3>
              <div class="action-bar">
                  <div class="bulk-toolbar">
                    <select id="bulk-action-experimental">
                        <option value="">Bulk Actions...</option>
                        <option value="status_Pending">Set Status: Pending</option>
                        <option value="status_Active">Set Status: Active</option>
                        <option value="status_Completed">Set Status: Completed</option>
                        <option value="archive">Archive Selected</option>
                    </select>
                    <button id="btn-apply-experimental" class="button secondary-button">Apply</button>
                  </div>
                  <div style="width: 1px; height: 24px; background: var(--border-color); margin: 0 10px;"></div>
                  <button id="btn-open-add-exp-from-sources" class="button" style="background-color: var(--color-accent);">Create from Sources</button>
                  <a id="export-samples-btn" class="button secondary-button" href="#">Export Experimental CSV</a>
              </div>
          </div>
          <table id="experimental-table" class="display" style="width:100%">
            <thead>
              <tr>
                <th style="width: 30px; text-align:center;"><input type="checkbox" id="cb-all-experimental"></th>
                <th>Short ID</th>
                <th>Parent</th>
                <th>Status</th>
                <th>Created</th>
              </tr>
            </thead>
            <tbody></tbody>
          </table>
      </div>

      <div id="view-plates" class="tab-content" style="display: none;">
          <div class="table-header">
              <h3>Plates List</h3>
              <div class="action-bar">
                <div class="bulk-toolbar">
                    <select id="bulk-action-plates">
                        <option value="">Bulk Actions...</option>
                        <option value="print">Print Selected Labels</option>
                        <option value="transfer-map">Generate Transfer Map</option>
                    </select>
                    <button id="btn-apply-plates" class="button secondary-button">Apply</button>
                </div>
                <div style="width: 1px; height: 24px; background: var(--border-color); margin: 0 10px;"></div>
                <a id="export-plates-btn" class="button secondary-button" href="#">Export CSV</a>
              </div>
          </div>
          <table id="plates-table" class="display" style="width:100%">
            <thead>
              <tr>
                <th style="width: 30px; text-align:center;"><input type="checkbox" id="cb-all-plates"></th>
                <th>Short ID</th>
                <th>Name</th>
                <th>Format</th>
                <th>Created</th>
              </tr>
            </thead>
            <tbody></tbody>
          </table>
      </div>

      <div id="view-workflows" class="tab-content" style="display: none;">
          <h3>Active Workflows</h3>
          <div class="placeholder">No active workflows.</div>
      </div>

      <div id="add-strains-modal" class="modal" style="display:none;">
          <div class="modal-content" style="max-width: 900px;">
              <span class="close-button" onclick="document.getElementById('add-strains-modal').style.display='none'">&times;</span>
              <h3>Add Source Samples from Strains</h3>
              <p class="text-secondary">Search for strains in the database to pull into this project as source samples.</p>
              
              <div style="margin-bottom: 15px;">
                <label style="display:block; font-weight:bold; margin-bottom:5px;">Sample Type (Applies to all):</label>
                <div class="select-wrapper">
                    <select id="add-strains-sample-type" style="width:100%;">
                        <option value="Liquid Cell Culture" selected>Liquid Cell Culture</option>
                        <option value="Solid Cell Culture">Solid Cell Culture</option>
                        <option value="Library Sample">Library Sample</option>
                        <option value="Unknown">Unknown</option>
                    </select>
                </div>
              </div>

              <div style="margin-bottom: 15px;">
                  <label style="display:block; font-weight:bold; margin-bottom:5px;">Search Strains:</label>
                  <div style="display: flex; gap: 10px; margin-bottom: 10px;">
                      <div class="select-wrapper" style="flex: 1;">
                          <select id="add-strains-genus-filter" style="width: 100%;">
                              <option value="">All Genera</option>
                          </select>
                      </div>
                      <div class="select-wrapper" style="flex: 1;">
                          <select id="add-strains-species-filter" style="width: 100%;">
                              <option value="">All Species</option>
                          </select>
                      </div>
                  </div>
                  <div style="display: flex; gap: 10px; margin-bottom: 10px;">
                      <input type="text" id="add-strains-search-input" placeholder="Type strain name..." style="flex: 1; font-size: 1rem; padding: 8px;">
                      <button id="add-strains-search-btn" class="button secondary-button">Search</button>
                  </div>
                  <div id="add-strains-table-container" style="display: none; max-height: 200px; overflow-y: auto; border: 1px solid var(--border-color); border-radius: 4px;">
                      <table id="add-strains-table" class="display" style="width:100%; font-size: 0.9em; margin: 0; border-collapse: collapse;">
                          <thead style="background: var(--bg-dark); position: sticky; top: 0;">
                              <tr style="border-bottom: 2px solid var(--border-color);">
                                  <th style="padding: 8px; text-align: left;">Action</th>
                                  <th style="padding: 8px; text-align: left;">Strain Name</th>
                                  <th style="padding: 8px; text-align: left;">Species</th>
                              </tr>
                          </thead>
                          <tbody id="add-strains-tbody"></tbody>
                      </table>
                      <div id="add-strains-loading" style="display:none; padding: 10px; text-align: center; color: var(--text-secondary);">Searching...</div>
                      <div id="add-strains-empty" style="display:none; padding: 10px; text-align: center; color: var(--text-secondary);">No strains found.</div>
                  </div>
              </div>
              
              <h4>Staged Strains</h4>
              <div id="staged-strains-list" style="border: 1px solid var(--border-color); border-radius: 4px; padding: 10px; min-height: 100px; margin-bottom: 15px; max-height: 250px; overflow-y: auto;">
                  <div class="text-secondary" style="text-align:center; padding: 20px;">No strains staged. Search and select above to add.</div>
              </div>
              
              <div style="display: flex; justify-content: flex-end; gap: 10px;">
                  <button class="secondary-button" onclick="document.getElementById('add-strains-modal').style.display='none'">Cancel</button>
                  <button id="btn-confirm-add-strains" class="button">Create Samples</button>
              </div>
          </div>
      </div>
      <div id="add-exp-from-sources-modal" class="modal" style="display:none;">
          <div class="modal-content" style="max-width: 900px;">
              <span class="close-button" onclick="document.getElementById('add-exp-from-sources-modal').style.display='none'">&times;</span>
              <h3>Create Experimental Samples from Sources</h3>
              <p class="text-secondary">Select source samples and specify how many experimental replicates to generate for each.</p>
              
              <div style="margin-bottom: 15px;">
                <label style="display:block; font-weight:bold; margin-bottom:5px;">Default Replicates (Applies to all selected):</label>
                <input type="number" id="add-exp-default-replicates" value="1" min="1" style="width: 100px; padding: 5px;">
                <button id="btn-apply-replicates" class="button secondary-button" style="margin-left: 10px;">Apply to Selected</button>
              </div>

              <div id="add-exp-table-container" style="max-height: 400px; overflow-y: auto; border: 1px solid var(--border-color); border-radius: 4px; margin-bottom: 15px;">
                  <table id="add-exp-table" class="display" style="width:100%; font-size: 0.9em; margin: 0; border-collapse: collapse;">
                      <thead style="background: var(--bg-dark); position: sticky; top: 0;">
                          <tr style="border-bottom: 2px solid var(--border-color);">
                              <th style="padding: 8px; text-align: center; width: 40px;"><input type="checkbox" id="add-exp-select-all"></th>
                              <th style="padding: 8px; text-align: left;">Source ID</th>
                              <th style="padding: 8px; text-align: left;">Type</th>
                              <th style="padding: 8px; text-align: left;">Strain</th>
                              <th style="padding: 8px; text-align: center; width: 100px;">Replicates</th>
                          </tr>
                      </thead>
                      <tbody id="add-exp-tbody"></tbody>
                  </table>
                  <div id="add-exp-empty" style="display:none; padding: 20px; text-align: center; color: var(--text-secondary);">No source samples available.</div>
              </div>
              
              <div style="display: flex; justify-content: flex-end; gap: 10px;">
                  <button class="secondary-button" onclick="document.getElementById('add-exp-from-sources-modal').style.display='none'">Cancel</button>
                  <button id="btn-confirm-add-exp" class="button">Create Experimental Samples</button>
              </div>
          </div>
      </div>
    `;

  // --- 3. DOM ELEMENTS ---
  const dom = {
    infoContainer: document.getElementById('project-info-container'),
    editBtn: document.getElementById('edit-btn'),
    saveBtn: document.getElementById('save-btn'),
    cancelBtn: document.getElementById('cancel-btn'),
    
    // Tabs
    btnSources: document.getElementById('tab-btn-sources'),
    btnExperimental: document.getElementById('tab-btn-experimental'),
    btnPlates: document.getElementById('tab-btn-plates'),
    btnDashboard: document.getElementById('tab-btn-dashboard'),
    btnTeam: document.getElementById('tab-btn-team'),
    btnWorkflows: document.getElementById('tab-btn-workflows'),
    
    // Exports
    exportSamplesBtn: document.getElementById('export-samples-btn'),
    exportSourcesBtn: document.getElementById('export-sources-btn'),
    exportPlatesBtn: document.getElementById('export-plates-btn'),
    exportDashboardBtn: document.getElementById('export-dashboard-btn'),
    
    // Bulk applies
    applySourcesBtn: document.getElementById('btn-apply-sources'),
    applyExpBtn: document.getElementById('btn-apply-experimental'),
    applyPlatesBtn: document.getElementById('btn-apply-plates'),
    
    // Team
    adminTeamControls: document.getElementById('admin-team-controls'),
    thTeamActions: document.getElementById('th-team-actions'),
    addUserForm: document.getElementById('add-user-form'),
    userAutocompleteContainer: document.getElementById('user-autocomplete-container'),
    teamTableBody: document.querySelector('#team-table tbody'),

    // Add Strains Modal
    btnOpenAddStrains: document.getElementById('btn-open-add-strains'),
    btnConfirmAddStrains: document.getElementById('btn-confirm-add-strains'),
    addStrainsModal: document.getElementById('add-strains-modal'),
    addStrainsSearchInput: document.getElementById('add-strains-search-input'),
    addStrainsSearchBtn: document.getElementById('add-strains-search-btn'),
    addStrainsTableContainer: document.getElementById('add-strains-table-container'),
    addStrainsTbody: document.getElementById('add-strains-tbody'),
    addStrainsLoading: document.getElementById('add-strains-loading'),
    addStrainsEmpty: document.getElementById('add-strains-empty'),
    addStrainsSampleType: document.getElementById('add-strains-sample-type'),
    addStrainsGenusFilter: document.getElementById('add-strains-genus-filter'),
    addStrainsSpeciesFilter: document.getElementById('add-strains-species-filter'),
    stagedStrainsList: document.getElementById('staged-strains-list'),

    // Add Exp from Sources Modal
    btnOpenAddExpFromSources: document.getElementById('btn-open-add-exp-from-sources'),
    btnConfirmAddExp: document.getElementById('btn-confirm-add-exp'),
    addExpModal: document.getElementById('add-exp-from-sources-modal'),
    addExpTbody: document.getElementById('add-exp-tbody'),
    addExpEmpty: document.getElementById('add-exp-empty'),
    addExpSelectAll: document.getElementById('add-exp-select-all'),
    addExpDefaultReplicates: document.getElementById('add-exp-default-replicates'),
    btnApplyReplicates: document.getElementById('btn-apply-replicates'),

    // Collapsible
    collapsibleBar: document.getElementById('collapsible-bar'),
    collapsibleContent: document.getElementById('collapsible-actions'),
    collapsibleChevron: document.querySelector('.collapsible-chevron')
  };

  // --- 4. CORE FUNCTIONS ---

  async function init() {
    try {
        const [projectRes, templateRes] = await Promise.all([
            ApiUtils.request(`/api/v1/projects/${state.projectId}`),
            ApiUtils.request('/api/v1/settings/project_metadata_template').catch(() => []) // Fallback if missing
        ]);
        
        state.project = projectRes;
        if (Array.isArray(templateRes)) state.metadataTemplate = templateRes;
        
        dom.exportSamplesBtn.href = `/api/v1/projects/${state.projectId}/samples?format=csv&category=Experimental`;
        dom.exportSourcesBtn.href = `/api/v1/projects/${state.projectId}/samples?format=csv&category=Source`;
        dom.exportPlatesBtn.href = `/api/v1/projects/${state.projectId}/plates?format=csv`;

        renderInfoView();
        await fetchTablesData();
        renderTables();
        await fetchAndRenderTeam();
        setupEventListeners();
        
        switchTab('sources');
        renderDashboard();
        dom.exportDashboardBtn.href = `/api/v1/projects/${state.projectId}/results/export`;

    } catch (error) {
        UIUtils.handleError(error, "Failed to load project details.");
        dom.infoContainer.innerHTML = `<div class="error">Error loading project</div>`;
    }
  }

  async function fetchTablesData() {
    const [samplesRes, platesRes, strainsRes, communitiesRes] = await Promise.all([
        ApiUtils.request(`/api/v1/projects/${state.projectId}/samples`),
        ApiUtils.request(`/api/v1/projects/${state.projectId}/plates`),
        ApiUtils.request('/api/v1/strains'),
        ApiUtils.request('/api/v1/communities')
    ]);

    state.allSamples = samplesRes.data || [];
    state.plates = platesRes.data || [];
    state.sampleMap = Object.fromEntries((samplesRes.data || []).map(s => [s.id, s]));
    state.strainMap = Object.fromEntries((strainsRes.data || []).map(s => [s.id, s.strain_name]));
    state.communityMap = Object.fromEntries((communitiesRes.data || []).map(c => [c.id, c.name]));
  }

  function renderInfoView() {
    const p = state.project;
    
    // Build metadata rows safely
    let metadataHtml = '';
    if (p.metadata && typeof p.metadata === 'object' && Object.keys(p.metadata).length > 0) {
      Object.keys(p.metadata).forEach(key => {
        const val = typeof p.metadata[key] === 'object' ? JSON.stringify(p.metadata[key]) : p.metadata[key];
        metadataHtml += `<div class="info-item"><span class="info-key">${key}</span><span class="info-value">${val || '-'}</span></div>`;
      });
    }

    dom.infoContainer.innerHTML = `
        <div class="info-grid">
          <div class="info-item"><span class="info-key">ID</span><span class="info-value">${p.short_id || p.id}</span></div>
          <div class="info-item"><span class="info-key">Name</span><span class="info-value">${p.name}</span></div>
          <div class="info-item"><span class="info-key">Description</span><span class="info-value">${p.description || '-'}</span></div>
          <div class="info-item"><span class="info-key">Contact</span><span class="info-value">${p.contact_name || '-'}</span></div>
          <div class="info-item"><span class="info-key">Status</span><span class="info-value"><span class="status-pill status-${p.status.toLowerCase()}">${p.status}</span></span></div>
          ${metadataHtml}
        </div>`;
    dom.editBtn.style.display = 'inline-block';
    dom.saveBtn.style.display = 'none';
    dom.cancelBtn.style.display = 'none';
    toggleCollapsible(false);
  }

  function renderEditInfoView() {
    const p = state.project;

    // Combine existing metadata with missing template keys
    const combinedKeys = new Set();
    const currentMeta = (p.metadata && typeof p.metadata === 'object') ? p.metadata : {};
    
    // Add existing keys first
    Object.keys(currentMeta).forEach(k => combinedKeys.add(k));
    // Then add template keys (they will only append if not already there)
    state.metadataTemplate.forEach(def => combinedKeys.add(def.key));

    let metadataRowsHtml = '';
    Array.from(combinedKeys).forEach(key => {
        const val = currentMeta[key] !== undefined ? currentMeta[key] : '';
        const displayVal = typeof val === 'object' ? JSON.stringify(val) : val;
        
        // Find the definition if it exists in the template
        const def = state.metadataTemplate.find(d => d.key === key);
        const isLocked = !!def;
        
        let valueInputHtml = '';
        if (def && typeof def.field_type === 'object' && def.field_type[0] === 'Enum') {
            const options = def.field_type[1];
            const optionsHtml = options.map(opt => `<option value="${opt}" ${opt === displayVal ? 'selected' : ''}>${opt}</option>`).join('');
            valueInputHtml = `
                <select class="meta-value" style="flex-grow: 1; margin-bottom: 0;">
                    <option value="">Select a value...</option>
                    ${optionsHtml}
                </select>`;
        } else {
             valueInputHtml = `<input type="text" class="meta-value" placeholder="Value" value="${displayVal}" style="flex-grow: 1;">`;
        }
        
        const rowId = 'edit-meta-' + Math.random().toString(36).substr(2, 5);
        metadataRowsHtml += `
            <div id="${rowId}" style="display: flex; gap: 10px; align-items: center; margin-bottom: 5px;">
                <input type="text" class="meta-key" placeholder="Key" value="${key}" ${isLocked ? 'readonly style="background-color: var(--bg-dark); cursor: not-allowed; width: 120px;"' : 'style="width: 120px;"'}>
                ${valueInputHtml}
            </div>`;
    });

    dom.infoContainer.innerHTML = `
        <div class="info-grid">
          <div class="info-item"><span class="info-key">ID</span><span class="info-value">${p.short_id || p.id}</span></div>
          <div class="info-item"><span class="info-key">Name</span><input type="text" id="edit-name" value="${p.name}"></div>
          <div class="info-item"><span class="info-key">Description</span><textarea id="edit-desc" rows="3">${p.description || ''}</textarea></div>
          <div class="info-item"><span class="info-key">Contact</span><input type="text" id="edit-contact" value="${p.contact_name || ''}"></div>
          <div class="info-item"><span class="info-key">Status</span>
            <select id="edit-status">
              <option value="Pending" ${p.status === 'Pending' ? 'selected' : ''}>Pending</option>
              <option value="Active" ${p.status === 'Active' ? 'selected' : ''}>Active</option>
              <option value="Completed" ${p.status === 'Completed' ? 'selected' : ''}>Completed</option>
              <option value="Archived" ${p.status === 'Archived' ? 'selected' : ''}>Archived</option>
            </select>
          </div>
        </div>
        
        <div style="margin-top: 1.5rem; padding-top: 1rem; border-top: 1px solid var(--border-color);">
            <h4 style="margin-bottom: 10px;">Metadata</h4>
            <div id="edit-metadata-container">
                ${metadataRowsHtml}
            </div>
        </div>`;
        
    dom.editBtn.style.display = 'none';
    dom.saveBtn.style.display = 'inline-block';
    dom.cancelBtn.style.display = 'inline-block';
    toggleCollapsible(true);
  }

  
  async function renderDashboard() {
    try {
      const res = await ApiUtils.request(`/api/v1/projects/${state.projectId}/dashboard`);
      if (res.columns.length === 0) return; // Empty project
      
      res.columns.forEach(col => { col.defaultContent = ""; });
      
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
                  filename: `project_${state.projectId}_matrix`
              }
          ]
      });
    } catch (e) {
      console.error(e);
      UIUtils.handleError(e, "Failed to load dashboard");
    }
  }

  function renderTables() {
    const activeSamples = state.allSamples.filter(s => s.status !== 'Archived');
    const sourceSamples = activeSamples.filter(s => s.category === 'Source');
    const expSamples = activeSamples.filter(s => s.category === 'Experimental');

    const fill = (id, rows, mapFn, sortCol = 4) => {
      const tbl = document.getElementById(id);
      const tbody = tbl.querySelector('tbody');
      const colCount = tbl.querySelector('thead tr').children.length;

      if (rows.length === 0) {
        tbody.innerHTML = `<tr><td colspan="${colCount}" style="text-align:center; padding:20px; color:var(--text-secondary);">No items found.</td></tr>`;
      } else {
        tbody.innerHTML = rows.map(mapFn).join('');
        $(tbl).DataTable({
          pageLength: 10,
          lengthChange: false,
          order: [[sortCol, "desc"]],
          destroy: true
        });
      }
    };

    fill('sources-table', sourceSamples, s => `
        <tr>
            <td style="text-align:center;"><input type="checkbox" class="row-cb cb-source-row" value="${s.id}"></td>
            <td><a href="/samples/${s.id}?from=project&context_id=${state.projectId}&context_name=${state.project.name}" class="id-link">${s.short_id}</a></td>
            <td>${s.sample_type}</td>
            <td>${s.strain_id ? `<a href="/strains/${s.strain_id}?from=project&context_id=${state.projectId}&context_name=${state.project.name}" class="badge"><span class="emoji">🧬</span> ${state.strainMap[s.strain_id] || 'Strain ' + s.strain_id}</a>` : s.community_id ? `<a href="/communities/${s.community_id}?from=project&context_id=${state.projectId}&context_name=${state.project.name}" class="badge"><span class="emoji">🦠</span> ${state.communityMap[s.community_id] || 'Community ' + s.community_id}</a>` : '-'}</td>
            <td data-sort="${s.created_at}">${new Date(s.created_at * 1000).toLocaleDateString()}</td>
        </tr>`, 4);

    fill('experimental-table', expSamples, s => `
        <tr>
            <td style="text-align:center;"><input type="checkbox" class="row-cb cb-exp-row" value="${s.id}"></td>
            <td><a href="/samples/${s.id}?from=project&context_id=${state.projectId}&context_name=${state.project.name}" class="id-link">${s.short_id}</a></td>
            <td>${s.parent_sample_id ? `<a href="/samples/${s.parent_sample_id}?from=project&context_id=${state.projectId}&context_name=${state.project.name}" class="badge"><span class="emoji">🧪</span> ${state.sampleMap[s.parent_sample_id]?.short_id || 'Sample ' + s.parent_sample_id}</a>` : '-'}</td>
            <td><span class="status-pill status-${s.status.toLowerCase()}">${s.status}</span></td>
            <td data-sort="${s.created_at}">${new Date(s.created_at * 1000).toLocaleDateString()}</td>
        </tr>`, 4);

    fill('plates-table', state.plates, p => `
        <tr>
            <td style="text-align:center;"><input type="checkbox" class="row-cb cb-plate-row" value="${p.id}"></td>
            <td><a href="/plates/${p.id}?from=project&context_id=${state.projectId}&context_name=${state.project.name}" class="id-link">${p.short_id || p.id}</a></td>
            <td><a href="/plates/${p.id}?from=project&context_id=${state.projectId}&context_name=${state.project.name}" class="badge"><span class="emoji">🧫</span> ${p.name}</a></td>
            <td><small>${p.plate_format}</small></td>
            <td data-sort="${p.created_at}">${new Date(p.created_at * 1000).toLocaleDateString()}</td>
        </tr>`, 3);

    bindSelectAll('cb-all-sources', 'cb-source-row', 'sources-table');
    bindSelectAll('cb-all-experimental', 'cb-exp-row', 'experimental-table');
    bindSelectAll('cb-all-plates', 'cb-plate-row', 'plates-table');
  }

  let userAutocomplete = null;

  async function fetchAndRenderTeam() {
    try {
        const [usersRes, meRes] = await Promise.all([
            ApiUtils.request(`/api/v1/projects/${state.projectId}/users`),
            ApiUtils.request('/api/v1/me')
        ]);
        
        state.users = usersRes.data || [];
        state.isAdmin = meRes.role === 'admin' || meRes.role === 'lab_manager';

        if (state.isAdmin) {
            dom.adminTeamControls.style.display = 'block';
            dom.thTeamActions.style.display = 'table-cell';

            if (!userAutocomplete && dom.userAutocompleteContainer) {
                userAutocomplete = new AutocompleteComponent(dom.userAutocompleteContainer, {
                    placeholder: 'Type to search users...',
                    fetchItems: async (query) => {
                        const res = await ApiUtils.request(`/api/v1/users?search=${encodeURIComponent(query)}`);
                        return (res.data || []).map(u => ({ id: u.email, label: u.email }));
                    }
                });
            }
        }

        dom.teamTableBody.innerHTML = state.users.map(u => `
            <tr>
                <td>${UIUtils.escapeHTML(u.email)}</td>
                <td><span class="badge">${UIUtils.escapeHTML(u.role)}</span></td>
                <td>-</td>
                ${state.isAdmin ? `<td><button onclick="removeUserFromProject('${UIUtils.escapeHTML(u.email)}')" class="button secondary-button" style="padding: 2px 8px; font-size: 12px;">Remove</button></td>` : ''}
            </tr>
        `).join('');
        
        window.removeUserFromProject = handleRemoveUser;
        
    } catch (error) {
        UIUtils.handleError(error, "Failed to load team data");
    }
  }

  // --- 5. EVENT HANDLERS ---

  function toggleCollapsible(expand) {
    const isOpen = dom.collapsibleContent.classList.toggle('open', expand);
    dom.collapsibleChevron.textContent = isOpen ? '▾' : '▸';
    dom.collapsibleBar.querySelector('span:last-child').textContent = isOpen ? 'Hide actions' : 'Show actions';
  }

  function setupEventListeners() {
    dom.editBtn.onclick = renderEditInfoView;
    dom.cancelBtn.onclick = renderInfoView;
    dom.saveBtn.onclick = handleSaveProjectInfo;

    dom.collapsibleBar.onclick = () => toggleCollapsible();

    dom.btnDashboard.onclick = () => switchTab('dashboard');
    dom.btnSources.onclick = () => switchTab('sources');
    dom.btnExperimental.onclick = () => switchTab('experimental');
    dom.btnPlates.onclick = () => switchTab('plates');
    dom.btnTeam.onclick = () => switchTab('team');
    dom.btnWorkflows.onclick = () => switchTab('workflows');

    dom.applySourcesBtn.onclick = () => handleBulkAction('sources-table', 'bulk-action-sources');
    dom.applyExpBtn.onclick = () => handleBulkAction('experimental-table', 'bulk-action-experimental');
    dom.applyPlatesBtn.onclick = () => handleBulkAction('plates-table', 'bulk-action-plates');

    dom.addUserForm.onsubmit = handleAddUser;
    
    dom.btnOpenAddStrains.onclick = openAddStrainsModal;
    dom.btnConfirmAddStrains.onclick = handleConfirmAddStrains;
    
    if (dom.btnOpenAddExpFromSources) dom.btnOpenAddExpFromSources.onclick = openAddExpFromSourcesModal;
    if (dom.btnConfirmAddExp) dom.btnConfirmAddExp.onclick = handleConfirmAddExp;
  }

  async function handleSaveProjectInfo() {
    const metadata = {};
    const metaContainer = document.getElementById('edit-metadata-container');
    if (metaContainer) {
        metaContainer.querySelectorAll('div').forEach(row => {
            const keyInput = row.querySelector('.meta-key');
            const valInput = row.querySelector('.meta-value');
            if (keyInput && valInput) {
                const key = keyInput.value.trim();
                const val = valInput.value.trim();
                if (key) metadata[key] = val;
            }
        });
    }

    const payload = {
      name: document.getElementById('edit-name').value,
      description: document.getElementById('edit-desc').value || null,
      contact_name: document.getElementById('edit-contact').value || null,
      status: document.getElementById('edit-status').value,
      metadata: Object.keys(metadata).length > 0 ? metadata : null
    };

    // Strip nulls
    Object.keys(payload).forEach(k => payload[k] === null && delete payload[k]);

    try {
      dom.saveBtn.disabled = true;
      dom.saveBtn.textContent = 'Saving...';
      
      const res = await ApiUtils.request(`/api/v1/projects/${state.projectId}`, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });
      
      state.project = res;
      renderInfoView();
      UIUtils.showToast("Project updated", "success");
    } catch (e) {
      UIUtils.handleError(e, "Failed to save project updates");
    } finally {
      dom.saveBtn.disabled = false;
      dom.saveBtn.textContent = 'Save';
    }
  }

  async function handleBulkAction(tableId, dropdownId) {
    const action = document.getElementById(dropdownId).value;
    if (!action) return UIUtils.showToast("Please select an action.", "warning");
    
    const dt = $(`#${tableId}`).DataTable();
    const checkedBoxes = dt.$('.row-cb:checked');
    if (checkedBoxes.length === 0) return UIUtils.showToast("No items selected.", "warning");

    const ids = Array.from(checkedBoxes).map(cb => cb.value);
    
    if (action.startsWith('status_')) {
      const newStatus = action.split('_')[1];
      if (!confirm(`Change status to ${newStatus} for ${ids.length} item(s)?`)) return;
      try {
        await Promise.all(ids.map(id => {
          const sample = state.allSamples.find(s => s.id === parseInt(id, 10));
          return ApiUtils.request(`/api/v1/samples/${id}`, {
            method: 'PUT',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
              sample_type: sample.sample_type,
              category: sample.category,
              parent_sample_id: sample.parent_sample_id || null,
              strain_id: sample.strain_id || null,
              community_id: null,
              status: newStatus
            })
          });
        }));
        UIUtils.showToast(`Updated ${ids.length} items`, "success");
        await fetchTablesData();
        renderTables();
      } catch (error) {
        UIUtils.handleError(error, "Bulk status update failed.");
      }
    } else if (action === 'archive') {
      if (!confirm(`Archive ${ids.length} item(s)?`)) return;
      try {
        await Promise.all(ids.map(id => ApiUtils.request(`/api/v1/samples/${id}`, { method: 'DELETE' })));
        UIUtils.showToast(`Archived ${ids.length} items`, "success");
        await fetchTablesData();
        renderTables();
      } catch (error) {
        UIUtils.handleError(error, "Bulk archive failed.");
      }
    } else if (action === 'print') {
      const type = tableId === 'plates-table' ? 'plates' : 'samples';
      const selectedData = ids.map(id => {
        const item = type === 'plates' 
            ? state.plates.find(p => p.id === parseInt(id, 10)) 
            : state.allSamples.find(s => s.id === parseInt(id, 10));
            
        return {
          id: id,
          short_id: item.short_id || item.id,
          name: item.name || 'Unknown',
          project_name: state.project.name
        };
      });
      PrintService.renderPreview(type, selectedData);
    } else if (action === 'transfer-map') {
      if (tableId !== 'plates-table') return;
      if (ids.length < 2) return UIUtils.showToast("Select at least 2 plates for a transfer map.", "warning");
      
      const selectedPlates = ids.map(id => state.plates.find(p => p.id === parseInt(id, 10)));
      
      // Determine source plate. Default to the one starting with S- if there's exactly one.
      const sourcePlates = selectedPlates.filter(p => (p.short_id || "").startsWith("S-") || p.category === "Source");
      
      let sourcePlateId = null;
      if (sourcePlates.length === 1) {
          sourcePlateId = sourcePlates[0].id;
      } else {
          // In a real app we'd open a modal, but for simplicity let's prompt or pick the first if none found
          const plateListText = selectedPlates.map((p, i) => `${i+1}. ${p.name} (${p.short_id})`).join('\n');
          const choice = prompt(`Select the SOURCE plate by typing its number (1-${selectedPlates.length}):\n\n${plateListText}`);
          const idx = parseInt(choice, 10) - 1;
          if (isNaN(idx) || idx < 0 || idx >= selectedPlates.length) {
              return UIUtils.showToast("Transfer map cancelled or invalid source plate.", "info");
          }
          sourcePlateId = selectedPlates[idx].id;
      }
      
      const destPlateIds = selectedPlates.filter(p => p.id !== sourcePlateId).map(p => p.id);
      
      const endpoint = `/api/v1/projects/${state.projectId}/plates/transfer-map?format=csv`;
      
      try {
          // We must send a POST request with the source and dest ids, and trigger a download.
          const res = await fetch(endpoint, {
              method: 'POST',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({ source_plate_id: sourcePlateId, destination_plate_ids: destPlateIds })
          });
          
          if (!res.ok) {
              const errText = await res.text();
              let errMessage = 'Failed to generate transfer map';
              try {
                  const errJson = JSON.parse(errText);
                  errMessage = errJson.error || errMessage;
              } catch (parseError) {
                  errMessage = errText || `HTTP ${res.status} ${res.statusText}`;
              }
              throw new Error(errMessage);
          }
          
          // Trigger download
          const blob = await res.blob();
          const url = window.URL.createObjectURL(blob);
          const a = document.createElement('a');
          a.style.display = 'none';
          a.href = url;
          a.download = `transfer_map_project_${state.projectId}.csv`;
          document.body.appendChild(a);
          a.click();
          window.URL.revokeObjectURL(url);
          
      } catch (err) {
          UIUtils.handleError(err, "Failed to generate transfer map");
      }
    }
  }

  async function handleAddUser(e) {
    e.preventDefault();
    const email = userAutocomplete ? userAutocomplete.getValue() : null;
    if (!email) return UIUtils.showToast("Please select a user to add", "warning");

    try {
        await ApiUtils.request(`/api/v1/projects/${state.projectId}/users`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ email })
        });
        
        // Reset the autocomplete input
        if (userAutocomplete && userAutocomplete.inputElement) {
            userAutocomplete.inputElement.value = '';
        }
        
        await fetchAndRenderTeam();
        UIUtils.showToast("User added to project", "success");
    } catch (e) { 
        UIUtils.handleError(e, "Failed to add user");
    }
  }

  async function handleRemoveUser(email) {
    if (!confirm(`Remove user ${email} from this project?`)) return;
    try {
        await ApiUtils.request(`/api/v1/projects/${state.projectId}/users/${email}`, { method: 'DELETE' });
        await fetchAndRenderTeam();
        UIUtils.showToast("User removed", "info");
    } catch (e) { 
        UIUtils.handleError(e, "Failed to remove user"); 
    }
  }

  // --- Add Strains Modal Logic ---
  let stagedStrains = [];

  async function openAddStrainsModal() {
      dom.addStrainsModal.style.display = 'block';
      
      if (dom.addStrainsGenusFilter && !dom.addStrainsGenusFilter.dataset.loaded) {
          try {
              const res = await ApiUtils.request('/api/v1/strains');
              const strains = res.data || [];
              const genera = [...new Set(strains.map(s => s.genus))].filter(Boolean).sort();
              const species = [...new Set(strains.map(s => s.species))].filter(Boolean).sort();
              
              genera.forEach(g => {
                  const opt = document.createElement('option');
                  opt.value = g; opt.textContent = g;
                  dom.addStrainsGenusFilter.appendChild(opt);
              });
              species.forEach(s => {
                  const opt = document.createElement('option');
                  opt.value = s; opt.textContent = s;
                  dom.addStrainsSpeciesFilter.appendChild(opt);
              });
              dom.addStrainsGenusFilter.dataset.loaded = "true";
          } catch (err) {
              console.error("Failed to load reference data for strains modal", err);
          }
      }

      if (!dom.addStrainsSearchBtn.dataset.bound) {
          dom.addStrainsSearchBtn.addEventListener('click', performStrainSearch);
          dom.addStrainsSearchInput.addEventListener('keypress', (e) => {
              if (e.key === 'Enter') performStrainSearch();
          });
          dom.addStrainsGenusFilter.addEventListener('change', performStrainSearch);
          dom.addStrainsSpeciesFilter.addEventListener('change', performStrainSearch);
          dom.addStrainsSearchBtn.dataset.bound = "true";
      }

      stagedStrains = [];
      renderStagedStrains();
      performStrainSearch(); // initial search
  }

  async function performStrainSearch() {
      const query = dom.addStrainsSearchInput.value.trim();
      const genus = dom.addStrainsGenusFilter.value;
      const species = dom.addStrainsSpeciesFilter.value;

      dom.addStrainsTableContainer.style.display = 'block';
      dom.addStrainsTbody.innerHTML = '';
      dom.addStrainsEmpty.style.display = 'none';
      dom.addStrainsLoading.style.display = 'block';

      let url = `/api/v1/strains?search=${encodeURIComponent(query)}`;
      if (genus) url += `&genus=${encodeURIComponent(genus)}`;
      if (species) url += `&species=${encodeURIComponent(species)}`;

      try {
          const res = await ApiUtils.request(url);
          const strains = res.data || [];
          
          dom.addStrainsLoading.style.display = 'none';
          
          if (strains.length === 0) {
              dom.addStrainsEmpty.textContent = "No strains found.";
              dom.addStrainsEmpty.style.display = 'block';
              return;
          }

          // Show top 5 matches
          const displayStrains = strains.slice(0, 5);
          dom.addStrainsTbody.innerHTML = displayStrains.map(s => `
              <tr style="border-bottom: 1px solid var(--border-color);">
                  <td style="padding: 8px;">
                      <button class="button secondary-button" style="padding: 2px 8px; font-size: 12px;" onclick="stageStrain(${s.id}, '${UIUtils.escapeHTML(s.strain_name)}', '${UIUtils.escapeHTML(s.species)}')">Add</button>
                  </td>
                  <td style="padding: 8px;">${UIUtils.escapeHTML(s.strain_name)}</td>
                  <td style="padding: 8px;"><em>${UIUtils.escapeHTML(s.species)}</em></td>
              </tr>
          `).join('');
      } catch (err) {
          console.error("Search failed", err);
          dom.addStrainsLoading.style.display = 'none';
          dom.addStrainsEmpty.textContent = "Error searching strains.";
          dom.addStrainsEmpty.style.display = 'block';
      }
  }

  window.stageStrain = (id, name, species) => {
      const existing = stagedStrains.find(s => s.id === id);
      if (existing) {
          existing.count += 1;
      } else {
          stagedStrains.push({ id, label: `${name} (${species})`, count: 1 });
      }
      renderStagedStrains();
  };

  window.updateStagedStrainCount = (id, delta) => {
      const strain = stagedStrains.find(s => s.id === id);
      if (strain) {
          strain.count += delta;
          if (strain.count <= 0) {
              removeStagedStrain(id);
          } else {
              renderStagedStrains();
          }
      }
  };

  function renderStagedStrains() {
      if (stagedStrains.length === 0) {
          dom.stagedStrainsList.innerHTML = '<div class="text-secondary" style="text-align:center; padding: 20px;">No strains staged. Search and select above to add.</div>';
          return;
      }
      
      dom.stagedStrainsList.innerHTML = stagedStrains.map(s => `
          <div style="display: flex; justify-content: space-between; align-items: center; padding: 8px; border-bottom: 1px solid var(--border-color);">
              <span>${UIUtils.escapeHTML(s.label)}</span>
              <div style="display: flex; align-items: center; gap: 10px;">
                  <div style="display: flex; align-items: center; gap: 5px; background: var(--bg-dark-1); padding: 2px 5px; border-radius: 4px;">
                      <button class="secondary-button" style="padding: 2px 8px; font-weight: bold; border: none; background: transparent;" onclick="updateStagedStrainCount(${s.id}, -1)">-</button>
                      <span style="min-width: 24px; text-align: center; font-weight: bold;">${s.count}</span>
                      <button class="secondary-button" style="padding: 2px 8px; font-weight: bold; border: none; background: transparent;" onclick="updateStagedStrainCount(${s.id}, 1)">+</button>
                  </div>
                  <button class="secondary-button" style="padding: 2px 8px; font-size: 12px; border-color: #dc3545; color: #dc3545;" onclick="removeStagedStrain(${s.id})">Remove</button>
              </div>
          </div>
      `).join('');
  }

  window.removeStagedStrain = (id) => {
      stagedStrains = stagedStrains.filter(s => s.id !== id);
      renderStagedStrains();
  };

  async function handleConfirmAddStrains() {
      if (stagedStrains.length === 0) return UIUtils.showToast("No strains staged.", "warning");
      
      const sampleType = dom.addStrainsSampleType.value;
      dom.btnConfirmAddStrains.disabled = true;
      dom.btnConfirmAddStrains.textContent = 'Creating...';
      
      const payloads = [];
      stagedStrains.forEach(s => {
          for (let i = 0; i < s.count; i++) {
              payloads.push({
                  sample_type: sampleType,
                  category: "Source",
                  strain_id: parseInt(s.id, 10)
              });
          }
      });
      
      try {
          await ApiUtils.request(`/api/v1/projects/${state.projectId}/samples/bulk`, {
              method: 'POST',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({ samples: payloads })
          });
          UIUtils.showToast(`Successfully created ${payloads.length} source samples!`, "success");
          dom.addStrainsModal.style.display = 'none';
          await fetchTablesData();
          renderTables();
      } catch (e) {
          UIUtils.handleError(e, "Failed to create source samples");
      } finally {
          dom.btnConfirmAddStrains.disabled = false;
          dom.btnConfirmAddStrains.textContent = 'Create Samples';
      }
  }

  // --- Add Experimental Samples from Sources Modal Logic ---
  
  function openAddExpFromSourcesModal() {
      dom.addExpModal.style.display = 'block';
      updateAddExpTable();
  }

  function updateAddExpTable() {
      const sourceSamples = state.allSamples.filter(s => s.category === 'Source' && s.status !== 'Archived');
      dom.addExpSelectAll.checked = false;

      if (sourceSamples.length === 0) {
          dom.addExpEmpty.style.display = 'block';
          dom.addExpTbody.innerHTML = '';
          return;
      }

      dom.addExpEmpty.style.display = 'none';
      dom.addExpTbody.innerHTML = sourceSamples.map(s => {
          const strainLabel = s.strain_id ? (state.strainMap[s.strain_id] || `Strain ${s.strain_id}`) : '-';
          return `
          <tr style="border-bottom: 1px solid var(--border-color);">
              <td style="padding: 8px; text-align: center;"><input type="checkbox" class="add-exp-cb" value="${s.id}"></td>
              <td style="padding: 8px;">${UIUtils.escapeHTML(s.short_id || s.id.toString())}</td>
              <td style="padding: 8px;">${UIUtils.escapeHTML(s.sample_type || '-')}</td>
              <td style="padding: 8px;">${UIUtils.escapeHTML(strainLabel)}</td>
              <td style="padding: 8px; text-align: center;"><input type="number" class="add-exp-count" data-id="${s.id}" value="${dom.addExpDefaultReplicates.value}" min="1" style="width: 60px; padding: 4px;"></td>
          </tr>
          `;
      }).join('');
      
      const cbs = document.querySelectorAll('.add-exp-cb');
      cbs.forEach(cb => {
          cb.addEventListener('change', () => {
              const allChecked = Array.from(cbs).every(c => c.checked);
              const someChecked = Array.from(cbs).some(c => c.checked);
              dom.addExpSelectAll.checked = allChecked;
              dom.addExpSelectAll.indeterminate = someChecked && !allChecked;
          });
      });
  }

  dom.addExpSelectAll.addEventListener('change', (e) => {
      document.querySelectorAll('.add-exp-cb').forEach(cb => cb.checked = e.target.checked);
  });

  dom.btnApplyReplicates.addEventListener('click', () => {
      const defaultVal = dom.addExpDefaultReplicates.value;
      document.querySelectorAll('.add-exp-cb:checked').forEach(cb => {
          const tr = cb.closest('tr');
          const numInput = tr.querySelector('.add-exp-count');
          if (numInput) numInput.value = defaultVal;
      });
  });

  async function handleConfirmAddExp() {
      const selected = Array.from(document.querySelectorAll('.add-exp-cb:checked'));
      if (selected.length === 0) return UIUtils.showToast("No source samples selected.", "warning");
      
      dom.btnConfirmAddExp.disabled = true;
      dom.btnConfirmAddExp.textContent = 'Creating...';
      
      const payloads = [];
      selected.forEach(cb => {
          const id = parseInt(cb.value, 10);
          const sample = state.allSamples.find(s => s.id === id);
          if (!sample) return;
          
          const tr = cb.closest('tr');
          const countInput = tr.querySelector('.add-exp-count');
          const count = parseInt(countInput.value, 10) || 1;
          
          for (let i = 0; i < count; i++) {
              payloads.push({
                  sample_type: sample.sample_type,
                  category: "Experimental",
                  parent_sample_id: id,
                  strain_id: sample.strain_id || null
              });
          }
      });
      
      try {
          await ApiUtils.request(`/api/v1/projects/${state.projectId}/samples/bulk`, {
              method: 'POST',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({ samples: payloads })
          });
          UIUtils.showToast(`Successfully created ${payloads.length} experimental samples!`, "success");
          dom.addExpModal.style.display = 'none';
          await fetchTablesData();
          renderTables();
      } catch (e) {
          UIUtils.handleError(e, "Failed to create experimental samples");
      } finally {
          dom.btnConfirmAddExp.disabled = false;
          dom.btnConfirmAddExp.textContent = 'Create Experimental Samples';
      }
  }

  // --- Helpers ---
  function switchTab(tabName) {
    document.querySelectorAll('.tab-content').forEach(el => el.style.display = 'none');
    document.querySelectorAll('.tab-btn').forEach(btn => btn.classList.remove('active'));

    const masterSources = document.getElementById('cb-all-sources');
    const masterExp = document.getElementById('cb-all-experimental');
    if (masterSources) masterSources.checked = false;
    if (masterExp) masterExp.checked = false;

    document.getElementById(`tab-btn-${tabName}`).classList.add('active');
    document.getElementById(`view-${tabName}`).style.display = 'block';

    const tblId = `${tabName}-table`;
    const tbl = document.getElementById(tblId);
    if (tbl && $.fn.DataTable.isDataTable(tbl)) $(tbl).DataTable().columns.adjust().draw();
  }

  const bindSelectAll = (selectAllId, rowClass, tableId) => {
    $(`#${selectAllId}`).on('change', function() {
      const isChecked = this.checked;
      const dt = $(`#${tableId}`).DataTable();
      $('input.' + rowClass, dt.rows({search: 'applied'}).nodes()).prop('checked', isChecked);
    });
  };

  // --- Bootstrap ---
  init();
}
