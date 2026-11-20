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
    sampleMap: {},
    dashboardLoaded: false,
    analysisRunsLoaded: false,
    analysisRuns: [],
    selectedToolName: null,
    selectedRunVersion: null,
    previewArtifact: null, // { key, name, href }
    selectedIds: {
      'sources-table': new Set(),
      'experimental-table': new Set(),
      'plates-table': new Set()
    },
    showArchived: {
      sources: false,
      experimental: false,
      plates: false
    }
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
        <button onclick="UploadService.openWizard('results', '${projectId}')" class="button secondary-button">Bulk Import Data</button>
        <button onclick="UploadService.openWizard('bulk-samples', '${projectId}')" class="button secondary-button">Bulk Import Samples</button>
        <button onclick="UploadService.openWizard('bulk-plates', '${projectId}')" class="button secondary-button">Bulk Plate Actions</button>
        <button onclick="MultiPlatePlanner.openWizard('${projectId}')" class="button" style="background-color: #28a745; border-color: #28a745; white-space: nowrap;">Plan Multiple Plates</button>
      </div>
      
      <div id="project-info-container">Loading details...</div>
  
      <div class="tab-bar">
          <button id="tab-btn-sources" class="tab-btn">Sources</button>
          <button id="tab-btn-experimental" class="tab-btn">Experimental</button>
          <button id="tab-btn-plates" class="tab-btn">Plates</button>
          <button id="tab-btn-dashboard" class="tab-btn">Data Dashboard</button>
          <button id="tab-btn-team" class="tab-btn">Team</button>
          <button id="tab-btn-analysis-runs" class="tab-btn">Analysis Runs</button>
      </div>
      
      
      <div id="view-dashboard" class="tab-content" style="display: none;">
          <div class="table-header">
              <h3>Data Dashboard</h3>
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
             <h3>Source Samples <button type="button" class="workflow-info-btn" data-step="source" onclick="UIUtils.toggleSampleLineageBanner(this, 'source')" title="Sample Lineage Guide">?</button></h3>
             <div class="action-bar">
                <label id="wrap-show-archived-sources" class="show-archived-toggle" style="display:none; margin-right: 10px;">
                    <input type="checkbox" id="cb-show-archived-sources">
                    <span id="lbl-show-archived-sources">Show archived</span>
                </label>
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
              <h3>Experimental Samples <button type="button" class="workflow-info-btn" data-step="experimental" onclick="UIUtils.toggleSampleLineageBanner(this, 'experimental')" title="Sample Lineage Guide">?</button></h3>
              <div class="action-bar">
                  <label id="wrap-show-archived-experimental" class="show-archived-toggle" style="display:none; margin-right: 10px;">
                      <input type="checkbox" id="cb-show-archived-experimental">
                      <span id="lbl-show-archived-experimental">Show archived</span>
                  </label>
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
                <label id="wrap-show-archived-plates" class="show-archived-toggle" style="display:none; margin-right: 10px;">
                    <input type="checkbox" id="cb-show-archived-plates">
                    <span id="lbl-show-archived-plates">Show archived</span>
                </label>
                <div class="bulk-toolbar">
                    <select id="bulk-action-plates">
                        <option value="">Bulk Actions...</option>
                        <option value="print">Print Selected Labels</option>
                        <option value="transfer-map">Generate Transfer Map</option>
                        <option value="status_Active">Restore Selected</option>
                        <option value="archive">Archive Selected</option>
                        <option value="delete-permanent">Delete Selected (Reclaim IDs)</option>
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

      <div id="view-analysis-runs" class="tab-content" style="display: none;">
          <div class="table-header">
              <h3>Analysis Runs</h3>
              <div class="action-bar">
                <button type="button" id="btn-open-register-run" class="button secondary-button artifact-btn-sm">+ Register Run</button>
              </div>
          </div>

          <div id="register-analysis-run-drawer" class="storage-conn-form-panel" style="display: none; margin-top: 0; margin-bottom: 1rem;">
              <p class="text-secondary" style="font-size: 0.88em; margin: 0 0 1rem 0;">
                Link an output folder from your configured object storage (<code>gs://</code> or <code>s3://</code>). ExLab will scan the folder, index deliverables, compute a manifest checksum, and map plate media automatically.
              </p>

              <div class="storage-conn-form-grid">
                <div class="form-group">
                  <label for="register-run-uri-input">Deliverables Folder URI</label>
                  <input type="text" id="register-run-uri-input" placeholder="gs://bucket/path/to/deliverables or s3://bucket/path/to/deliverables" style="font-family: var(--font-mono, monospace); font-size: 0.85rem;">
                </div>

                <div class="form-group">
                  <label for="register-run-tool-input">Analysis Tool <span style="font-weight: normal; font-size: 0.85em;">(optional)</span></label>
                  <input type="text" id="register-run-tool-input" placeholder="Auto-detected (e.g. PhenoTypic)">
                </div>
              </div>

              <div class="form-group" style="margin-bottom: 0;">
                <label for="register-run-notes-input">Notes <span style="font-weight: normal; font-size: 0.85em;">(optional)</span></label>
                <input type="text" id="register-run-notes-input" placeholder="Describe pipeline adjustments or QC pass...">
              </div>

              <div class="storage-conn-form-actions">
                <button type="button" id="btn-cancel-register-run" class="secondary-button artifact-btn-sm">Cancel</button>
                <button type="button" id="btn-confirm-register-run" class="button artifact-btn-sm">Import Run</button>
              </div>
          </div>

          <div id="analysis-runs-container">
              <div class="placeholder">Loading analysis runs...</div>
          </div>
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
    btnAnalysisRuns: document.getElementById('tab-btn-analysis-runs'),
    analysisRunsContainer: document.getElementById('analysis-runs-container'),
    btnOpenRegisterRun: document.getElementById('btn-open-register-run'),
    registerRunDrawer: document.getElementById('register-analysis-run-drawer'),
    registerRunUriInput: document.getElementById('register-run-uri-input'),
    registerRunToolInput: document.getElementById('register-run-tool-input'),
    registerRunNotesInput: document.getElementById('register-run-notes-input'),
    btnCancelRegisterRun: document.getElementById('btn-cancel-register-run'),
    btnConfirmRegisterRun: document.getElementById('btn-confirm-register-run'),
    
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
        dom.exportSamplesBtn.href = `/api/v1/projects/${state.projectId}/samples?format=csv&category=Experimental`;
        dom.exportSourcesBtn.href = `/api/v1/projects/${state.projectId}/samples?format=csv&category=Source`;
        dom.exportPlatesBtn.href = `/api/v1/projects/${state.projectId}/plates?format=csv`;
        dom.exportDashboardBtn.href = `/api/v1/projects/${state.projectId}/results/export`;

        const [projectRes, templateRes] = await Promise.all([
            ApiUtils.request(`/api/v1/projects/${state.projectId}`),
            ApiUtils.request('/api/v1/settings/project_metadata_template').catch(() => []),
            fetchTablesData(),
            fetchAndRenderTeam(),
            fetchAnalysisRuns()
        ]);
        
        state.project = projectRes;
        if (Array.isArray(templateRes)) state.metadataTemplate = templateRes;

        renderInfoView();
        renderTables();
        setupEventListeners();
        switchTab('sources');

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
      
      res.columns.forEach(col => {
          col.defaultContent = "";
          if (col.title === "Well_Pos") col.title = "Well Position";
          if (col.title === "Time_Point") col.title = "Time Point";
      });
      UIUtils.applyNaturalColumnTypes(res.columns);
      
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
                  filename: `project_${state.projectId}_matrix`,
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
      UIUtils.handleError(e, "Failed to load dashboard");
    }
  }

  function updateArchivedToggleUi(key, archivedCount) {
    const wrap = document.getElementById(`wrap-show-archived-${key}`);
    const cb = document.getElementById(`cb-show-archived-${key}`);
    const lbl = document.getElementById(`lbl-show-archived-${key}`);
    if (!wrap || !cb || !lbl) return;
    cb.checked = !!state.showArchived[key];
    lbl.textContent = `Show archived (${archivedCount})`;
    wrap.style.display = (archivedCount > 0 || state.showArchived[key]) ? 'inline-flex' : 'none';
  }

  function renderTables() {
    const allSources = state.allSamples.filter(s => s.category === 'Source');
    const allExperimental = state.allSamples.filter(s => s.category === 'Experimental');
    const archivedSourceCount = allSources.filter(s => s.status === 'Archived').length;
    const archivedExpCount = allExperimental.filter(s => s.status === 'Archived').length;
    const archivedPlatesCount = state.plates.filter(p => p.status === 'Archived').length;

    updateArchivedToggleUi('sources', archivedSourceCount);
    updateArchivedToggleUi('experimental', archivedExpCount);
    updateArchivedToggleUi('plates', archivedPlatesCount);

    const sourceSamples = state.showArchived.sources
      ? allSources
      : allSources.filter(s => s.status !== 'Archived');
    const expSamples = state.showArchived.experimental
      ? allExperimental
      : allExperimental.filter(s => s.status !== 'Archived');
    const activePlates = state.showArchived.plates
      ? state.plates
      : state.plates.filter(p => p.status !== 'Archived');
    const dateFormatter = new Intl.DateTimeFormat();
    const projNameEncoded = encodeURIComponent(state.project?.name || '');

    state.selectedIds['sources-table'].clear();
    state.selectedIds['experimental-table'].clear();
    state.selectedIds['plates-table'].clear();

    const initFastTable = (id, rows, columns, sortCol) => {
      const tbl = document.getElementById(id);
      if ($.fn.DataTable.isDataTable(tbl)) {
        $(tbl).DataTable().destroy();
      }
      tbl.querySelector('tbody').innerHTML = '';

      $(tbl).DataTable({
        data: rows,
        columns: columns,
        deferRender: true,
        pageLength: 10,
        lengthChange: false,
        order: [[sortCol, "desc"]],
        language: {
          emptyTable: "No items found."
        }
      });
    };

    initFastTable('sources-table', sourceSamples, [
      {
        data: null,
        orderable: false,
        className: 'dt-body-center',
        render: (_data, _type, s) =>
          `<input type="checkbox" class="row-cb cb-source-row" value="${s.id}" ${state.selectedIds['sources-table'].has(String(s.id)) ? 'checked' : ''}>`
      },
      {
        data: 'short_id',
        type: 'natural',
        render: (val, type, s) => {
          if (type !== 'display') return val;
          const archivedTag = s.status === 'Archived' ? '<span class="archived-tag">(Archived)</span>' : '';
          return `<a href="/samples/${s.id}?from=project&context_id=${state.projectId}&context_name=${projNameEncoded}" class="id-link">${val}</a>${archivedTag}`;
        }
      },
      { data: 'sample_type' },
      {
        data: null,
        render: (_data, type, s) => {
          if (s.strain_id) {
            const label = state.strainMap[s.strain_id] || `Strain ${s.strain_id}`;
            return type === 'display'
              ? `<a href="/strains/${s.strain_id}?from=project&context_id=${state.projectId}&context_name=${projNameEncoded}" class="badge"><span class="emoji">🧬</span> ${label}</a>`
              : label;
          }
          if (s.community_id) {
            const label = state.communityMap[s.community_id] || `Community ${s.community_id}`;
            return type === 'display'
              ? `<a href="/communities/${s.community_id}?from=project&context_id=${state.projectId}&context_name=${projNameEncoded}" class="badge"><span class="emoji">🦠</span> ${label}</a>`
              : label;
          }
          return '-';
        }
      },
      {
        data: 'created_at',
        render: (val, type) =>
          (type === 'sort' || type === 'type') ? val : dateFormatter.format(new Date(val * 1000))
      }
    ], 4);

    initFastTable('experimental-table', expSamples, [
      {
        data: null,
        orderable: false,
        className: 'dt-body-center',
        render: (_data, _type, s) =>
          `<input type="checkbox" class="row-cb cb-exp-row" value="${s.id}" ${state.selectedIds['experimental-table'].has(String(s.id)) ? 'checked' : ''}>`
      },
      {
        data: 'short_id',
        type: 'natural',
        render: (val, type, s) =>
          type === 'display'
            ? `<a href="/samples/${s.id}?from=project&context_id=${state.projectId}&context_name=${projNameEncoded}" class="id-link">${val}</a>`
            : val
      },
      {
        data: 'parent_sample_id',
        type: 'natural',
        render: (pid, type) => {
          if (!pid) return '-';
          const label = state.sampleMap[pid]?.short_id || `Sample ${pid}`;
          return type === 'display'
            ? `<a href="/samples/${pid}?from=project&context_id=${state.projectId}&context_name=${projNameEncoded}" class="badge"><span class="emoji">🧪</span> ${label}</a>`
            : label;
        }
      },
      {
        data: 'status',
        render: (val, type) =>
          type === 'display'
            ? `<span class="status-pill status-${val.toLowerCase()}">${val}</span>`
            : val
      },
      {
        data: 'created_at',
        render: (val, type) =>
          (type === 'sort' || type === 'type') ? val : dateFormatter.format(new Date(val * 1000))
      }
    ], 4);

    initFastTable('plates-table', activePlates, [
      {
        data: null,
        orderable: false,
        className: 'dt-body-center',
        render: (_data, _type, p) =>
          `<input type="checkbox" class="row-cb cb-plate-row" value="${p.id}" ${state.selectedIds['plates-table'].has(String(p.id)) ? 'checked' : ''}>`
      },
      {
        data: 'short_id',
        type: 'natural',
        render: (val, type, p) => {
          const label = val || p.id;
          if (type !== 'display') return label;
          const archivedTag = p.status === 'Archived' ? '<span class="archived-tag">(Archived)</span>' : '';
          return `<a href="/plates/${p.id}?from=project&context_id=${state.projectId}&context_name=${projNameEncoded}" class="id-link">${label}</a>${archivedTag}`;
        }
      },
      {
        data: 'name',
        type: 'natural',
        render: (val, type, p) =>
          type === 'display'
            ? `<a href="/plates/${p.id}?from=project&context_id=${state.projectId}&context_name=${projNameEncoded}" class="badge"><span class="emoji">🧫</span> ${val}</a>`
            : val
      },
      {
        data: 'plate_format',
        render: (val, type) => type === 'display' ? `<small>${val}</small>` : val
      },
      {
        data: 'created_at',
        render: (val, type) =>
          (type === 'sort' || type === 'type') ? val : dateFormatter.format(new Date(val * 1000))
      }
    ], 4);

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

  // --- Analysis Runs & Artifact File Tree ---

  const ARTIFACT_GLYPHS = {
    folder: `<svg class="artifact-glyph" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M1.75 3.75c0-.55.45-1 1-1h3.3l1.4 1.5h5.8c.55 0 1 .45 1 1v7c0 .55-.45 1-1 1H2.75c-.55 0-1-.45-1-1v-8.5z"/></svg>`,
    parquet: `<svg class="artifact-glyph" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true"><path d="M10.953 20.391c-.95-1.013-.978-1.057-.807-1.248.27-.304 11.811-10.868 11.977-10.964.143-.083 1.558.6 1.867.902.119.115-.8 1.098-5.817 6.218-3.277 3.345-6.021 6.095-6.097 6.112-.076.016-.582-.442-1.123-1.02zM8.333 17.7c-.511-.515-.835-.92-.803-1.003.058-.152 6.693-5.515 6.817-5.51.153.005 1.714 1.191 1.714 1.302 0 .116-6.67 6.073-6.8 6.073-.038 0-.456-.388-.928-.862zM5.91 15.227c-.4-.448-.652-.81-.598-.86.28-.26 3.164-2.26 3.26-2.26.154.002 1.547 1.196 1.547 1.327 0 .161-3.124 2.566-3.334 2.566-.1 0-.494-.348-.875-.773zm-1.988-2.04c-.574-.58-.642-.69-.512-.81.187-.174 9.357-6.094 9.545-6.162.146-.053 1.417.716 1.417.858 0 .102-9.401 6.685-9.63 6.744-.091.023-.435-.241-.82-.63zm6.117-.935c-.379-.314-.687-.626-.684-.692.002-.066.337-.354.743-.64.406-.285 2.227-1.587 4.046-2.894 1.82-1.307 3.383-2.376 3.474-2.376.245 0 1.485.717 1.445.835-.057.173-8.012 6.336-8.179 6.337-.086 0-.467-.256-.845-.57zm-7.963-.923c-.452-.478-.518-.593-.4-.701.103-.093 5.783-3.449 6.47-3.821.114-.062 1.318.807 1.28.922-.05.15-6.456 4.172-6.645 4.172-.09 0-.408-.257-.705-.572Zm13.735.038c-.416-.298-.772-.59-.792-.65-.028-.083 3.74-3.215 4.595-3.818.166-.118.297-.079 1.01.301.45.24.818.485.817.545-.002.152-4.487 4.098-4.7 4.135-.096.016-.515-.214-.93-.513zM.547 9.707 0 9.156l.243-.158c.133-.086 2.841-1.563 6.018-3.282l5.775-3.126.553.27c.303.15.552.322.552.384 0 .084-11.875 7.018-12.02 7.018-.015 0-.274-.249-.574-.553zm8.88-2.79c-.269-.2-.487-.418-.487-.483s1.083-.755 2.406-1.534l2.405-1.417.617.33c.34.18.617.363.617.406 0 .122-4.715 3.07-4.904 3.067-.092-.001-.387-.167-.655-.368Zm4.816-.612c-.325-.2-.589-.418-.588-.484.002-.112 1.64-1.209 1.909-1.279.123-.032 1.226.55 1.315.694.027.044-.031.15-.128.237-.333.296-1.7 1.202-1.81 1.199-.06-.002-.374-.167-.698-.367z"/></svg>`,
    csv: `<svg class="artifact-glyph" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.35" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><rect x="2" y="2.5" width="12" height="11" rx="1.2"/><line x1="2" y1="6.2" x2="14" y2="6.2"/><line x1="2" y1="9.8" x2="14" y2="9.8"/><line x1="6.2" y1="2.5" x2="6.2" y2="13.5"/></svg>`,
    html: `<svg class="artifact-glyph" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><polyline points="5.2 4.8 2 8 5.2 11.2"/><polyline points="10.8 4.8 14 8 10.8 11.2"/><line x1="9.2" y1="3.2" x2="6.8" y2="12.8"/></svg>`,
    pdf: `<svg class="artifact-glyph" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.35" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M3.5 1.75h6l3 3v9.5c0 .41-.34.75-.75.75h-8.25c-.41 0-.75-.34-.75-.75V2.5c0-.41.34-.75.75-.75z"/><polyline points="9.5 1.75 9.5 4.75 12.5 4.75"/><polyline points="5.2 11.2 7.2 8.8 9 10.2 11 7.5"/></svg>`,
    config: `<svg class="artifact-glyph" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.35" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><line x1="2.5" y1="4.5" x2="13.5" y2="4.5"/><circle cx="5.5" cy="4.5" r="1.4" fill="var(--bg-dark-1)"/><line x1="2.5" y1="8" x2="13.5" y2="8"/><circle cx="10.5" cy="8" r="1.4" fill="var(--bg-dark-1)"/><line x1="2.5" y1="11.5" x2="13.5" y2="11.5"/><circle cx="7" cy="11.5" r="1.4" fill="var(--bg-dark-1)"/></svg>`,
    file: `<svg class="artifact-glyph" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.35" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M3.5 1.75h6l3 3v9.5c0 .41-.34.75-.75.75h-8.25c-.41 0-.75-.34-.75-.75V2.5c0-.41.34-.75.75-.75z"/><polyline points="9.5 1.75 9.5 4.75 12.5 4.75"/></svg>`
  };

  function glyphForFilename(filename) {
    const lower = filename.toLowerCase();
    if (lower.endsWith('.parquet')) return ARTIFACT_GLYPHS.parquet;
    if (lower.endsWith('.csv') || lower.endsWith('.tsv')) return ARTIFACT_GLYPHS.csv;
    if (lower.endsWith('.html') || lower.endsWith('.htm')) return ARTIFACT_GLYPHS.html;
    if (lower.endsWith('.pdf') || lower.endsWith('.png') || lower.endsWith('.svg')) return ARTIFACT_GLYPHS.pdf;
    if (lower.endsWith('.yaml') || lower.endsWith('.yml') || lower.endsWith('.json') || lower.endsWith('.pht-pipe')) return ARTIFACT_GLYPHS.config;
    return ARTIFACT_GLYPHS.file;
  }

  async function fetchAnalysisRuns() {
    try {
      const res = await ApiUtils.request(`/api/v1/projects/${state.projectId}/analysis-runs`);
      state.analysisRuns = res.data || [];
      state.analysisRunsLoaded = true;

      if (dom.btnAnalysisRuns) {
        dom.btnAnalysisRuns.textContent = state.analysisRuns.length > 0
          ? `Analysis Runs (${state.analysisRuns.length})`
          : 'Analysis Runs';
      }

      const canonicalRun = state.analysisRuns.find(r => r.is_canonical) || state.analysisRuns[0];
      if (canonicalRun) {
        if (!state.selectedToolName || !state.analysisRuns.some(r => r.tool_name === state.selectedToolName)) {
          state.selectedToolName = canonicalRun.tool_name;
        }
        const toolRuns = state.analysisRuns.filter(r => r.tool_name === state.selectedToolName);
        const toolCanonical = toolRuns.find(r => r.is_canonical) || toolRuns[0];
        if (!state.selectedRunVersion || !toolRuns.some(r => r.version_number === state.selectedRunVersion)) {
          state.selectedRunVersion = toolCanonical ? toolCanonical.version_number : null;
        }
      } else {
        state.selectedToolName = null;
        state.selectedRunVersion = null;
      }
      renderAnalysisRunsView();
    } catch (error) {
      console.error("Failed to load analysis runs", error);
      if (dom.analysisRunsContainer) {
        dom.analysisRunsContainer.innerHTML = `<div class="placeholder">Failed to load analysis runs.</div>`;
      }
    }
  }

  function isPreviewableFile(filename) {
    const lower = filename.toLowerCase();
    return lower.endsWith('.html') || lower.endsWith('.htm') || lower.endsWith('.pdf');
  }

  function fileSortPriority(filename) {
    const lower = filename.toLowerCase();
    if (lower === 'measurements.parquet') return 0;
    if (lower === 'measurements.csv') return 1;
    if (lower === 'processing_report.html') return 2;
    if (lower === 'master_measurements.parquet') return 3;
    if (lower === 'master_measurements.csv') return 4;
    return 10;
  }

  function buildArtifactTreeModel(run) {
    const root = { folders: {}, files: [] };
    const artifacts = run.artifacts || {};
    const baseUri = (run.storage_base_uri || '').replace(/\/+$/, '');

    Object.entries(artifacts).forEach(([artifactKey, uri]) => {
      if (typeof uri !== 'string' || !uri) return;
      let relPath = uri;
      if (baseUri && uri.startsWith(baseUri + '/')) {
        relPath = uri.slice(baseUri.length + 1);
      } else {
        const lastSlash = uri.lastIndexOf('/');
        relPath = lastSlash >= 0 ? uri.slice(lastSlash + 1) : artifactKey;
      }

      const parts = relPath.split('/').filter(Boolean);
      let current = root;
      for (let i = 0; i < parts.length - 1; i++) {
        const folderName = parts[i];
        if (!current.folders[folderName]) {
          current.folders[folderName] = { folders: {}, files: [] };
        }
        current = current.folders[folderName];
      }
      const fileName = parts[parts.length - 1] || artifactKey;
      current.files.push({ key: artifactKey, name: fileName, uri });
    });

    const sortNode = (node) => {
      node.files.sort((a, b) => {
        const pa = fileSortPriority(a.name);
        const pb = fileSortPriority(b.name);
        if (pa !== pb) return pa - pb;
        return a.name.localeCompare(b.name);
      });
      Object.values(node.folders).forEach(sortNode);
    };
    sortNode(root);
    return root;
  }

  function countFilesInNode(node) {
    let total = node.files.length;
    Object.values(node.folders).forEach(child => {
      total += countFilesInNode(child);
    });
    return total;
  }

  function renderTreeNodeHtml(node, toolName, versionNumber) {
    const folderNames = Object.keys(node.folders).sort((a, b) => a.localeCompare(b));
    const filesHtml = node.files.map(file => {
      const downloadHref = `/api/v1/projects/${encodeURIComponent(state.projectId)}/analysis-runs/${versionNumber}/signed-url?artifact=${encodeURIComponent(file.key)}&tool=${encodeURIComponent(toolName)}&redirect=true`;
      const previewable = isPreviewableFile(file.name);
      const isPreviewing = state.previewArtifact && state.previewArtifact.key === file.key;
      const previewBtnHtml = previewable
        ? `<button type="button" class="secondary-button artifact-btn-sm btn-preview-artifact" data-key="${UIUtils.escapeHTML(file.key)}" data-name="${UIUtils.escapeHTML(file.name)}" data-href="${downloadHref}">${isPreviewing ? 'Hide' : 'View'}</button>`
        : '';
      const primaryActionLabel = previewable ? 'Open ↗' : 'Download';
      return `
        <div class="artifact-file-row ${isPreviewing ? 'is-previewing' : ''}">
          <div class="artifact-file-name" title="${UIUtils.escapeHTML(file.uri)}">
            ${glyphForFilename(file.name)}
            <span>${UIUtils.escapeHTML(file.name)}</span>
          </div>
          <div class="artifact-file-actions">
            <button type="button" class="artifact-ghost-btn btn-copy-uri" data-uri="${UIUtils.escapeHTML(file.uri)}">Copy URI</button>
            ${previewBtnHtml}
            <a class="button secondary-button artifact-btn-sm" href="${downloadHref}" target="_blank" rel="noopener">${primaryActionLabel}</a>
          </div>
        </div>
      `;
    }).join('');

    const foldersHtml = folderNames.map(folderName => {
      const childNode = node.folders[folderName];
      const count = countFilesInNode(childNode);
      return `
        <details>
          <summary>
            <span class="artifact-folder-label">
              <span class="artifact-folder-caret">▸</span>
              ${ARTIFACT_GLYPHS.folder}
              <span>${UIUtils.escapeHTML(folderName)}/</span>
            </span>
            <span style="font-size: 0.8rem; font-weight: normal; color: var(--text-secondary);">${count} file${count === 1 ? '' : 's'}</span>
          </summary>
          <div class="artifact-tree-children">
            ${renderTreeNodeHtml(childNode, toolName, versionNumber)}
          </div>
        </details>
      `;
    }).join('');

    return foldersHtml + filesHtml;
  }

  function toggleRegisterRunDrawer(forceOpen) {
    if (!dom.registerRunDrawer) return;
    const shouldOpen = typeof forceOpen === 'boolean'
      ? forceOpen
      : dom.registerRunDrawer.style.display === 'none';
    dom.registerRunDrawer.style.display = shouldOpen ? 'block' : 'none';
    if (shouldOpen && dom.registerRunUriInput) {
      dom.registerRunUriInput.focus();
    }
  }

  async function handleConfirmRegisterRun() {
    const uri = dom.registerRunUriInput.value.trim();
    if (!uri) {
      return UIUtils.showToast('Please enter a deliverables folder URI.', 'warning');
    }
    const toolName = dom.registerRunToolInput.value.trim();
    const notes = dom.registerRunNotesInput.value.trim();
    const payload = { storage_base_uri: uri };
    if (toolName) payload.tool_name = toolName;
    if (notes) payload.notes = notes;

    try {
      dom.btnConfirmRegisterRun.disabled = true;
      dom.btnConfirmRegisterRun.textContent = 'Scanning folder...';
      const created = await ApiUtils.request(`/api/v1/projects/${state.projectId}/analysis-runs/sync`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });
      toggleRegisterRunDrawer(false);
      dom.registerRunUriInput.value = '';
      dom.registerRunToolInput.value = '';
      dom.registerRunNotesInput.value = '';
      UIUtils.showToast(`Registered ${created.tool_name} Run v${created.version_number}.`, 'success');
      state.selectedToolName = created.tool_name;
      state.selectedRunVersion = created.version_number;
      await fetchAnalysisRuns();
      state.selectedToolName = created.tool_name;
      state.selectedRunVersion = created.version_number;
      renderAnalysisRunsView();
    } catch (err) {
      UIUtils.handleError(err, 'Failed to import analysis run from bucket');
    } finally {
      dom.btnConfirmRegisterRun.disabled = false;
      dom.btnConfirmRegisterRun.textContent = 'Import Run';
    }
  }

  function renderAnalysisRunsView() {
    if (!dom.analysisRunsContainer) return;

    if (!state.analysisRuns || state.analysisRuns.length === 0) {
      dom.analysisRunsContainer.innerHTML = `
        <div class="placeholder">No analysis runs have been registered for this project yet.</div>
      `;
      return;
    }

    const distinctTools = Array.from(new Set(state.analysisRuns.map(r => r.tool_name || 'Custom')));
    if (!state.selectedToolName || !distinctTools.includes(state.selectedToolName)) {
      state.selectedToolName = distinctTools[0];
    }

    const toolRuns = state.analysisRuns.filter(r => (r.tool_name || 'Custom') === state.selectedToolName);
    const selectedRun =
      toolRuns.find(r => r.version_number === state.selectedRunVersion) ||
      toolRuns.find(r => r.is_canonical) ||
      toolRuns[0];
    state.selectedRunVersion = selectedRun.version_number;

    const toolSelectorHtml = distinctTools.length > 1
      ? `
        <label for="select-analysis-tool" style="font-weight: 600;">Tool:</label>
        <select id="select-analysis-tool" style="min-width: 150px;">
          ${distinctTools.map(t => `<option value="${UIUtils.escapeHTML(t)}" ${t === state.selectedToolName ? 'selected' : ''}>${UIUtils.escapeHTML(t)}</option>`).join('')}
        </select>
      `
      : '';

    const optionsHtml = toolRuns.map(r => {
      const d = new Date(r.created_at * 1000);
      const isoDate = d.toISOString().slice(0, 10);
      const suffix = r.is_canonical ? ' *' : '';
      return `<option value="${r.version_number}" ${r.version_number === selectedRun.version_number ? 'selected' : ''}>v${r.version_number} — ${isoDate}${suffix}</option>`;
    }).join('');

    const setCanonicalBtnHtml = !selectedRun.is_canonical
      ? `<button type="button" id="btn-promote-canonical-run" class="button secondary-button artifact-btn-sm">Set v${selectedRun.version_number} as Canonical</button>`
      : '';

    const pipeConfig = selectedRun.pipeline_config || {};
    const gridDimStr = (pipeConfig.nrows && pipeConfig.ncols)
      ? `${pipeConfig.nrows} × ${pipeConfig.ncols}`
      : null;
    const pipeStageNames = pipeConfig.pipe_cfgs ? Object.keys(pipeConfig.pipe_cfgs) : (Array.isArray(pipeConfig.stages) ? pipeConfig.stages : []);
    const measNames = pipeConfig.meas ? Object.keys(pipeConfig.meas) : (Array.isArray(pipeConfig.modules) ? pipeConfig.modules : []);
    const pipeDesc = pipeConfig.desc || pipeConfig.description || null;

    const summaryMetrics = selectedRun.summary_metrics || {};
    const scopeParts = Object.entries(summaryMetrics)
      .filter(([_k, v]) => typeof v === 'number' || typeof v === 'string')
      .map(([k, v]) => {
        const cleanKey = k.replace(/_count$/, '').replace(/_/g, ' ');
        const formattedVal = typeof v === 'number' ? v.toLocaleString() : String(v);
        return `${formattedVal} ${cleanKey}`;
      });

    const toolDisplay = [
      selectedRun.tool_name || 'Custom',
      selectedRun.tool_version ? `v${selectedRun.tool_version}` : null
    ].filter(Boolean).join(' ');

    const breakdownItems = [
      { label: 'Status', html: selectedRun.is_canonical ? 'Canonical' : `Historical (v${selectedRun.version_number})` },
      { label: 'Analysis Tool', html: `<code>${UIUtils.escapeHTML(toolDisplay)}</code>` },
      selectedRun.pipeline_name ? { label: 'Pipeline', html: `<code>${UIUtils.escapeHTML(selectedRun.pipeline_name)}</code>` } : null,
      gridDimStr ? { label: 'Grid Dimensions', html: `<code>${UIUtils.escapeHTML(gridDimStr)}</code>` } : null,
      scopeParts.length > 0 ? { label: 'Dataset Scope', html: UIUtils.escapeHTML(scopeParts.join(' · ')) } : null,
      { label: 'Checksum', html: `<code>${UIUtils.escapeHTML(selectedRun.dataset_checksum)}</code>` },
      { label: 'Registered', html: UIUtils.escapeHTML(new Date(selectedRun.created_at * 1000).toLocaleString()) }
    ].filter(Boolean);

    const pipelineHeading = `${selectedRun.tool_name || 'Analysis'} Pipeline`;
    const stagesFlowHtml = pipeStageNames.length > 0
      ? `
        <div class="run-breakdown-item">
          <span class="run-breakdown-label">${UIUtils.escapeHTML(pipelineHeading)}</span>
          <ol class="run-breakdown-list run-breakdown-ordered">
            ${pipeStageNames.map(s => `<li>${UIUtils.escapeHTML(String(s))}</li>`).join('')}
          </ol>
        </div>
      `
      : '';

    const measFlowHtml = measNames.length > 0
      ? `
        <div class="run-breakdown-item">
          <span class="run-breakdown-label">Extracted Feature Modules</span>
          <ul class="run-breakdown-list">
            ${measNames.map(m => `<li>${UIUtils.escapeHTML(String(m))}</li>`).join('')}
          </ul>
        </div>
      `
      : '';

    const rootFolderName = (() => {
      const base = (selectedRun.storage_base_uri || '').replace(/\/+$/, '');
      const idx = base.lastIndexOf('/');
      return idx >= 0 ? base.slice(idx + 1) : 'deliverables';
    })();

    const treeModel = buildArtifactTreeModel(selectedRun);
    const totalArtifacts = countFilesInNode(treeModel);
    const treeInnerHtml = totalArtifacts > 0
      ? `
        <details open>
          <summary>
            <span class="artifact-folder-label">
              <span class="artifact-folder-caret">▸</span>
              ${ARTIFACT_GLYPHS.folder}
              <span>${UIUtils.escapeHTML(rootFolderName)}/</span>
            </span>
            <button type="button" class="artifact-ghost-btn btn-copy-uri" data-uri="${UIUtils.escapeHTML(selectedRun.storage_base_uri)}" onclick="event.stopPropagation();">Copy base URI</button>
          </summary>
          <div class="artifact-tree-children">
            ${renderTreeNodeHtml(treeModel, selectedRun.tool_name, selectedRun.version_number)}
          </div>
        </details>
      `
      : `<div class="text-secondary" style="padding: 0.75rem;">No downloadable artifacts listed for this run.</div>`;

    const previewPaneHtml = state.previewArtifact
      ? `
        <div class="artifact-preview-pane">
          <div class="artifact-preview-header">
            <span class="artifact-preview-title">${UIUtils.escapeHTML(state.previewArtifact.name)}</span>
            <div style="display: flex; align-items: center; gap: 0.5rem;">
              <a class="button secondary-button artifact-btn-sm" href="${state.previewArtifact.href}" target="_blank" rel="noopener">Open in New Tab ↗</a>
              <button type="button" id="btn-close-artifact-preview" class="artifact-ghost-btn">Close ✕</button>
            </div>
          </div>
          <iframe class="artifact-preview-frame" src="${state.previewArtifact.href}" title="${UIUtils.escapeHTML(state.previewArtifact.name)}"></iframe>
        </div>
      `
      : '';

    dom.analysisRunsContainer.innerHTML = `
      <div class="analysis-runs-card">
        <div class="analysis-run-toolbar">
          <div class="analysis-run-selector-group">
            ${toolSelectorHtml}
            <label for="select-analysis-run-version" style="font-weight: 600;">Run Version:</label>
            <select id="select-analysis-run-version">${optionsHtml}</select>
          </div>
          <div>${setCanonicalBtnHtml}</div>
        </div>

        <details class="analysis-run-details">
          <summary>
            <span class="artifact-folder-caret">▸</span>
            <span>Run breakdown</span>
          </summary>
          <div class="analysis-run-details-body">
            <div class="run-breakdown-grid">
              ${breakdownItems.map(item => `
                <div class="run-breakdown-item">
                  <span class="run-breakdown-label">${item.label}</span>
                  <span class="run-breakdown-val">${item.html}</span>
                </div>
              `).join('')}
            </div>
            ${(stagesFlowHtml || measFlowHtml) ? `<div class="run-breakdown-columns">${stagesFlowHtml}${measFlowHtml}</div>` : ''}
            ${pipeDesc ? `<div class="run-breakdown-item"><span class="run-breakdown-label">Pipeline Description</span><span class="run-breakdown-val">${UIUtils.escapeHTML(pipeDesc)}</span></div>` : ''}
            ${selectedRun.notes ? `<div class="run-breakdown-item"><span class="run-breakdown-label">Notes</span><span class="run-breakdown-val">${UIUtils.escapeHTML(selectedRun.notes)}</span></div>` : ''}
            <div style="display: flex; justify-content: flex-end; padding-top: 0.45rem; border-top: 1px solid var(--border-color);">
              <button type="button" id="btn-delete-analysis-run" class="artifact-ghost-btn artifact-ghost-btn-danger">Delete run</button>
            </div>
          </div>
        </details>

        ${previewPaneHtml}

        <div class="artifact-tree">
          ${treeInnerHtml}
        </div>
      </div>
    `;

    const toolSelectEl = document.getElementById('select-analysis-tool');
    if (toolSelectEl) {
      toolSelectEl.onchange = (e) => {
        state.selectedToolName = e.target.value;
        const nextToolRuns = state.analysisRuns.filter(r => (r.tool_name || 'Custom') === state.selectedToolName);
        const nextCan = nextToolRuns.find(r => r.is_canonical) || nextToolRuns[0];
        state.selectedRunVersion = nextCan ? nextCan.version_number : null;
        state.previewArtifact = null;
        renderAnalysisRunsView();
      };
    }

    const selectEl = document.getElementById('select-analysis-run-version');
    if (selectEl) {
      selectEl.onchange = (e) => {
        state.selectedRunVersion = parseInt(e.target.value, 10);
        state.previewArtifact = null;
        renderAnalysisRunsView();
      };
    }

    const closePreviewBtn = document.getElementById('btn-close-artifact-preview');
    if (closePreviewBtn) {
      closePreviewBtn.onclick = () => {
        state.previewArtifact = null;
        renderAnalysisRunsView();
      };
    }

    dom.analysisRunsContainer.querySelectorAll('.btn-preview-artifact').forEach(btn => {
      btn.onclick = () => {
        const key = btn.getAttribute('data-key');
        const name = btn.getAttribute('data-name');
        const href = btn.getAttribute('data-href');
        if (state.previewArtifact && state.previewArtifact.key === key) {
          state.previewArtifact = null;
        } else {
          state.previewArtifact = { key, name, href };
        }
        renderAnalysisRunsView();
      };
    });

    const promoteBtn = document.getElementById('btn-promote-canonical-run');
    if (promoteBtn) {
      promoteBtn.onclick = async () => {
        if (!confirm(`Set ${selectedRun.tool_name} v${selectedRun.version_number} as canonical for this project?`)) return;
        try {
          promoteBtn.disabled = true;
          promoteBtn.textContent = 'Promoting...';
          await ApiUtils.request(`/api/v1/projects/${state.projectId}/analysis-runs/${selectedRun.version_number}/canonical?tool=${encodeURIComponent(selectedRun.tool_name)}`, {
            method: 'PUT',
            headers: { 'Content-Type': 'application/json' },
            body: '{}'
          });
          UIUtils.showToast(`${selectedRun.tool_name} v${selectedRun.version_number} is now canonical.`, 'success');
          await fetchAnalysisRuns();
          state.selectedToolName = selectedRun.tool_name;
          state.selectedRunVersion = selectedRun.version_number;
          renderAnalysisRunsView();
        } catch (err) {
          UIUtils.handleError(err, 'Failed to update canonical run');
        }
      };
    }

    const deleteRunBtn = document.getElementById('btn-delete-analysis-run');
    if (deleteRunBtn) {
      deleteRunBtn.onclick = async () => {
        if (!confirm(`Delete ${selectedRun.tool_name} v${selectedRun.version_number} from ExLab? (Files in object storage are never deleted.)`)) return;
        try {
          deleteRunBtn.disabled = true;
          deleteRunBtn.textContent = 'Deleting...';
          await ApiUtils.request(`/api/v1/projects/${state.projectId}/analysis-runs/${selectedRun.version_number}?tool=${encodeURIComponent(selectedRun.tool_name)}`, {
            method: 'DELETE'
          });
          UIUtils.showToast(`Deleted ${selectedRun.tool_name} v${selectedRun.version_number}.`, 'success');
          state.previewArtifact = null;
          state.selectedRunVersion = null;
          await fetchAnalysisRuns();
        } catch (err) {
          deleteRunBtn.disabled = false;
          deleteRunBtn.textContent = 'Delete run';
          UIUtils.handleError(err, 'Failed to delete analysis run');
        }
      };
    }

    dom.analysisRunsContainer.querySelectorAll('.btn-copy-uri').forEach(btn => {
      btn.onclick = async (e) => {
        e.stopPropagation();
        const uri = btn.getAttribute('data-uri');
        const originalText = btn.textContent;
        try {
          await navigator.clipboard.writeText(uri);
          btn.textContent = 'Copied';
          setTimeout(() => { btn.textContent = originalText; }, 1400);
        } catch (_err) {
          UIUtils.showToast(uri, 'info');
        }
      };
    });
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
    dom.btnAnalysisRuns.onclick = () => switchTab('analysis-runs');
    if (dom.btnOpenRegisterRun) dom.btnOpenRegisterRun.onclick = () => toggleRegisterRunDrawer();
    if (dom.btnCancelRegisterRun) dom.btnCancelRegisterRun.onclick = () => toggleRegisterRunDrawer(false);
    if (dom.btnConfirmRegisterRun) dom.btnConfirmRegisterRun.onclick = handleConfirmRegisterRun;

    dom.applySourcesBtn.onclick = () => handleBulkAction('sources-table', 'bulk-action-sources');
    dom.applyExpBtn.onclick = () => handleBulkAction('experimental-table', 'bulk-action-experimental');
    dom.applyPlatesBtn.onclick = () => handleBulkAction('plates-table', 'bulk-action-plates');

    ['sources', 'experimental', 'plates'].forEach(key => {
      const cb = document.getElementById(`cb-show-archived-${key}`);
      if (cb) {
        cb.onchange = (e) => {
          state.showArchived[key] = e.target.checked;
          renderTables();
        };
      }
    });

    dom.addUserForm.onsubmit = handleAddUser;
    
    dom.btnOpenAddStrains.onclick = openAddStrainsModal;
    dom.btnConfirmAddStrains.onclick = handleConfirmAddStrains;
    
    if (dom.btnOpenAddExpFromSources) dom.btnOpenAddExpFromSources.onclick = openAddExpFromSourcesModal;
    if (dom.btnConfirmAddExp) dom.btnConfirmAddExp.onclick = handleConfirmAddExp;
  }

  async function handleSaveProjectInfo() {
    const metadata = {};
    const metaContainer = document.getElementById('edit-metadata-container');
    const currentMeta = (state.project && state.project.metadata && typeof state.project.metadata === 'object')
      ? state.project.metadata
      : {};
    if (metaContainer) {
        metaContainer.querySelectorAll('div').forEach(row => {
            const keyInput = row.querySelector('.meta-key');
            const valInput = row.querySelector('.meta-value');
            if (keyInput && valInput) {
                const key = keyInput.value.trim();
                const val = valInput.value.trim();
                if (key && (val !== '' || Object.prototype.hasOwnProperty.call(currentMeta, key))) {
                    metadata[key] = val;
                }
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
    
    const selectedSet = state.selectedIds[tableId];
    const ids = selectedSet && selectedSet.size > 0
      ? Array.from(selectedSet)
      : Array.from($(`#${tableId}`).DataTable().$('.row-cb:checked')).map(cb => cb.value);
    if (ids.length === 0) return UIUtils.showToast("No items selected.", "warning");
    
    if (action.startsWith('status_')) {
      const newStatus = action.split('_')[1];
      const isPlates = tableId === 'plates-table';
      const noun = isPlates ? 'plate(s)' : 'item(s)';
      if (!confirm(`Change status to ${newStatus} for ${ids.length} ${noun}?`)) return;
      try {
        if (isPlates) {
          await Promise.all(ids.map(id => {
            const plate = state.plates.find(p => p.id === parseInt(id, 10));
            return ApiUtils.request(`/api/v1/plates/${id}`, {
              method: 'PUT',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({
                name: plate.name,
                status: newStatus
              })
            });
          }));
        } else {
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
                community_id: sample.community_id || null,
                status: newStatus
              })
            });
          }));
        }
        UIUtils.showToast(`Updated ${ids.length} ${noun}`, "success");
        state.dashboardLoaded = false;
        await fetchTablesData();
        renderTables();
      } catch (error) {
        UIUtils.handleError(error, "Bulk status update failed.");
      }
    } else if (action === 'archive') {
      const isPlates = tableId === 'plates-table';
      const noun = isPlates ? 'plate(s)' : 'item(s)';
      if (!confirm(`Archive ${ids.length} ${noun}?`)) return;
      try {
        const endpointPrefix = isPlates ? '/api/v1/plates' : '/api/v1/samples';
        await Promise.all(ids.map(id => ApiUtils.request(`${endpointPrefix}/${id}`, { method: 'DELETE' })));
        UIUtils.showToast(`Archived ${ids.length} ${noun}`, "success");
        state.dashboardLoaded = false;
        await fetchTablesData();
        renderTables();
      } catch (error) {
        UIUtils.handleError(error, "Bulk archive failed.");
      }
    } else if (action === 'delete-permanent' && tableId === 'plates-table') {
      try {
        const inspections = await Promise.all(ids.map(async id => {
          const [detailRes, resultsRes] = await Promise.all([
            ApiUtils.request(`/api/v1/plates/${id}`),
            ApiUtils.request(`/api/v1/plates/${id}/results`)
          ]);
          const assignedWells = (detailRes.wells || []).filter(w => w.well && w.well.sample_id != null).length;
          const plateDataCount = (resultsRes.data || []).length;
          return { assignedWells, plateDataCount };
        }));

        const platesWithData = inspections.filter(x => x.assignedWells > 0 || x.plateDataCount > 0).length;
        const totalAssignedWells = inspections.reduce((acc, x) => acc + x.assignedWells, 0);
        const totalPlateData = inspections.reduce((acc, x) => acc + x.plateDataCount, 0);

        const promptMsg = platesWithData > 0
          ? `WARNING: ${platesWithData} of the ${ids.length} selected plate(s) currently have assigned data (${totalAssignedWells} assigned well(s), ${totalPlateData} plate data value(s)).\n\nPermanently delete ${ids.length} plate(s), their wells, and their assigned data values, and reclaim trailing plate IDs?`
          : `Permanently delete ${ids.length} plate(s) and reclaim trailing plate IDs? This cannot be undone.`;

        if (!confirm(promptMsg)) return;

        for (const id of ids) {
          await ApiUtils.request(`/api/v1/plates/${id}?permanent=true`, { method: 'DELETE' });
        }
        UIUtils.showToast(`Deleted ${ids.length} plate(s)`, "success");
        state.dashboardLoaded = false;
        await fetchTablesData();
        renderTables();
      } catch (error) {
        UIUtils.handleError(error, "Bulk plate delete failed.");
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
// Prompt for the source plate when more than one is selected
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
      const sourceSamples = state.allSamples
          .filter(s => s.category === 'Source' && s.status !== 'Archived')
          .sort((a, b) => UIUtils.naturalCompare(a.short_id || a.id, b.short_id || b.id));
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
    state.selectedIds['sources-table'].clear();
    state.selectedIds['experimental-table'].clear();
    $('#sources-table tbody input.cb-source-row, #experimental-table tbody input.cb-exp-row').prop('checked', false);

    document.getElementById(`tab-btn-${tabName}`).classList.add('active');
    document.getElementById(`view-${tabName}`).style.display = 'block';

    if (tabName === 'dashboard' && !state.dashboardLoaded) {
      state.dashboardLoaded = true;
      renderDashboard();
      return;
    }

    const tblId = `${tabName}-table`;
    const tbl = document.getElementById(tblId);
    if (tbl && $.fn.DataTable.isDataTable(tbl)) $(tbl).DataTable().columns.adjust().draw();
  }

  const bindSelectAll = (selectAllId, rowClass, tableId) => {
    const selectedSet = state.selectedIds[tableId];
    $(`#${selectAllId}`).off('change').on('change', function() {
      const isChecked = this.checked;
      const dt = $(`#${tableId}`).DataTable();
      const filteredData = dt.rows({ search: 'applied' }).data().toArray();
      filteredData.forEach(item => {
        if (isChecked) selectedSet.add(String(item.id));
        else selectedSet.delete(String(item.id));
      });
      $(`#${tableId} tbody input.${rowClass}`).prop('checked', isChecked);
    });

    $(`#${tableId} tbody`).off('change', `input.${rowClass}`).on('change', `input.${rowClass}`, function() {
      if (this.checked) selectedSet.add(String(this.value));
      else {
        selectedSet.delete(String(this.value));
        const masterCb = document.getElementById(selectAllId);
        if (masterCb) masterCb.checked = false;
      }
    });
  };

  // --- Bootstrap ---
  init();
}
