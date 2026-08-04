/* src/server/static/js/upload-service.js */

/**
 * UPLOAD_CONFIG defines the rules, UI requirements, and endpoints for all bulk CSV uploads.
 * 
 * To add a new upload wizard type, simply add a new key to this dictionary.
 * If 'tabGroup' is provided, the renderer will automatically group multiple 
 * configurations into a single tabbed modal (e.g., 'bulk-plates').
 */
const UPLOAD_CONFIG = {
  'strains': {
    title: 'Bulk Upload Strains',
    tabGroup: null,
    tabLabel: null,
    requiresProject: false,
    requiresPlateFormat: false,
    requiresPlateCategory: false,
    getEndpoint: (targetId, selectedProjectId) => '/api/v1/strains/bulk-csv',
    headerHints: `
        <div style="margin-bottom: 5px;"><strong>Required Columns:</strong> <code>genus</code>, <code>species</code>, <code>strain_name</code></div>
        <div><strong>Optional Columns:</strong> <code>genotype</code>, <code>notes</code>, <code>parent_strain_id</code></div>
    `,
    templateCsv: "genus,species,strain_name,genotype,notes,parent_strain_id\nEscherichia,coli,MG1655,F-,, \nSaccharomyces,cerevisiae,BY4741,MATa his3Δ1,,",
    httpMethod: 'POST'
  },
  'results': {
    title: 'Bulk Upload Results',
    tabGroup: null,
    tabLabel: null,
    requiresProject: false,
    requiresPlateFormat: false,
    requiresPlateCategory: false,
    getEndpoint: (targetId, selectedProjectId) => '/api/v1/results/bulk-csv',
    headerHints: `
        <div style="margin-bottom: 5px;"><strong>Required Column 1:</strong> Must be either <code>sample_short_id</code> OR <code>plate_short_id</code></div>
        <div><strong>Subsequent Columns:</strong> Use the exact Short IDs of existing Result Definitions (e.g. <code>OD600</code>, <code>Yield</code>). Leave cells blank to skip.</div>
    `,
    templateCsv: "sample_short_id,OD600,Yield\nS-001,1.5,42.5\nS-002,1.2,38.1",
    httpMethod: 'POST'
  },
  'bulk-samples-source': {
    title: 'Bulk Upload Samples',
    tabGroup: 'bulk-samples',
    tabLabel: 'Source Samples',
    requiresProject: true,
    requiresPlateFormat: false,
    requiresPlateCategory: false,
    getEndpoint: (targetId, selectedProjectId) => `/api/v1/projects/${targetId || selectedProjectId}/samples/bulk-csv?default_category=Source`,
    headerHints: `
        <div style="margin-bottom: 5px;"><strong>Required Columns:</strong> <code>sample_type</code></div>
        <div style="margin-bottom: 10px;"><strong>Optional Columns:</strong> <code>strain_id</code>, <code>result_definition_ids</code>, <code>genus</code>, <code>species</code>, <code>strain_name</code>, <code>genotype</code></div>
        <hr style="border: none; border-top: 1px solid var(--border-color); margin: 8px 0;" />
        <ul style="margin: 0; padding-left: 20px; font-size: 0.9em;">
            <li>Creates <strong>Source</strong> samples. The <code>category</code> column is automatically inferred as Source.</li>
            <li>Source samples MUST have a <code>strain_id</code> (or provide <code>genus</code>, <code>species</code>, and <code>strain_name</code> to link/create a strain dynamically) and CANNOT have a <code>parent_sample_id</code>.</li>
        </ul>
    `,
    templateCsv: "sample_type,strain_id,genus,species,strain_name,genotype\nLiquid Cell Culture,,,Escherichia,coli,K-12,WT\nLiquid Cell Culture,1,,,,",
    httpMethod: 'POST'
  },
  'bulk-samples-experimental': {
    title: 'Bulk Upload Samples',
    tabGroup: 'bulk-samples',
    tabLabel: 'Experimental Samples',
    requiresProject: true,
    requiresPlateFormat: false,
    requiresPlateCategory: false,
    getEndpoint: (targetId, selectedProjectId) => `/api/v1/projects/${targetId || selectedProjectId}/samples/bulk-csv?default_category=Experimental`,
    headerHints: `
        <div style="margin-bottom: 5px;"><strong>Required Columns:</strong> <code>sample_type</code>, <code>parent_sample_short_id</code></div>
        <div style="margin-bottom: 10px;"><strong>Optional Columns:</strong> <code>strain_id</code>, <code>result_definition_ids</code></div>
        <hr style="border: none; border-top: 1px solid var(--border-color); margin: 8px 0;" />
        <ul style="margin: 0; padding-left: 20px; font-size: 0.9em;">
            <li>Creates <strong>Experimental</strong> samples. The <code>category</code> column is automatically inferred as Experimental.</li>
            <li>Experimental samples MUST have a <code>parent_sample_short_id</code>.</li>
        </ul>
    `,
    templateCsv: "sample_type,parent_sample_short_id,strain_id,result_definition_ids\nLiquid Cell Culture,S-001,1,\nLiquid Cell Culture,S-002,1,1",
    httpMethod: 'POST'
  },
  'bulk-plates-with-layouts': {
    title: 'Bulk Upload Plates',
    tabGroup: 'bulk-plates',
    tabLabel: 'Create & Assign Layouts',
    requiresProject: true,
    requiresPlateFormat: true,
    requiresPlateCategory: true,
    getEndpoint: (targetId, selectedProjectId) => `/api/v1/projects/${targetId || selectedProjectId}/plates/bulk-csv`,
    headerHints: `
        <div style="margin-bottom: 5px;"><strong>Required Columns:</strong> <code>plate_name</code>, <code>well</code>, <code>sample_short_id</code></div>
        <hr style="border: none; border-top: 1px solid var(--border-color); margin: 8px 0;" />
        <ul style="margin: 0; padding-left: 20px; font-size: 0.9em;">
            <li>Rows with the identical <code>plate_name</code> are grouped together to generate a single plate.</li>
            <li>You can instantly generate dozens of unique plates in one upload by varying the <code>plate_name</code>.</li>
        </ul>
    `,
    templateCsv: "plate_name,well,sample_short_id\nPlate 1,A1,S-001\nPlate 1,A2,S-002\nPlate 2,A1,S-003",
    httpMethod: 'POST'
  },
  'bulk-plates-blank': {
    title: 'Bulk Upload Plates',
    tabGroup: 'bulk-plates',
    tabLabel: 'Create Blank Plates',
    requiresProject: true,
    requiresPlateFormat: true,
    requiresPlateCategory: true,
    getEndpoint: (targetId, selectedProjectId) => `/api/v1/projects/${targetId || selectedProjectId}/plates/bulk-csv`,
    headerHints: `
        <div style="margin-bottom: 5px;"><strong>Required Columns:</strong> <code>plate_name</code></div>
        <hr style="border: none; border-top: 1px solid var(--border-color); margin: 8px 0;" />
        <ul style="margin: 0; padding-left: 20px; font-size: 0.9em;">
            <li>Creates blank plates within the project.</li>
        </ul>
    `,
    templateCsv: "plate_name\nPlate 1\nPlate 2",
    httpMethod: 'POST'
  },
  'bulk-plates-update': {
    title: 'Bulk Upload Plates',
    tabGroup: 'bulk-plates',
    tabLabel: 'Update Existing Layouts',
    requiresProject: true,
    requiresPlateFormat: true,
    requiresPlateCategory: true,
    getEndpoint: (targetId, selectedProjectId) => `/api/v1/projects/${targetId || selectedProjectId}/plates/bulk-csv`,
    headerHints: `
        <div style="margin-bottom: 5px;"><strong>Required Columns:</strong> <code>plate_name</code>, <code>well</code>, <code>sample_short_id</code></div>
        <hr style="border: none; border-top: 1px solid var(--border-color); margin: 8px 0;" />
        <ul style="margin: 0; padding-left: 20px; font-size: 0.9em;">
            <li>Assigns samples to <strong>existing</strong> plates in this project.</li>
            <li>Rows with identical <code>plate_name</code> are grouped and applied to that specific plate.</li>
        </ul>
    `,
    templateCsv: "plate_name,well,sample_short_id\nPlate 1,A1,S-001\nPlate 1,A2,S-002\nPlate 2,A1,S-003",
    httpMethod: 'PATCH'
  },
  'plate-layout': {
    title: 'Bulk Upload Plate Layout',
    tabGroup: null,
    tabLabel: null,
    requiresProject: false,
    requiresPlateFormat: false,
    requiresPlateCategory: false,
    getEndpoint: (targetId, selectedProjectId) => `/api/v1/plates/${targetId}/layout-csv`,
    headerHints: `
        <div style="margin-bottom: 5px;"><strong>Required Columns:</strong> <code>well</code>, <code>sample_short_id</code></div>
        <hr style="border: none; border-top: 1px solid var(--border-color); margin: 8px 0;" />
        <div style="font-size: 0.9em;"><strong>Alternative:</strong> You can upload a Matrix-style CSV grid (like an Excel plate layout) where the first row contains column numbers and the first column contains row letters.</div>
    `,
    templateCsv: "well,sample_short_id\nA1,S-001\nA2,S-002\nB1,S-003",
    httpMethod: 'POST'
  }
};

window.UploadService = {
  openWizard: async function (type, targetId = null) {
    let activeType = type;
    if (type === 'bulk-plates') {
      activeType = 'bulk-plates-with-layouts'; // default tab
    } else if (type === 'bulk-samples') {
      activeType = 'bulk-samples-source'; // default tab
    }
    
    // If the base type is opened globally (no targetId) but it's part of a tab group,
    // we should render using the activeType directly if it has config.
    const config = UPLOAD_CONFIG[activeType];
    if (!config) {
        console.error("Unknown upload wizard type: ", activeType);
        return;
    }

    this.renderWizard(activeType, targetId);
  },

  renderWizard: function(activeType, targetId) {
    // Remove existing if any
    const existing = document.getElementById('uploadWizard');
    if (existing) existing.remove();

    const config = UPLOAD_CONFIG[activeType];
    
    let projectSelectorHtml = '';
    if (config.requiresProject && !targetId) {
      projectSelectorHtml = `
        <div style="margin-bottom: 15px;">
          <label for="wizard-project-select" style="display:block; font-weight:bold; margin-bottom:5px;">Select Destination Project:</label>
          <div class="select-wrapper">
             <select id="wizard-project-select" style="width:100%;">
                <option value="">Loading projects...</option>
             </select>
          </div>
        </div>
      `;
    }

    let formatSelectorHtml = '';
    if (config.requiresPlateFormat) {
        formatSelectorHtml = `
        <div style="margin-bottom: 15px;">
          <label for="wizard-format-select" style="display:block; font-weight:bold; margin-bottom:5px;">Plate Format:</label>
          <div class="select-wrapper">
             <select id="wizard-format-select" style="width:100%;">
                <option value="24-well">24-well</option>
                <option value="48-well">48-well</option>
                <option value="96-well" selected>96-well</option>
                <option value="384-well">384-well</option>
             </select>
          </div>
        </div>
        `;
    }

    let typeSelectorHtml = '';
    if (config.requiresPlateCategory) {
        typeSelectorHtml = `
        <div style="margin-bottom: 15px;">
          <label for="wizard-type-select" style="display:block; font-weight:bold; margin-bottom:5px;">Plate Category:</label>
          <div class="select-wrapper">
             <select id="wizard-type-select" style="width:100%;">
                <option value="Experimental" selected>Experimental</option>
                <option value="Source">Source</option>
             </select>
          </div>
        </div>
        `;
    }

    let tabsHtml = '';
    if (config.tabGroup) {
        tabsHtml = '<div style="display:flex; border-bottom: 2px solid var(--border-color); margin-bottom: 20px;">';
        for (const [key, tabConfig] of Object.entries(UPLOAD_CONFIG)) {
            if (tabConfig.tabGroup === config.tabGroup) {
                const isActive = (key === activeType);
                tabsHtml += `
                    <div class="wizard-tab ${isActive ? 'active-tab' : ''}" 
                         onclick="UploadService.renderWizard('${key}', '${targetId || ''}')"
                         style="padding: 10px 20px; cursor: pointer; border-bottom: ${isActive ? '3px solid var(--color-accent)' : 'none'}; color: ${isActive ? 'var(--color-accent)' : 'var(--text-color)'}; font-weight: bold;">
                        ${tabConfig.tabLabel}
                    </div>
                `;
            }
        }
        tabsHtml += '</div>';
    }

    const modalHtml = `
        <div id="uploadWizard" class="modal">
            <div class="modal-content" style="max-width: 900px;">
                <span class="close-button" onclick="document.getElementById('uploadWizard').remove()">&times;</span>
                <h3>${config.title}</h3>
                
                ${tabsHtml}

                <div id="wizard-step-1">
                    ${projectSelectorHtml}
                    ${formatSelectorHtml}
                    ${typeSelectorHtml}
                    <div style="background: var(--bg-dark-2); padding: 10px; border-radius: 4px; margin-bottom: 15px; border: 1px solid var(--border-color);">
                        <p class="text-secondary" style="margin: 0; font-size: 14px;">${config.headerHints}</p>
                    </div>
                    
                    <div style="text-align: right; margin-bottom: 15px;">
                        <a href="#" id="wizard-download-template" style="color: var(--color-accent); text-decoration: none; font-size: 14px; font-weight: bold;">
                            ⬇️ Download CSV Template
                        </a>
                    </div>

                    <div style="margin-top: 20px; text-align: center; padding: 20px; background: var(--bg-dark); border: 2px dashed var(--border-color); border-radius: 8px;">
                        <input type="file" id="csv-file-input" accept=".csv" style="display:none">
                        <button id="wizard-browse-btn" class="button secondary-button" style="display: inline-flex; align-items: center; justify-content: center; gap: 8px; font-size: 16px; padding: 10px 20px;">
                            <span style="font-size: 1.2em;">📄</span> Browse files
                        </button>
                        <div id="file-input-helper" class="text-secondary" style="margin-top: 10px; font-size: 0.9em;">
                            Accepted format: .csv
                        </div>
                    </div>
                </div>

                <div id="wizard-step-2" style="display:none;">
                    <div style="display:flex; justify-content:space-between; align-items:center; margin-bottom:1rem;">
                        <h4>Review Data (<span id="preview-count">0</span> rows)</h4>
                        <div>
                            <button id="btn-dry-run" class="button secondary-button" style="margin-right: 10px; display: none;">Simulate Upload</button>
                            <button id="btn-commit" class="button">Confirm & Upload</button>
                        </div>
                    </div>
                    <div id="dry-run-results" style="display: none; background: var(--bg-dark-2); padding: 15px; margin-bottom: 15px; border-radius: 4px; border: 1px solid var(--color-accent);">
                    </div>
                    <div class="preview-container" style="max-height: 400px; overflow-y: auto; border: 1px solid var(--border-color);">
                        <table id="wizard-preview-table" class="display" style="width:100%">
                            <thead><tr id="preview-head"></tr></thead>
                            <tbody id="preview-body"></tbody>
                        </table>
                    </div>
                    <div style="margin-top:1rem;">
                        <button onclick="location.reload()" class="secondary-button">Cancel</button>
                    </div>
                </div>

                <div id="upload-status" style="display:none; margin-top:20px; text-align:center;">
                    <div class="spinner"></div> <p>Processing on Server...</p>
                </div>

                <div id="upload-error" class="error" style="display:none; margin-top:20px;"></div>
            </div>
        </div>`;

    document.body.insertAdjacentHTML('beforeend', modalHtml);
    document.getElementById('uploadWizard').style.display = 'block';
    
    if (config.requiresProject && !targetId) {
      this.populateProjectSelector();
    }
    
    const templateLink = document.getElementById('wizard-download-template');
    if (templateLink && config.templateCsv) {
        templateLink.onclick = (e) => {
            e.preventDefault();
            const encodedUri = encodeURI("data:text/csv;charset=utf-8," + config.templateCsv);
            const link = document.createElement("a");
            link.setAttribute("href", encodedUri);
            link.setAttribute("download", `template_${activeType}.csv`);
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
        };
    }

    this.setupHandlers(activeType, targetId, config);
  },

  populateProjectSelector: async function() {
    const select = document.getElementById('wizard-project-select');
    if (!select) return;

    try {
      const res = await fetch('/api/v1/projects');
      if (res.ok) {
        const json = await res.json();
        const projects = json.data || [];
        if (projects.length === 0) {
           select.innerHTML = '<option value="">No projects available</option>';
           return;
        }
        
        select.innerHTML = '<option value="">-- Select a Project --</option>' + 
          projects.map(p => `<option value="${p.id}">${p.short_id || p.id} - ${p.name}</option>`).join('');
      } else {
        select.innerHTML = '<option value="">Failed to load projects</option>';
      }
    } catch(e) {
      select.innerHTML = '<option value="">Network error loading projects</option>';
    }
  },

  setupHandlers: function (activeType, targetId, config) {
    const fileInput = document.getElementById('csv-file-input');
    const browseBtn = document.getElementById('wizard-browse-btn');
    const helperText = document.getElementById('file-input-helper');

    if (browseBtn) {
      browseBtn.onclick = () => fileInput.click();
    }
    if (fileInput) {
      fileInput.onchange = (e) => {
        if (e.target.files.length > 0) {
            helperText.innerHTML = `Selected: <strong>${e.target.files[0].name}</strong>`;
            helperText.style.color = "var(--color-accent)";
        }
        
        const select = document.getElementById('wizard-project-select');
        let selectedProjectId = select ? select.value : null;

        if (config.requiresProject && !targetId && !selectedProjectId) {
            UIUtils.showToast('Please select a Destination Project before uploading a CSV.', "error");
            fileInput.value = '';
            return;
        }

        let finalEndpoint = config.getEndpoint(targetId, selectedProjectId);

        const formatSelect = document.getElementById('wizard-format-select');
        if (formatSelect && finalEndpoint) {
            const separator = finalEndpoint.includes('?') ? '&' : '?';
            finalEndpoint += `${separator}plate_format=${formatSelect.value}`;
        }

        const typeSelect = document.getElementById('wizard-type-select');
        if (typeSelect && finalEndpoint) {
            const separator = finalEndpoint.includes('?') ? '&' : '?';
            finalEndpoint += `${separator}plate_type=${typeSelect.value}`;
        }

        const file = e.target.files[0];
        
        // Matrix layouts are notoriously tricky to parse correctly with PapaParse into a neat preview 
        // because the first column doesn't have a header name. We'll send it straight to execution 
        // or just let PapaParse show a raw grid. PapaParse is generally smart enough.
        this.processPreview(file, finalEndpoint, config);
      };
    }
  },

  processPreview: function (file, endpoint, config) {
    if (!file) return;

    Papa.parse(file, {
      header: true,
      skipEmptyLines: true,
      complete: (results) => {
        const data = results.data;
        const headers = results.meta.fields;

        document.getElementById('wizard-step-1').style.display = 'none';
        document.getElementById('wizard-step-2').style.display = 'block';
        document.getElementById('preview-count').textContent = data.length;

        const head = document.getElementById('preview-head');
        head.innerHTML = headers.map(h => `<th>${h || 'Row'}</th>`).join('');

        const body = document.getElementById('preview-body');
        body.innerHTML = data.slice(0, 20).map(row => {
          return `<tr>${headers.map(h => {
            const cellValue = row[h] !== undefined ? row[h] : "";
            return `<td>${cellValue}</td>`;
          }).join('')}</tr>`;
        }).join('') + (data.length > 20 ? `<tr><td colspan="${headers.length}" style="text-align:center; padding: 20px; color:var(--text-secondary); background: var(--bg-dark-2);">+ ${data.length - 20} more rows hidden in preview</td></tr>` : '');
        
        const btnDryRun = document.getElementById('btn-dry-run');
        if (endpoint.includes('/samples/bulk-csv')) {
            btnDryRun.style.display = 'inline-block';
            btnDryRun.onclick = () => this.executeUpload(file, endpoint, config, true);
        } else {
            btnDryRun.style.display = 'none';
        }
        
        document.getElementById('btn-commit').onclick = () => this.executeUpload(file, endpoint, config, false);
      }
    });
  },

  executeUpload: async function (file, endpoint, config, isDryRun = false) {
    const status = document.getElementById('upload-status');
    const wizardStep = document.getElementById('wizard-step-2');
    const errorDiv = document.getElementById('upload-error');
    const dryRunResults = document.getElementById('dry-run-results');

    wizardStep.style.display = 'none';
    status.style.display = 'block';
    errorDiv.style.display = 'none';

    const rawCsv = await file.text();
    
    // Matrix routing check: if uploading a plate layout and PapaParse saw blank headers,
    // the backend needs to know it's a matrix file to decode it properly.
    // If headers exist, the user might still be trying to upload a standard 2 column list.
    // The backend `update_plate_layout_matrix_csv_handler` natively handles the switch.
    let finalEndpoint = endpoint;
    if (endpoint && endpoint.includes('/layout-csv') && !rawCsv.startsWith("well,sample")) {
        // If it doesn't strictly start with well,sample..., try throwing it at the matrix route just in case
        finalEndpoint = endpoint.replace('/layout-csv', '/layout-matrix-csv');
    }

    if (isDryRun) {
        const separator = finalEndpoint.includes('?') ? '&' : '?';
        finalEndpoint += `${separator}dry_run=true`;
    }

    const httpMethod = config.httpMethod || 'POST';

    try {
      const response = await fetch(finalEndpoint, {
        method: httpMethod,
        headers: { 'Content-Type': 'text/csv' },
        body: rawCsv
      });

      const result = await response.json();
      if (response.ok) {
        if (isDryRun) {
            status.style.display = 'none';
            wizardStep.style.display = 'block';
            let msg = `<strong>Dry Run Results:</strong><br>This upload will create ${result.summary.created_samples} samples.`;
            if (result.summary.linked_strains > 0 || result.summary.created_strains > 0) {
                msg += `<br>It will link to ${result.summary.linked_strains} existing strains and dynamically create ${result.summary.created_strains} new strains.`;
            }
            dryRunResults.innerHTML = msg;
            dryRunResults.style.display = 'block';
        } else {
            let msg = "Upload Successful!";
            if (result.summary) {
                msg = `Upload Successful! Created ${result.summary.created_samples} samples.`;
                if (result.summary.linked_strains > 0 || result.summary.created_strains > 0) {
                    msg += ` Linked to ${result.summary.linked_strains} existing strains and dynamically created ${result.summary.created_strains} new strains.`;
                }
            }
            UIUtils.showToast(msg, "success");
            setTimeout(() => window.location.reload(), 2000);
        }
      } else {
        throw new Error(result.error || "Server validation failed");
      }
    } catch (err) {
      status.style.display = 'none';
      wizardStep.style.display = 'block';
      errorDiv.innerHTML = `❌ ${err.message.replace(/\n/g, '<br>')}`;
      errorDiv.style.display = 'block';
    }
  }
};
