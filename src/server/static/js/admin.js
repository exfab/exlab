/* src/server/static/js/admin.js */

async function loadAdminPage(mainElement) {
  mainElement.innerHTML = `
    <div class="container">
      <h1>Admin Panel</h1>
      <p>Welcome to the admin panel. Here you can manage users and other system settings.</p>
      
      <div class="admin-sections">
        <div id="admin-content" class="admin-section">
          <h2>Create User</h2>
          <form id="create-user-form">
            <div class="form-group">
              <label for="username">Username</label>
              <input type="text" id="username" name="username" required>
            </div>
            <div class="form-group">
              <label for="password">Password</label>
              <input type="password" id="password" name="password" required>
            </div>
            <div class="form-group">
              <label for="role">Role</label>
              <select id="role" name="role">
                <option value="admin">Admin</option>
                <option value="lab_manager">Lab Manager</option>
                <option value="project_manager">Project Manager</option>
                <option value="project_user">Project User</option>
              </select>
            </div>
            <button type="submit" class="button">Create User</button>
          </form>
        </div>

        <div id="admin-settings" class="admin-section" style="margin-top: 2rem; padding-top: 2rem; border-top: 1px solid var(--border-color);">
          <h2>System Settings</h2>
          
          <div class="setting-block">
            <h3>Project Metadata Template</h3>
            <p style="font-size: 0.9em; color: var(--text-secondary); margin-bottom: 1rem;">
              Define the standard metadata keys that should be requested when users create a new project. 
              These act as default fields in the project creation form.
            </p>
            
            <div id="metadata-template-list" style="margin-bottom: 1rem;">
              <!-- Template items will be injected here -->
            </div>
            
            <div style="background: var(--bg-dark-1); border: 1px solid var(--border-color); border-radius: 4px; padding: 15px; max-width: 400px; margin-bottom: 1rem;">
              <h4 style="margin-top: 0; margin-bottom: 15px; color: var(--text-secondary);">Add New Field</h4>
              
              <div class="form-group" style="margin-bottom: 10px;">
                <label for="new-template-key" style="font-size: 0.85em; color: var(--text-secondary); margin-bottom: 4px; display: block;">Field Name</label>
                <input type="text" id="new-template-key" placeholder="e.g., Client Name" style="width: 100%;">
              </div>

              <div class="form-group" style="margin-bottom: 10px;">
                <label for="new-template-type" style="font-size: 0.85em; color: var(--text-secondary); margin-bottom: 4px; display: block;">Input Type</label>
                <select id="new-template-type" style="width: 100%;">
                  <option value="String">Text Input</option>
                  <option value="Enum">Dropdown Menu</option>
                </select>
              </div>

              <div class="form-group" id="options-container" style="display: none; margin-bottom: 10px;">
                <label for="new-template-options" style="font-size: 0.85em; color: var(--text-secondary); margin-bottom: 4px; display: block;">Dropdown Options (Comma Separated)</label>
                <input type="text" id="new-template-options" placeholder="e.g. Option A, Option B" style="width: 100%;">
              </div>

              <button id="add-template-key-btn" class="button secondary-button" style="width: 100%; margin-top: 10px;">+ Add to Template</button>
            </div>
            
            <button id="save-template-btn" class="button">Save Template</button>
          </div>

          <div class="setting-block" style="margin-top: 2rem;">
            <h3>Community Metadata Template</h3>
            <p style="font-size: 0.9em; color: var(--text-secondary); margin-bottom: 1rem;">
              Define the standard metadata keys that should be requested when users create a new community.
              These act as default fields in the community creation form.
            </p>

            <div id="community-metadata-template-list" style="margin-bottom: 1rem;">
            </div>

            <div style="background: var(--bg-dark-1); border: 1px solid var(--border-color); border-radius: 4px; padding: 15px; max-width: 400px; margin-bottom: 1rem;">
              <h4 style="margin-top: 0; margin-bottom: 15px; color: var(--text-secondary);">Add New Field</h4>

              <div class="form-group" style="margin-bottom: 10px;">
                <label for="community-new-template-key" style="font-size: 0.85em; color: var(--text-secondary); margin-bottom: 4px; display: block;">Field Name</label>
                <input type="text" id="community-new-template-key" placeholder="e.g., Environment" style="width: 100%;">
              </div>

              <div class="form-group" style="margin-bottom: 10px;">
                <label for="community-new-template-type" style="font-size: 0.85em; color: var(--text-secondary); margin-bottom: 4px; display: block;">Input Type</label>
                <select id="community-new-template-type" style="width: 100%;">
                  <option value="String">Text Input</option>
                  <option value="Enum">Dropdown Menu</option>
                </select>
              </div>

              <div class="form-group" id="community-options-container" style="display: none; margin-bottom: 10px;">
                <label for="community-new-template-options" style="font-size: 0.85em; color: var(--text-secondary); margin-bottom: 4px; display: block;">Dropdown Options (Comma Separated)</label>
                <input type="text" id="community-new-template-options" placeholder="e.g. Soil, Water, Gut" style="width: 100%;">
              </div>

              <button id="community-add-template-key-btn" class="button secondary-button" style="width: 100%; margin-top: 10px;">+ Add to Template</button>
            </div>

            <button id="community-save-template-btn" class="button">Save Template</button>
          </div>

          <div class="setting-block storage-conn-section" style="margin-top: 2rem; padding-top: 2rem; border-top: 1px solid var(--border-color);">
            <div class="storage-conn-header">
              <h3>External Object Storage Connections</h3>
              <button type="button" id="btn-toggle-storage-form" class="button secondary-button artifact-btn-sm">+ Add Connection</button>
            </div>
            <p style="font-size: 0.9em; color: var(--text-secondary); margin: 0 0 1rem 0;">
              Read-only signing keys used to generate download URLs for Analysis Run artifacts and Plate Media. Public <code>https://</code> links work automatically without a connection.
            </p>

            <div id="storage-connections-list" class="storage-conn-list">
              <div class="storage-conn-empty">Loading connections...</div>
            </div>

            <div id="storage-conn-form-panel" class="storage-conn-form-panel" style="display: none;">
              <div class="form-group">
                <label for="storage-provider-select">Storage Provider</label>
                <select id="storage-provider-select">
                  <option value="gcs">Google Cloud Storage (gs://)</option>
                  <option value="s3">S3 / MinIO Compatible (s3://)</option>
                </select>
              </div>

              <div id="storage-form-gcs">
                <div class="form-group" style="margin-bottom: 0;">
                  <div style="display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 0.5rem;">
                    <label for="gcs-sa-json-input" style="margin-bottom: 0;">Service Account JSON Key (Read-Only)</label>
                    <label style="font-size: 0.82rem; color: var(--color-accent); cursor: pointer; margin-bottom: 0;">
                      Load from .json file
                      <input type="file" id="gcs-sa-file-input" accept=".json,application/json" style="display: none;">
                    </label>
                  </div>
                  <textarea id="gcs-sa-json-input" rows="5" placeholder='Paste service account JSON key or click "Load from .json file"...'></textarea>
                </div>
              </div>

              <div id="storage-form-s3" style="display: none;">
                <div class="storage-conn-form-grid">
                  <div class="form-group">
                    <label for="s3-endpoint-input">Endpoint URL</label>
                    <input type="text" id="s3-endpoint-input" placeholder="https://s3.amazonaws.com or https://minio.university.edu">
                  </div>
                  <div class="form-group">
                    <label for="s3-region-input">Region</label>
                    <input type="text" id="s3-region-input" placeholder="us-east-1">
                  </div>
                </div>
                <div class="storage-conn-form-grid">
                  <div class="form-group" style="margin-bottom: 0;">
                    <label for="s3-access-key-input">Access Key ID</label>
                    <input type="text" id="s3-access-key-input" placeholder="AKIA...">
                  </div>
                  <div class="form-group" style="margin-bottom: 0;">
                    <label for="s3-secret-key-input">Secret Access Key</label>
                    <input type="password" id="s3-secret-key-input" placeholder="Write-only secret key">
                  </div>
                </div>
              </div>

              <div class="storage-conn-form-actions">
                <button type="button" id="cancel-storage-conn-btn" class="secondary-button artifact-btn-sm">Cancel</button>
                <button type="button" id="save-storage-conn-btn" class="button artifact-btn-sm">Save Connection</button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  `;

  // --- Create User Logic ---
  const createUserForm = document.getElementById('create-user-form');
  if (createUserForm) {
    createUserForm.addEventListener('submit', async (event) => {
      event.preventDefault();

      const username = document.getElementById('username').value;
      const password = document.getElementById('password').value;
      const role = document.getElementById('role').value;

      try {
        const response = await fetch('/api/v1/users', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({ email: username, password: password, role }),
        });

        if (response.ok) {
          UIUtils.showToast('User created successfully!', "success");
          document.getElementById('username').value = '';
          document.getElementById('password').value = '';
        } else {
          const error = await response.json();
          UIUtils.showToast(`Error creating user: ${error.error || 'Unknown error'}`, "error");
        }
      } catch (error) {
        console.error('Error creating user:', error);
        UIUtils.showToast('An error occurred while creating the user.', "error");
      }
    });
  }

  // --- Settings Logic ---
  let currentTemplateKeys = [];
  const templateListEl = document.getElementById('metadata-template-list');
  const newKeyInput = document.getElementById('new-template-key');
  const addKeyBtn = document.getElementById('add-template-key-btn');
  const typeSelect = document.getElementById('new-template-type');
  const optionsInput = document.getElementById('new-template-options');
  const saveTemplateBtn = document.getElementById('save-template-btn');

  // Toggle options input based on type selection
  typeSelect.addEventListener('change', (e) => {
    const optionsContainer = document.getElementById('options-container');
    if (e.target.value === 'Enum') {
      optionsContainer.style.display = 'block';
    } else {
      optionsContainer.style.display = 'none';
      optionsInput.value = '';
    }
  });

  // Load existing template
  try {
    const res = await ApiUtils.request('/api/v1/settings/project_metadata_template');
    if (Array.isArray(res)) {
      currentTemplateKeys = res;
    }
  } catch (e) {
    console.warn("Failed to load metadata template, starting fresh.", e);
  }

  function renderTemplateList() {
    if (currentTemplateKeys.length === 0) {
      templateListEl.innerHTML = `<span style="color: var(--text-secondary); font-style: italic;">No template keys defined.</span>`;
      return;
    }

    templateListEl.innerHTML = currentTemplateKeys.map((def, index) => {
      let typeLabel = def.field_type;
      if (typeof def.field_type === 'object' && def.field_type[0] === 'Enum') {
          typeLabel = `Dropdown (${def.field_type[1].length} options)`;
      } else if (def.field_type === 'String' || (typeof def.field_type === 'object' && def.field_type[0] === 'String')) {
          typeLabel = 'Text';
      }

      return `
      <div style="display: flex; align-items: center; justify-content: space-between; background: var(--bg-dark-2); padding: 8px 12px; margin-bottom: 5px; border-radius: 4px; border: 1px solid var(--border-color); max-width: 400px;">
        <div style="display: flex; flex-direction: column;">
            <span style="font-weight: bold;">${def.key}</span>
            <span style="font-size: 0.8em; color: var(--text-secondary);">${typeLabel}</span>
        </div>
        <button onclick="removeTemplateKey(${index})" style="background: none; border: none; color: #ff4444; cursor: pointer; font-weight: bold;">✕</button>
      </div>
    `}).join('');
  }

  window.removeTemplateKey = function(index) {
    currentTemplateKeys.splice(index, 1);
    renderTemplateList();
  };

  addKeyBtn.addEventListener('click', () => {
    const key = newKeyInput.value.trim();
    if (!key) return;
    
    if (currentTemplateKeys.some(def => def.key === key)) {
      UIUtils.showToast("Key already exists in template.", "warning");
      return;
    }

    const type = typeSelect.value;
    let fieldDef = { key: key };

    if (type === 'Enum') {
        const rawOptions = optionsInput.value.split(',').map(s => s.trim()).filter(s => s.length > 0);
        if (rawOptions.length === 0) {
            UIUtils.showToast("Dropdowns require at least one option.", "warning");
            return;
        }
        fieldDef.field_type = ["Enum", rawOptions];
    } else {
        fieldDef.field_type = ["String"];
    }

    currentTemplateKeys.push(fieldDef);
    newKeyInput.value = '';
    optionsInput.value = '';
    renderTemplateList();
  });

  saveTemplateBtn.addEventListener('click', async () => {
    try {
      saveTemplateBtn.disabled = true;
      saveTemplateBtn.textContent = 'Saving...';
      
      await ApiUtils.request('/api/v1/settings/project_metadata_template', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ value: currentTemplateKeys })
      });
      
      UIUtils.showToast('Metadata template saved successfully.', "success");
    } catch (e) {
      UIUtils.handleError(e, "Failed to save template.");
    } finally {
      saveTemplateBtn.disabled = false;
      saveTemplateBtn.textContent = 'Save Template';
    }
  });

  renderTemplateList();

  // --- Community Metadata Template ---
  let communityTemplateKeys = [];
  const communityTemplateListEl = document.getElementById('community-metadata-template-list');
  const communityNewKeyInput = document.getElementById('community-new-template-key');
  const communityAddKeyBtn = document.getElementById('community-add-template-key-btn');
  const communityTypeSelect = document.getElementById('community-new-template-type');
  const communityOptionsInput = document.getElementById('community-new-template-options');
  const communitySaveTemplateBtn = document.getElementById('community-save-template-btn');

  communityTypeSelect.addEventListener('change', (e) => {
    const optionsContainer = document.getElementById('community-options-container');
    if (e.target.value === 'Enum') {
      optionsContainer.style.display = 'block';
    } else {
      optionsContainer.style.display = 'none';
      communityOptionsInput.value = '';
    }
  });

  try {
    const res = await ApiUtils.request('/api/v1/settings/community_metadata_template');
    if (Array.isArray(res)) {
      communityTemplateKeys = res;
    }
  } catch (e) {
    console.warn("Failed to load community metadata template, starting fresh.", e);
  }

  function renderCommunityTemplateList() {
    if (communityTemplateKeys.length === 0) {
      communityTemplateListEl.innerHTML = `<span style="color: var(--text-secondary); font-style: italic;">No template keys defined.</span>`;
      return;
    }

    communityTemplateListEl.innerHTML = communityTemplateKeys.map((def, index) => {
      let typeLabel = def.field_type;
      if (typeof def.field_type === 'object' && def.field_type[0] === 'Enum') {
          typeLabel = `Dropdown (${def.field_type[1].length} options)`;
      } else if (def.field_type === 'String' || (typeof def.field_type === 'object' && def.field_type[0] === 'String')) {
          typeLabel = 'Text';
      }

      return `
      <div style="display: flex; align-items: center; justify-content: space-between; background: var(--bg-dark-2); padding: 8px 12px; margin-bottom: 5px; border-radius: 4px; border: 1px solid var(--border-color); max-width: 400px;">
        <div style="display: flex; flex-direction: column;">
            <span style="font-weight: bold;">${def.key}</span>
            <span style="font-size: 0.8em; color: var(--text-secondary);">${typeLabel}</span>
        </div>
        <button onclick="removeCommunityTemplateKey(${index})" style="background: none; border: none; color: #ff4444; cursor: pointer; font-weight: bold;">✕</button>
      </div>
    `}).join('');
  }

  window.removeCommunityTemplateKey = function(index) {
    communityTemplateKeys.splice(index, 1);
    renderCommunityTemplateList();
  };

  communityAddKeyBtn.addEventListener('click', () => {
    const key = communityNewKeyInput.value.trim();
    if (!key) return;

    if (communityTemplateKeys.some(def => def.key === key)) {
      UIUtils.showToast("Key already exists in template.", "warning");
      return;
    }

    const type = communityTypeSelect.value;
    let fieldDef = { key: key };

    if (type === 'Enum') {
        const rawOptions = communityOptionsInput.value.split(',').map(s => s.trim()).filter(s => s.length > 0);
        if (rawOptions.length === 0) {
            UIUtils.showToast("Dropdowns require at least one option.", "warning");
            return;
        }
        fieldDef.field_type = ["Enum", rawOptions];
    } else {
        fieldDef.field_type = ["String"];
    }

    communityTemplateKeys.push(fieldDef);
    communityNewKeyInput.value = '';
    communityOptionsInput.value = '';
    renderCommunityTemplateList();
  });

  communitySaveTemplateBtn.addEventListener('click', async () => {
    try {
      communitySaveTemplateBtn.disabled = true;
      communitySaveTemplateBtn.textContent = 'Saving...';

      await ApiUtils.request('/api/v1/settings/community_metadata_template', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ value: communityTemplateKeys })
      });

      UIUtils.showToast('Community metadata template saved successfully.', "success");
    } catch (e) {
      UIUtils.handleError(e, "Failed to save template.");
    } finally {
      communitySaveTemplateBtn.disabled = false;
      communitySaveTemplateBtn.textContent = 'Save Template';
    }
  });

  renderCommunityTemplateList();

  // --- External Object Storage Connections ---
  const storageListEl = document.getElementById('storage-connections-list');
  const toggleStorageFormBtn = document.getElementById('btn-toggle-storage-form');
  const storageFormPanel = document.getElementById('storage-conn-form-panel');
  const cancelStorageBtn = document.getElementById('cancel-storage-conn-btn');
  const providerSelect = document.getElementById('storage-provider-select');
  const gcsFormEl = document.getElementById('storage-form-gcs');
  const s3FormEl = document.getElementById('storage-form-s3');

  const gcsInput = document.getElementById('gcs-sa-json-input');
  const gcsFileInput = document.getElementById('gcs-sa-file-input');
  const s3EndpointInput = document.getElementById('s3-endpoint-input');
  const s3RegionInput = document.getElementById('s3-region-input');
  const s3AccessKeyInput = document.getElementById('s3-access-key-input');
  const s3SecretKeyInput = document.getElementById('s3-secret-key-input');
  const saveStorageBtn = document.getElementById('save-storage-conn-btn');

  if (toggleStorageFormBtn && storageFormPanel) {
    toggleStorageFormBtn.onclick = () => {
      const isHidden = storageFormPanel.style.display === 'none';
      storageFormPanel.style.display = isHidden ? 'block' : 'none';
    };
  }

  if (cancelStorageBtn && storageFormPanel) {
    cancelStorageBtn.onclick = () => {
      storageFormPanel.style.display = 'none';
    };
  }

  if (gcsFileInput && gcsInput) {
    gcsFileInput.onchange = async (e) => {
      const file = e.target.files && e.target.files[0];
      if (!file) return;
      const text = await file.text();
      gcsInput.value = text;
      gcsFileInput.value = '';
    };
  }

  if (providerSelect) {
    providerSelect.addEventListener('change', (e) => {
      const isS3 = e.target.value === 's3';
      gcsFormEl.style.display = isS3 ? 'none' : 'block';
      s3FormEl.style.display = isS3 ? 'block' : 'none';
    });
  }

  function renderStorageConnectionsList(summary) {
    if (!storageListEl || !summary) return;
    const gcs = summary.gcs || {};
    const s3 = summary.s3 || {};

    const activeRows = [];
    if (gcs.configured) {
      const sourceNote = gcs.source === 'environment' ? 'server environment' : 'encrypted in database';
      activeRows.push({
        provider: 'gcs',
        title: 'Google Cloud Storage (gs://)',
        subtitle: `${gcs.client_email || 'Service Account'} · ${sourceNote}`
      });
    }
    if (s3.configured) {
      activeRows.push({
        provider: 's3',
        title: 'S3 / MinIO Compatible (s3://)',
        subtitle: `${s3.endpoint_url || ''} · ${s3.region || 'us-east-1'} · ${s3.access_key_id || ''}`
      });
    }

    if (activeRows.length === 0) {
      storageListEl.innerHTML = `<div class="storage-conn-empty">No active storage connections.</div>`;
      return;
    }

    storageListEl.innerHTML = activeRows.map(item => `
      <div class="storage-conn-row">
        <div class="storage-conn-info">
          <span class="storage-conn-title">${UIUtils.escapeHTML(item.title)}</span>
          <span class="storage-conn-detail">${UIUtils.escapeHTML(item.subtitle)}</span>
        </div>
        <button type="button" class="secondary-button artifact-btn-sm btn-revoke-storage" data-provider="${item.provider}" style="border-color: #dc3545; color: #dc3545; flex-shrink: 0;">Revoke</button>
      </div>
    `).join('');

    storageListEl.querySelectorAll('.btn-revoke-storage').forEach(btn => {
      btn.onclick = async () => {
        const provider = btn.getAttribute('data-provider');
        const label = provider === 'gcs' ? 'Google Cloud Storage (gs://)' : 'S3 / MinIO (s3://)';
        if (!confirm(`Revoke ${label} signing key?`)) return;
        try {
          btn.disabled = true;
          const payload = provider === 'gcs' ? { clear_gcs: true } : { clear_s3: true };
          const res = await ApiUtils.request('/api/v1/admin/storage-connections', {
            method: 'PUT',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(payload)
          });
          renderStorageConnectionsList(res);
          UIUtils.showToast(`${label} key revoked.`, 'success');
        } catch (e) {
          UIUtils.handleError(e, 'Failed to revoke storage key');
        }
      };
    });
  }

  async function loadStorageConnectionsStatus() {
    try {
      const res = await ApiUtils.request('/api/v1/admin/storage-connections');
      renderStorageConnectionsList(res);
    } catch (_e) {
      if (storageListEl) {
        storageListEl.innerHTML = `<div class="storage-conn-empty">Could not load storage connections.</div>`;
      }
    }
  }

  if (saveStorageBtn) {
    saveStorageBtn.onclick = async () => {
      const provider = providerSelect.value;
      let payload = null;

      if (provider === 'gcs') {
        const raw = gcsInput.value.trim();
        if (!raw) return UIUtils.showToast('Paste or load a Service Account JSON key first.', 'warning');
        payload = { gcs_service_account_json: raw };
      } else {
        const accessKey = s3AccessKeyInput.value.trim();
        const secretKey = s3SecretKeyInput.value.trim();
        if (!accessKey || !secretKey) {
          return UIUtils.showToast('Access Key ID and Secret Access Key are required.', 'warning');
        }
        payload = {
          s3: {
            endpoint_url: s3EndpointInput.value.trim() || 'https://s3.amazonaws.com',
            region: s3RegionInput.value.trim() || 'us-east-1',
            access_key_id: accessKey,
            secret_access_key: secretKey
          }
        };
      }

      try {
        saveStorageBtn.disabled = true;
        saveStorageBtn.textContent = 'Saving...';
        const res = await ApiUtils.request('/api/v1/admin/storage-connections', {
          method: 'PUT',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(payload)
        });
        gcsInput.value = '';
        s3SecretKeyInput.value = '';
        if (storageFormPanel) storageFormPanel.style.display = 'none';
        renderStorageConnectionsList(res);
        UIUtils.showToast('Storage connection saved (encrypted).', 'success');
      } catch (e) {
        UIUtils.handleError(e, 'Failed to save storage connection');
      } finally {
        saveStorageBtn.disabled = false;
        saveStorageBtn.textContent = 'Save Connection';
      }
    };
  }

  loadStorageConnectionsStatus();
}
