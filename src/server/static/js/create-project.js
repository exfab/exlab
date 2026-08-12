/* src/server/static/js/create-project.js */

async function loadCreateProjectPage(mainElement) {
  const backLinkHtml = generateBackArrow('/projects');

  // Fetch metadata template
  let metadataTemplate = [];
  try {
    const res = await ApiUtils.request('/api/v1/settings/project_metadata_template');
    if (Array.isArray(res)) metadataTemplate = res;
  } catch(e) {
    console.warn("Could not fetch metadata template", e);
  }

  mainElement.innerHTML = `
    <div class="page-header">
      ${backLinkHtml}
      <h2>Create Project</h2>
    </div>
    <div class="form-container">
      <div class="input-group">
        <label for="project-name">Project Name <span style="color:red">*</span></label>
        <input type="text" id="project-name" placeholder="Enter project name" />
      </div>
      <div class="input-group">
        <label for="project-short-id">Short ID (Optional)</label>
        <input type="text" id="project-short-id" placeholder="e.g. CUSTOM-123" />
      </div>
      <div class="input-group">
        <label for="project-description">Description</label>
        <input type="text" id="project-description" placeholder="Enter project description" />
      </div>
      <div class="input-group">
        <label for="project-contact">Contact Name</label>
        <input type="text" id="project-contact" placeholder="Enter contact name" />
      </div>
      <div class="input-group">
        <label for="project-owner">Owner</label>
        <input type="text" id="project-owner" placeholder="Enter owner email" />
      </div>

      <div class="input-group" style="margin-top: 2rem;">
        <label>Metadata</label>
        <div id="metadata-rows" style="display: flex; flex-direction: column; gap: 10px; margin-bottom: 10px;">
          <!-- Rows will be injected here -->
        </div>
      </div>

      <button id="save-button" class="button" style="margin-top: 1rem;">Save</button>
    </div>
  `;

  const metadataContainer = document.getElementById('metadata-rows');

  function addMetadataRow(def) {
    const rowId = 'meta-' + Date.now() + Math.random().toString(36).substr(2, 5);
    let valueInputHtml = '';
    
    if (def.field_type === 'String' || (typeof def.field_type === 'object' && def.field_type[0] === 'String')) {
        valueInputHtml = `<input type="text" class="meta-value" placeholder="Value" style="flex-grow: 1;">`;
    } else if (typeof def.field_type === 'object' && def.field_type[0] === 'Enum') {
        const options = def.field_type[1];
        const optionsHtml = options.map(opt => `<option value="${UIUtils.escapeHTML(opt)}">${UIUtils.escapeHTML(opt)}</option>`).join('');
        valueInputHtml = `
            <select class="meta-value" style="flex-grow: 1; margin-bottom: 0;">
                <option value="">Select a value...</option>
                ${optionsHtml}
            </select>`;
    }

    const rowHtml = `
      <div id="${rowId}" style="display: flex; gap: 10px; align-items: center;">
        <input type="text" class="meta-key" placeholder="Key" value="${UIUtils.escapeHTML(def.key)}" readonly style="background-color: var(--bg-dark); cursor: not-allowed;">
        ${valueInputHtml}
      </div>
    `;
    metadataContainer.insertAdjacentHTML('beforeend', rowHtml);
  }

  // Inject templates
  metadataTemplate.forEach(def => addMetadataRow(def));

  const nameInput = document.getElementById('project-name');
  const shortIdInput = document.getElementById('project-short-id');
  const descriptionInput = document.getElementById('project-description');
  const contactInput = document.getElementById('project-contact');
  const ownerInput = document.getElementById('project-owner');
  const saveButton = document.getElementById('save-button');

  saveButton.addEventListener('click', async () => {
    const projectName = nameInput.value.trim();
    const projectShortId = shortIdInput.value.trim();
    const projectDescription = descriptionInput.value.trim();
    const projectContact = contactInput.value.trim();
    const projectOwner = ownerInput.value.trim();

    if (!projectName) {
      UIUtils.showToast('Please enter a project name.', "error");
      return;
    }

    // Gather Metadata
    const metadata = {};
    document.querySelectorAll('#metadata-rows > div').forEach(row => {
      const key = row.querySelector('.meta-key').value.trim();
      const val = row.querySelector('.meta-value').value.trim();
      if (key) metadata[key] = val; // Metadata values are stored as strings
    });

    const payload = {
      name: projectName,
      short_id: projectShortId || null,
      description: projectDescription || null,
      contact_name: projectContact || null,
      owner: projectOwner || null,
      status: "Pending",
      metadata: Object.keys(metadata).length > 0 ? metadata : null
    };

    Object.keys(payload).forEach(k => payload[k] === null && delete payload[k]);

    try {
      const response = await fetch('/api/v1/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });

      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.error);
      }

      // Redirect to the projects page
      history.pushState({}, "", "/projects");
      handleRouting();
    } catch (error) {
      UIUtils.showToast(`Error creating project: ${error.message}`, "error");
    }
  });

  if (window.attachSpecialCharHelper) {
    window.attachSpecialCharHelper('project-description');
  }
}
