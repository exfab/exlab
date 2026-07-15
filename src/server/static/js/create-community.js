/* src/server/static/js/create-community.js */

async function loadCreateCommunityPage(mainElement) {
  const backLinkHtml = generateBackArrow('/communities');

  let metadataTemplate = [];
  try {
    const res = await ApiUtils.request('/api/v1/settings/community_metadata_template');
    if (Array.isArray(res)) metadataTemplate = res;
  } catch(e) {
    console.warn("Could not fetch community metadata template", e);
  }

  mainElement.innerHTML = `
    <div class="page-header">
      ${backLinkHtml}
      <h2>Create Community</h2>
    </div>
    <div class="form-container">

      <div class="input-group">
        <label for="community-name">Name <span style="color:red">*</span></label>
        <input type="text" id="community-name" placeholder="e.g. Gut Microbiome Consortium" />
      </div>

      <div class="input-group">
        <label for="notes">Notes</label>
        <textarea id="notes" rows="3" placeholder="Additional details..."></textarea>
      </div>

      <div class="input-group" style="margin-top: 2rem;">
        <label>Metadata</label>
        <div id="metadata-rows" style="display: flex; flex-direction: column; gap: 10px; margin-bottom: 10px;">
        </div>
      </div>

      <button id="save-button" class="button" style="margin-top: 1rem;">Save Community</button>
    </div>
  `;

  const metadataContainer = document.getElementById('metadata-rows');

  function addMetadataRow(def) {
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
      <div style="display: flex; gap: 10px; align-items: center;">
        <input type="text" class="meta-key" placeholder="Key" value="${UIUtils.escapeHTML(def.key)}" readonly style="background-color: var(--bg-dark); cursor: not-allowed;">
        ${valueInputHtml}
      </div>
    `;
    metadataContainer.insertAdjacentHTML('beforeend', rowHtml);
  }

  metadataTemplate.forEach(def => addMetadataRow(def));

  const nameInput = document.getElementById('community-name');
  const notesInput = document.getElementById('notes');
  const saveButton = document.getElementById('save-button');

  saveButton.addEventListener('click', async () => {
    const name = nameInput.value.trim();
    const notes = notesInput.value.trim();

    if (!name) return UIUtils.showToast('Please enter a Name.', "error");

    const metadata = {};
    document.querySelectorAll('#metadata-rows > div').forEach(row => {
      const key = row.querySelector('.meta-key').value.trim();
      const val = row.querySelector('.meta-value').value.trim();
      if (key) metadata[key] = val;
    });

    const payload = { name, ...(notes ? { notes } : {}) };
    if (Object.keys(metadata).length > 0) payload.metadata = metadata;

    try {
      const res = await fetch('/api/v1/communities', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });

      if (res.ok) {
        history.pushState({}, "", "/communities");
        handleRouting();
      } else {
        const err = await res.json();
        UIUtils.showToast(`Error: ${err.error || 'Failed to create community'}`, "error");
      }
    } catch (error) {
      console.error('Error saving community:', error);
      UIUtils.showToast('Network error occurred.', "error");
    }
  });
}