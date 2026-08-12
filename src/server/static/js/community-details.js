/* src/server/static/js/community-details.js */

async function loadCommunityDetailsPage(mainElement, communityId) {
  const backLinkHtml = generateBackArrow('/communities');

  mainElement.innerHTML = `
    <div class="page-header">
      <div style="display:flex; align-items:center; gap:15px;">
          ${backLinkHtml}
          <h2>Community Details</h2>
      </div>
      <div id="action-buttons">
          <button id="edit-btn" class="button">Edit</button>
          <button id="save-btn" class="button" style="display:none; background-color: #28a745; border-color: #28a745;">Save</button>
          <button id="cancel-btn" class="secondary-button" style="display:none;">Cancel</button>
      </div>
    </div>

    <div id="community-details-container">Loading community details...</div>
  `;

  const container = document.getElementById('community-details-container');
  const editBtn = document.getElementById('edit-btn');
  const saveBtn = document.getElementById('save-btn');
  const cancelBtn = document.getElementById('cancel-btn');

  let currentData = null;
  let strainMap = {};
  let strainData = [];
  let metadataTemplate = [];

  function renderViewMode() {
    const c = currentData.community;
    const members = currentData.members || [];
    const sourceSamples = currentData.source_samples || [];
    const created = new Date(c.created_at * 1000).toLocaleString();
    const updated = new Date(c.updated_at * 1000).toLocaleString();

    let membersHtml = '';
    if (members.length > 0) {
      membersHtml = members.map(m => {
        const strainLink = m.strain_id
          ? `<a href="/strains/${m.strain_id}" class="badge"><span class="emoji">🧬</span> ${UIUtils.escapeHTML(strainMap[m.strain_id] || 'Strain ' + m.strain_id)}</a>`
          : '<span class="text-secondary">Unknown</span>';
        return `
          <tr>
            <td>${UIUtils.escapeHTML(m.label)}</td>
            <td>${strainLink}</td>
            <td>${m.taxon ? UIUtils.escapeHTML(m.taxon) : '<span class="text-secondary">—</span>'}</td>
            <td>
              <button class="button-small edit-member-btn" data-id="${m.id}" data-label="${UIUtils.escapeHTML(m.label)}" data-strain-id="${m.strain_id || ''}" data-taxon="${UIUtils.escapeHTML(m.taxon || '')}">Edit</button>
              <button class="button-small button-danger remove-member-btn" data-id="${m.id}">Remove</button>
            </td>
          </tr>`;
      }).join('');
    } else {
      membersHtml = '<tr><td colspan="4" class="text-secondary" style="text-align:center;">No members in this community.</td></tr>';
    }

    let sourceHtml = '';
    if (sourceSamples.length > 0) {
      sourceHtml = sourceSamples.map(entry => `
        <tr>
          <td><a href="/samples/${entry.sample.id}?from=community&context_id=${c.id}&context_name=${encodeURIComponent(c.name)}" class="id-link">${UIUtils.escapeHTML(entry.sample.short_id)}</a></td>
          <td>${UIUtils.escapeHTML(entry.sample.short_id || entry.sample.id)}</td>
          <td><a href="/projects/${entry.project.id}?from=community&context_id=${c.id}&context_name=${encodeURIComponent(c.name)}">${UIUtils.escapeHTML(entry.project.name)}</a></td>
        </tr>`).join('');
    } else {
      sourceHtml = '<tr><td colspan="3" class="text-secondary" style="text-align:center;">No source samples use this community.</td></tr>';
    }

    let metadataHtml = '';
    if (c.metadata && typeof c.metadata === 'object' && Object.keys(c.metadata).length > 0) {
      Object.keys(c.metadata).forEach(key => {
        const val = typeof c.metadata[key] === 'object' ? JSON.stringify(c.metadata[key]) : c.metadata[key];
        metadataHtml += `<div class="info-item"><span class="info-key">${key}</span><span class="info-value">${val || '-'}</span></div>`;
      });
    }

    container.innerHTML = `
      <div class="info-grid" id="view-grid">
        <div class="info-item"><span class="info-key">ID</span><span class="info-value">${c.id}</span></div>
        <div class="info-item"><span class="info-key">Name</span><span class="info-value">${UIUtils.escapeHTML(c.name)}</span></div>
        <div class="info-item"><span class="info-key">Notes</span><span class="info-value" style="white-space: pre-wrap;">${UIUtils.escapeHTML(c.notes || '')}</span></div>
        ${metadataHtml}
        <div class="info-item"><span class="info-key">Created</span><span class="info-value">${created}</span></div>
        <div class="info-item"><span class="info-key">Updated</span><span class="info-value">${updated}</span></div>
      </div>

      <div style="margin-top: 2rem;">
        <div class="table-header">
          <h3>Members</h3>
          <button id="add-member-btn" class="button secondary-button">+ Add Member</button>
        </div>
        <table id="members-table" class="display" style="width:100%">
          <thead><tr><th>Label</th><th>Strain</th><th>Taxon</th><th>Actions</th></tr></thead>
          <tbody>${membersHtml}</tbody>
        </table>
      </div>

      <div id="add-member-form" style="display:none; margin-top: 1rem; background: var(--bg-light); padding: 1rem; border-radius: 6px;">
        <div class="input-group">
          <label for="member-label">Label <span style="color:red">*</span></label>
          <input type="text" id="member-label" placeholder="e.g. OTU_001">
        </div>
        <div class="input-group">
          <label for="member-strain-container">Strain</label>
          <div id="member-strain-container"></div>
        </div>
        <div class="input-group">
          <label for="member-taxon">Taxon</label>
          <input type="text" id="member-taxon" placeholder="e.g. Bacteria;Proteobacteria;Gammaproteobacteria">
        </div>
        <div>
          <button id="save-member-btn" class="button">Save</button>
          <button id="cancel-member-btn" class="secondary-button">Cancel</button>
        </div>
      </div>

      <div style="margin-top: 2rem;">
        <h3>Source Samples</h3>
        <table id="source-samples-table" class="display" style="width:100%">
          <thead><tr><th>Short ID</th><th>Name</th><th>Project</th></tr></thead>
          <tbody>${sourceHtml}</tbody>
        </table>
      </div>
    `;

    editBtn.style.display = 'inline-block';
    saveBtn.style.display = 'none';
    cancelBtn.style.display = 'none';

    const memberTable = $('#members-table');
    if (members.length > 0) {
      memberTable.DataTable({ pageLength: 5, lengthChange: false });
    }

    const sourceTable = $('#source-samples-table');
    if (sourceSamples.length > 0) {
      sourceTable.DataTable({ pageLength: 5, lengthChange: false });
    }

    wireMemberForm(c);
  }

  let memberStrainAutocomplete = null;

  function wireMemberForm(c) {
    const addMemberBtn = document.getElementById('add-member-btn');
    const addMemberForm = document.getElementById('add-member-form');
    const cancelMemberBtn = document.getElementById('cancel-member-btn');
    const saveMemberBtn = document.getElementById('save-member-btn');
    const memberLabel = document.getElementById('member-label');
    const memberStrainContainer = document.getElementById('member-strain-container');
    const memberTaxon = document.getElementById('member-taxon');

    let editingMemberId = null;
    
    if (!memberStrainAutocomplete && memberStrainContainer) {
      memberStrainAutocomplete = new AutocompleteComponent(memberStrainContainer, {
        placeholder: 'Search for a strain (optional)...',
        fetchItems: async (query) => {
          const res = await fetch(`/api/v1/strains?search=${encodeURIComponent(query)}`);
          const json = await res.json();
          return json.data.map(s => ({
            id: s.id,
            label: `[${s.strain_name}] ${s.genus} ${s.species || 'Unknown'}`
          }));
        }
      });
    }

    function resetForm() {
      editingMemberId = null;
      memberLabel.value = '';
      if (memberStrainAutocomplete) {
        memberStrainAutocomplete.selectItem(null);
        if (memberStrainAutocomplete.inputElement) memberStrainAutocomplete.inputElement.value = '';
      }
      memberTaxon.value = '';
    }

    addMemberBtn.addEventListener('click', async () => {
      addMemberForm.style.display = 'block';
      addMemberBtn.style.display = 'none';
      resetForm();
    });

    cancelMemberBtn.addEventListener('click', () => {
      addMemberForm.style.display = 'none';
      addMemberBtn.style.display = 'inline-block';
    });

    saveMemberBtn.addEventListener('click', async () => {
      const label = memberLabel.value.trim();
      const strainId = memberStrainAutocomplete ? memberStrainAutocomplete.getValue() : null;
      const taxon = memberTaxon.value.trim();

      if (!label) return UIUtils.showToast("Label is required.", "error");

      const payload = {
        label,
        ...(strainId ? { strain_id: parseInt(strainId, 10) } : {}),
        ...(taxon ? { taxon: taxon } : {})
      };

      try {
        const url = editingMemberId
          ? `/api/v1/communities/${c.id}/members/${editingMemberId}`
          : `/api/v1/communities/${c.id}/members`;

        const method = editingMemberId ? 'PUT' : 'POST';

        const res = await fetch(url, {
          method,
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(payload)
        });

        if (!res.ok) {
          const err = await res.json();
          throw new Error(err.error || "Failed to save member");
        }

        await fetchCommunityData();

      } catch (err) {
        console.error(err);
        UIUtils.showToast(`Failed to save member: ${err.message}`, "error");
      }
    });

    document.querySelectorAll('.edit-member-btn').forEach(btn => {
      btn.addEventListener('click', () => {
        editingMemberId = parseInt(btn.dataset.id, 10);
        memberLabel.value = btn.dataset.label;
        memberStrain.value = btn.dataset.strainId;
        memberTaxon.value = btn.dataset.taxon;
        addMemberForm.style.display = 'block';
        addMemberBtn.style.display = 'none';

        memberStrain.innerHTML = '<option value="">(Unknown / Uncultured)</option>' +
          strainData.map(s => `<option value="${s.id}" ${String(s.id) === btn.dataset.strainId ? 'selected' : ''}>${s.strain_name} (${s.genus} ${s.species || 'Unknown'})</option>`).join('');
      });
    });

    document.querySelectorAll('.remove-member-btn').forEach(btn => {
      btn.addEventListener('click', async () => {
        if (!confirm("Remove this member from the community?")) return;
        try {
          const res = await fetch(`/api/v1/communities/${c.id}/members/${btn.dataset.id}`, { method: 'DELETE' });
          if (!res.ok) {
            const err = await res.json();
            throw new Error(err.error || "Failed to remove member");
          }
          await fetchCommunityData();
        } catch (err) {
          console.error(err);
          UIUtils.showToast(`Failed to remove member: ${err.message}`, "error");
        }
      });
    });
  }

  async function renderEditMode() {
    const c = currentData.community;
    const currentMeta = (c.metadata && typeof c.metadata === 'object') ? c.metadata : {};

    const combinedKeys = new Set();
    Object.keys(currentMeta).forEach(k => combinedKeys.add(k));
    metadataTemplate.forEach(def => combinedKeys.add(def.key));

    let metadataRows = '';
    combinedKeys.forEach(key => {
      const def = metadataTemplate.find(d => d.key === key);
      const existingVal = currentMeta[key] !== undefined ? String(currentMeta[key]) : '';
      let valueInputHtml = '';

      if (def && typeof def.field_type === 'object' && def.field_type[0] === 'Enum') {
        const options = def.field_type[1];
        const opts = options.map(opt =>
          `<option value="${opt}" ${existingVal === opt ? 'selected' : ''}>${opt}</option>`
        ).join('');
        valueInputHtml = `<select class="meta-value" style="flex-grow: 1; margin-bottom: 0;">
          <option value="">Select a value...</option>${opts}</select>`;
      } else {
        valueInputHtml = `<input type="text" class="meta-value" value="${existingVal}" placeholder="Value" style="flex-grow: 1;">`;
      }

      metadataRows += `
        <div style="display: flex; gap: 10px; align-items: center;">
          <input type="text" class="meta-key" value="${key}" readonly style="background-color: var(--bg-dark); cursor: not-allowed; width: 200px;">
          ${valueInputHtml}
        </div>`;
    });

    container.innerHTML = `
      <div class="form-container" id="edit-grid">
        <div class="input-group">
          <label>Name</label>
          <input type="text" id="edit-name" value="${UIUtils.escapeHTML(c.name)}" />
        </div>
        <div class="input-group">
          <label>Notes</label>
          <textarea id="edit-notes" rows="3">${UIUtils.escapeHTML(c.notes || '')}</textarea>
        </div>
        <div class="input-group" style="margin-top: 1rem;">
          <label>Metadata</label>
          <div id="metadata-rows" style="display: flex; flex-direction: column; gap: 10px; margin-bottom: 10px;">
            ${metadataRows}
          </div>
        </div>
      </div>
    `;

    editBtn.style.display = 'none';
    saveBtn.style.display = 'inline-block';
    cancelBtn.style.display = 'inline-block';
  }

  async function fetchCommunityData() {
    try {
      const [communityRes, templateRes] = await Promise.all([
          fetch(`/api/v1/communities/${communityId}`),
          fetch('/api/v1/settings/community_metadata_template').then(r => r.ok ? r.json() : []).catch(() => [])
      ]);

      if (!communityRes.ok) throw new Error(`Community not found (ID: ${communityId})`);

      currentData = await communityRes.json();
      
      // Strain names are not included in the members list; renderers fall back
      // to the strain ID when a name lookup is unavailable.
      strainMap = {}; 
      
      metadataTemplate = Array.isArray(templateRes) ? templateRes : [];

      renderViewMode();

    } catch (error) {
      console.error('Error loading community details:', error);
      container.innerHTML = `<div class="error">Error: ${error.message}</div>`;
    }
  }

  await fetchCommunityData();

  editBtn.addEventListener('click', renderEditMode);
  cancelBtn.addEventListener('click', renderViewMode);

  saveBtn.addEventListener('click', async () => {
    const name = document.getElementById('edit-name').value.trim();
    const notes = document.getElementById('edit-notes').value.trim();

    if (!name) {
      UIUtils.showToast("Name is required.", "error");
      return;
    }

    const metadata = {};
    document.querySelectorAll('#metadata-rows > div').forEach(row => {
      const key = row.querySelector('.meta-key').value.trim();
      const val = row.querySelector('.meta-value').value.trim();
      if (key) metadata[key] = val;
    });

    const payload = { name, ...(notes ? { notes } : {}) };
    if (Object.keys(metadata).length > 0) payload.metadata = metadata;

    try {
      const res = await fetch(`/api/v1/communities/${communityId}`, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });

      if (!res.ok) {
        const err = await res.json();
        throw new Error(err.error || "Update failed");
      }
      await fetchCommunityData();

    } catch (err) {
      console.error(err);
      UIUtils.showToast(`Failed to save: ${err.message}`, "error");
    }
  });
}
