/* src/server/static/js/strain-details.js */

async function loadStrainDetailsPage(mainElement, strainId) {
  const backLinkHtml = generateBackArrow('/strains');

  // 1. Setup Shell
  mainElement.innerHTML = `
    <div class="page-header">
      <div style="display:flex; align-items:center; gap:15px;">
          ${backLinkHtml}
          <h2>Strain Details</h2>
      </div>
      <div id="action-buttons">
          <button id="edit-btn" class="button">Edit</button>
          <button id="save-btn" class="button" style="display:none; background-color: #28a745; border-color: #28a745;">Save</button>
          <button id="cancel-btn" class="secondary-button" style="display:none;">Cancel</button>
      </div>
    </div>
    
    <div id="strain-details-container">Loading strain details...</div>
  `;

  const container = document.getElementById('strain-details-container');
  const editBtn = document.getElementById('edit-btn');
  const saveBtn = document.getElementById('save-btn');
  const cancelBtn = document.getElementById('cancel-btn');

  let currentStrainData = null;
  let parentStrainData = null; // New: Store parent info here
  let allStrainsCache = null;

  // --- Helper: Render Read-Only View ---
  function renderViewMode() {
    const s = currentStrainData;
    const created = new Date(s.created_at * 1000).toLocaleString();
    const updated = new Date(s.updated_at * 1000).toLocaleString();

    // Logic for Parent Link
    let parentHtml = '<span class="text-secondary">None (Wild Type)</span>';
    if (s.parent_strain_id) {
      if (parentStrainData) {
        parentHtml = `<a href="/strains/${parentStrainData.id}" class="badge"><span class="emoji">🧬</span> ${UIUtils.escapeHTML(parentStrainData.strain_name)}</a>`;
      } else {
        parentHtml = `<a href="/strains/${s.parent_strain_id}" class="badge"><span class="emoji">🧬</span> Strain #${s.parent_strain_id}</a>`;
      }
    }

    // External Links
    let linksHtml = '';
    if (s.external_links && s.external_links.length > 0) {
      linksHtml = s.external_links.map(link => {
        const href = link.resolved_url || '#';
        const label = `${UIUtils.escapeHTML(link.db_name)}: ${UIUtils.escapeHTML(link.value)}`;

        const linkTag = link.resolved_url
          ? `<a href="${href}" target="_blank" class="badge">${label}</a>`
          : `<span class="badge">${label} (No URL)</span>`;

        return linkTag;
      }).join(' ');
    } else {
      linksHtml = '<span class="text-secondary">No external links</span>';
    }

    container.innerHTML = `
        <div class="info-grid" id="view-grid">
            <div class="info-item"><span class="info-key">ID</span><span class="info-value">${s.id}</span></div>
            <div class="info-item"><span class="info-key">Genus</span><span class="info-value">${UIUtils.escapeHTML(s.genus)}</span></div>
            <div class="info-item"><span class="info-key">Species</span><span class="info-value">${UIUtils.escapeHTML(s.species)}</span></div>
            <div class="info-item"><span class="info-key">Strain Name</span><span class="info-value">${UIUtils.escapeHTML(s.strain_name)}</span></div>
            <div class="info-item"><span class="info-key">Genotype</span><span class="info-value">${s.genotype ? UIUtils.escapeHTML(s.genotype) : '<span class="text-secondary">N/A</span>'}</span></div>
            <div class="info-item"><span class="info-key">Parent</span><span class="info-value">${parentHtml}</span></div>
            <div class="info-item"><span class="info-key">Notes</span><span class="info-value" style="white-space: pre-wrap;">${s.notes ? UIUtils.escapeHTML(s.notes) : ''}</span></div>
            <div class="info-item"><span class="info-key">Created</span><span class="info-value">${created}</span></div>
            <div class="info-item"><span class="info-key">Updated</span><span class="info-value">${updated}</span></div>
        </div>

        <div style="margin-top: 2rem;">
            <div class="table-header">
                <h3>External Links</h3>
                <button id="add-link-btn" class="button secondary-button">+ Add</button>
            </div>
            <div class="badge-container">${linksHtml}</div>
            <div id="add-link-form" style="display:none; margin-top: 1rem; background: var(--bg-light); padding: 1rem; border-radius: 6px;">
                <div class="input-group">
                    <label for="db-select">Database</label>
                    <div class="select-wrapper">
                        <select id="db-select"></select>
                    </div>
                </div>
                <div class="input-group">
                    <label for="link-value">Value / ID</label>
                    <input type="text" id="link-value" placeholder="e.g. 12345">
                </div>
                <div>
                    <button id="save-link-btn" class="button">Save Link</button>
                    <button id="cancel-link-btn" class="secondary-button">Cancel</button>
                </div>
            </div>
        </div>

        <div style="margin-top: 2rem;">
            <h3>Source Samples</h3>
            <table id="source-samples-table" class="display" style="width:100%">
                <thead><tr><th>Short ID</th><th>Name</th><th>Project</th></tr></thead>
                <tbody></tbody>
            </table>
        </div>
      `;

    const sourceTable = $('#source-samples-table');
    if (s.source_samples && s.source_samples.length > 0) {
      const tableBody = s.source_samples.map(entry => `
                <tr>
                    <td><a href="/samples/${entry.sample.id}?from=strain&context_id=${s.id}&context_name=${encodeURIComponent(s.strain_name)}" class="id-link">${UIUtils.escapeHTML(entry.sample.short_id)}</a></td>
                    <td>${UIUtils.escapeHTML(entry.sample.short_id || entry.sample.id)}</td>
                    <td><a href="/projects/${entry.project.id}?from=strain&context_id=${s.id}&context_name=${encodeURIComponent(s.strain_name)}">${UIUtils.escapeHTML(entry.project.name)}</a></td>
                </tr>
            `).join('');
      sourceTable.find('tbody').html(tableBody);
      sourceTable.DataTable({ pageLength: 5, lengthChange: false });
    } else {
      sourceTable.find('tbody').html('<tr><td colspan="3" class="text-secondary" style="text-align:center;">This strain is not used in any source samples.</td></tr>');
    }


    editBtn.style.display = 'inline-block';
    saveBtn.style.display = 'none';
    cancelBtn.style.display = 'none';

    // --- Event Listeners for Link Form ---
    const addLinkBtn = document.getElementById('add-link-btn');
    const addLinkForm = document.getElementById('add-link-form');
    const cancelLinkBtn = document.getElementById('cancel-link-btn');
    const saveLinkBtn = document.getElementById('save-link-btn');
    const dbSelect = document.getElementById('db-select');

    addLinkBtn.addEventListener('click', async () => {
      addLinkForm.style.display = 'block';
      addLinkBtn.style.display = 'none';

      try {
        const res = await fetch('/api/v1/external-db-definitions');
        const json = await res.json();
        dbSelect.innerHTML = json.data.map(db => `<option value="${db.id}">${UIUtils.escapeHTML(db.name)}</option>`).join('');
      } catch (err) {
        console.error("Failed to load DB definitions", err);
        dbSelect.innerHTML = '<option>Error loading</option>';
      }
    });

    cancelLinkBtn.addEventListener('click', () => {
      addLinkForm.style.display = 'none';
      addLinkBtn.style.display = 'inline-block';
    });

    saveLinkBtn.addEventListener('click', async () => {
      const dbId = dbSelect.value;
      const linkValue = document.getElementById('link-value').value.trim();

      if (!dbId || !linkValue) return UIUtils.showToast("Please select a database and provide a value.", "error");

      const payload = {
        strain_id: parseInt(strainId, 10),
        external_db_definition_id: parseInt(dbId, 10),
        value: linkValue
      };

      try {
        const res = await fetch(`/api/v1/strain-external-links`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(payload)
        });

        if (!res.ok) {
          const err = await res.json();
          throw new Error(err.error || "Failed to save link");
        }

        // Targeted Refetch instead of window.location.reload()
        await fetchStrainData();

      } catch (err) {
        console.error(err);
        UIUtils.showToast(`Failed to save link: ${err.message}`, "error");
      }
    });
  }

  // --- Helper: Render Edit Mode ---
  let parentAutocomplete = null;

  async function renderEditMode() {
    const s = currentStrainData;

    container.innerHTML = `
        <div class="form-container" id="edit-grid">
            <div class="input-group">
                <label>Genus</label>
                <input type="text" id="edit-genus" value="${UIUtils.escapeHTML(s.genus)}" />
            </div>
            <div class="input-group">
                <label>Species</label>
                <input type="text" id="edit-species" value="${UIUtils.escapeHTML(s.species)}" />
            </div>
            <div class="input-group">
                <label>Strain Name</label>
                <input type="text" id="edit-strain" value="${UIUtils.escapeHTML(s.strain_name)}" />
            </div>
            <div class="input-group">
                <label>Genotype</label>
                <input type="text" id="edit-genotype" value="${UIUtils.escapeHTML(s.genotype || '')}" />
            </div>
            <div class="input-group">
                <label>Parent Strain</label>
                <div id="edit-parent-container"></div>
            </div>
            <div class="input-group">
                <label>Notes</label>
                <textarea id="edit-notes" rows="3">${UIUtils.escapeHTML(s.notes || '')}</textarea>
            </div>
        </div>
      `;

    const parentContainer = document.getElementById('edit-parent-container');
    const initialParent = parentStrainData ? { id: parentStrainData.id, label: `[${parentStrainData.strain_name}] ${parentStrainData.genus} ${parentStrainData.species}` } : null;

    parentAutocomplete = new AutocompleteComponent(parentContainer, {
        placeholder: 'Search for a parent strain...',
        initialValue: initialParent,
        fetchItems: async (query) => {
            const res = await fetch(`/api/v1/strains?search=${encodeURIComponent(query)}`);
            const json = await res.json();
            return json.data
                .filter(other => other.id !== s.id) // Cannot be its own parent
                .map(other => ({
                    id: other.id,
                    label: `[${other.strain_name}] ${other.genus} ${other.species}`
                }));
        }
    });

    editBtn.style.display = 'none';
    saveBtn.style.display = 'inline-block';
    cancelBtn.style.display = 'inline-block';

    if (window.attachSpecialCharHelper) {
      window.attachSpecialCharHelper('edit-genotype');
      window.attachSpecialCharHelper('edit-notes');
    }
  }

  // --- Main Logic: Load Data ---
  async function fetchStrainData() {
    try {
      const response = await fetch(`/api/v1/strains/${strainId}`);
      if (!response.ok) throw new Error(`Strain not found (ID: ${strainId})`);

      const json = await response.json();
      currentStrainData = { ...json.strain, external_links: json.external_links, source_samples: json.source_samples };

      if (currentStrainData.parent_strain_id) {
        try {
          const pRes = await fetch(`/api/v1/strains/${currentStrainData.parent_strain_id}`);
          if (pRes.ok) {
            const pJson = await pRes.json();
            parentStrainData = pJson.strain;
          }
        } catch (e) {
          console.warn("Could not fetch parent strain", e);
        }
      } else {
        parentStrainData = null;
      }

      renderViewMode();

    } catch (error) {
      console.error('Error loading strain details:', error);
      container.innerHTML = `<div class="error">Error: ${error.message}</div>`;
    }
  }

  await fetchStrainData();

  // --- Event Listeners ---
  editBtn.addEventListener('click', renderEditMode);
  cancelBtn.addEventListener('click', renderViewMode);

  saveBtn.addEventListener('click', async () => {
    const genus = document.getElementById('edit-genus').value.trim();
    const species = document.getElementById('edit-species').value.trim();
    const strain = document.getElementById('edit-strain').value.trim();
    const genotype = document.getElementById('edit-genotype').value.trim();
    const parentVal = parentAutocomplete ? parentAutocomplete.getValue() : null;
    const notes = document.getElementById('edit-notes').value.trim();

    if (!genus || !species || !strain) {
      UIUtils.showToast("Genus, Species, and Strain Name are required.", "error");
      return;
    }

    const payload = {
      genus: genus,
      species: species,
      strain_name: strain,
      genotype: genotype || null,
      parent_strain_id: parentVal ? parseInt(parentVal, 10) : null,
      notes: notes || null
    };

    Object.keys(payload).forEach(k => payload[k] === null && delete payload[k]);

    try {
      const res = await fetch(`/api/v1/strains/${strainId}`, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });

      if (!res.ok) {
        const err = await res.json();
        throw new Error(err.error || "Update failed");
      }
      
      // Targeted Refetch instead of window.location.reload()
      await fetchStrainData();

    } catch (err) {
      console.error(err);
      UIUtils.showToast(`Failed to save: ${err.message}`, "error");
    }
  });
}
