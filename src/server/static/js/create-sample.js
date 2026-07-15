/* src/server/static/js/create-sample.js */

async function loadCreateSamplePage(mainElement) {
  const backLinkHtml = generateBackArrow('/samples');

  mainElement.innerHTML = `
    <div class="page-header">
      ${backLinkHtml}
      <h2>Create Samples</h2>
    </div>

    <form id="create-sample-form" class="form-container">
      
      <div class="input-group">
        <label for="project-select">Project</label>
        <div class="select-wrapper">
            <select id="project-select" required>
              <option value="">Loading projects...</option>
            </select>
        </div>
      </div>

      <div class="input-group">
        <label for="sample-category">Category</label>
        <div class="select-wrapper">
            <select id="sample-category" required>
              <option value="Source">Source (from Strain)</option>
              <option value="Experimental">Experimental (from Parent Sample)</option>
            </select>
        </div>
      </div>

      <div class="input-group">
        <label for="sample-type">Sample Type</label>
        <div class="select-wrapper">
            <select id="sample-type" required>
              <option value="Unknown">Unknown</option>
              <option value="Liquid Cell Culture">Liquid Cell Culture</option>
              <option value="Solid Cell Culture">Solid Cell Culture</option>
            </select>
        </div>
      </div>

      <div class="input-group" id="group-strain">
        <label for="strain-container">Strain</label>
        <div id="strain-container"></div>
      </div>

      <div class="input-group" id="group-community" style="display: none;">
        <label for="community-container">Community</label>
        <div id="community-container"></div>
      </div>

      <div class="input-group" id="group-parent" style="display: none;">
        <label for="parent-container">Source Sample (Parent)</label>
        <div id="parent-container"></div>
      </div>

      <div class="input-group">
        <label for="num-samples">Number of Samples to Create</label>
        <input type="number" id="num-samples" value="1" min="1" max="100" required />
      </div>

      <div class="form-actions">
        <button type="submit" id="save-button" class="button">Create Samples</button>
      </div>
    </form>
  `;

  // --- DOM Elements ---
  const projectSelect = document.getElementById('project-select');
  const categorySelect = document.getElementById('sample-category');
  const typeSelect = document.getElementById('sample-type');
  const numSamplesInput = document.getElementById('num-samples');
  const form = document.getElementById('create-sample-form');

    // Dynamic Groups
  const groupStrain = document.getElementById('group-strain');
  const groupCommunity = document.getElementById('group-community');
  const groupParent = document.getElementById('group-parent');

  // Inputs & Datalists
  const strainContainer = document.getElementById('strain-container');
  const communityContainer = document.getElementById('community-container');
  const parentContainer = document.getElementById('parent-container');

  let strainAutocomplete = null;
  let communityAutocomplete = null;
  let parentAutocomplete = null;

  // --- 1. Fetchers ---

  async function loadProjects() {
    try {
      const response = await fetch('/api/v1/projects');
      if (!response.ok) throw new Error('Failed to fetch projects');
      const result = await response.json();

      projectSelect.innerHTML = '<option value="">Select a Project...</option>';
      result.data.forEach(p => {
        projectSelect.innerHTML += `<option value="${p.id}">${UIUtils.escapeHTML(p.name)}</option>`;
      });
    } catch (e) {
      console.error(e);
      projectSelect.innerHTML = '<option value="">Error loading projects</option>';
    }
  }

  function initAutocompletes() {
    strainAutocomplete = new AutocompleteComponent(strainContainer, {
      placeholder: 'Type to search strains...',
      fetchItems: async (query) => {
        const res = await fetch(`/api/v1/strains?search=${encodeURIComponent(query)}`);
        const json = await res.json();
        return (json.data || []).map(s => ({
          id: s.id,
          label: `[${s.strain_name}] ${s.genus} ${s.species || 'Unknown'}`
        }));
      }
    });

    communityAutocomplete = new AutocompleteComponent(communityContainer, {
      placeholder: 'Type to search communities...',
      fetchItems: async (query) => {
        const res = await fetch(`/api/v1/communities?search=${encodeURIComponent(query)}`);
        const json = await res.json();
        return (json.data || []).map(c => ({
          id: c.id,
          label: c.name
        }));
      }
    });

    // Parent sample autocomplete must be restricted to the selected project.
    parentAutocomplete = new AutocompleteComponent(parentContainer, {
      placeholder: 'Select a project first...',
      fetchItems: async (query) => {
        const projectId = projectSelect.value;
        if (!projectId) return [];
        const res = await fetch(`/api/v1/projects/${projectId}/samples?search=${encodeURIComponent(query)}&category=Source`);
        const json = await res.json();
        return (json.data || []).map(s => ({
          id: s.id,
          label: `${s.short_id || s.id} - ${s.sample_type}`
        }));
      }
    });
  }

  function updateParentAutocomplete() {
    const projectId = projectSelect.value;
    if (parentAutocomplete && parentAutocomplete.inputElement) {
        parentAutocomplete.inputElement.placeholder = projectId ? 'Type to search source samples...' : 'Select a project first...';
        parentAutocomplete.selectItem(null);
        parentAutocomplete.inputElement.value = '';
    }
  }

  // --- 2. Event Listeners ---

  function updateCategoryUI() {
    const category = categorySelect.value;
    if (category === 'Source') {
      groupStrain.style.display = 'flex';
      groupCommunity.style.display = 'flex';
      groupParent.style.display = 'none';
    } else {
      groupStrain.style.display = 'none';
      groupCommunity.style.display = 'none';
      groupParent.style.display = 'flex';
    }
  }

  categorySelect.addEventListener('change', updateCategoryUI);

  projectSelect.addEventListener('change', updateParentAutocomplete);

  // --- 3. Initialization ---
  loadProjects();
  initAutocompletes();
  updateCategoryUI();

  // --- 4. Submit Logic ---
  form.addEventListener('submit', async (event) => {
    event.preventDefault();

    const projectId = projectSelect.value;
    const category = categorySelect.value;
    const sampleType = typeSelect.value;
    const numSamples = parseInt(numSamplesInput.value, 10);

    if (!projectId) { UIUtils.showToast("Please select a project.", "error"); return; }

    let strainId = null;
    let communityId = null;
    let parentSampleId = null;

    if (category === 'Source') {
      strainId = strainAutocomplete.getValue();
      communityId = communityAutocomplete.getValue();
      if (!strainId && !communityId) {
        UIUtils.showToast("Please select a valid Strain or Community.", "error"); return;
      }
    } else {
      parentSampleId = parentAutocomplete.getValue();
      if (!parentSampleId) { UIUtils.showToast("Please select a valid Source Sample from the list.", "error"); return; }
    }

    const samplesList = [];
    for (let i = 0; i < numSamples; i++) {
      const s = {
        sample_type: sampleType,
        category: category,
        result_definition_ids: []
      };

      if (strainId) s.strain_id = parseInt(strainId, 10);
      if (communityId) s.community_id = parseInt(communityId, 10);
      if (parentSampleId) s.parent_sample_id = parseInt(parentSampleId, 10);
      samplesList.push(s);
    }

    const samplesPayload = { samples: samplesList };

    try {
      const response = await fetch(`/api/v1/projects/${projectId}/samples/bulk`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(samplesPayload),
      });

      if (response.ok) {
        history.pushState({}, "", `/projects/${projectId}`);
        handleRouting();
      } else {
        let errorMessage = 'Failed to create samples';
        try {
          const errorData = await response.json();
          errorMessage = errorData.message || errorMessage;
        } catch (err) {
          console.warn("Could not parse error response JSON", err);
        }
        UIUtils.showToast(`Error: ${errorMessage}`, "error");
      }
    } catch (error) {
      console.error('Error submitting form:', error);
      UIUtils.showToast('An unexpected network error occurred.', "error");
    }
  });
}
