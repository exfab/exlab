/* src/server/static/js/create-strain.js */

async function loadCreateStrainPage(mainElement) {
  const backLinkHtml = generateBackArrow('/strains');

  mainElement.innerHTML = `
    <div class="page-header">
      ${backLinkHtml}
      <h2>Create Strain</h2>
    </div>
    <div class="form-container">
      
      <div class="input-group">
        <label for="genus">Genus <span style="color:red">*</span></label>
        <input type="text" id="genus" placeholder="e.g. Escherichia" />
      </div>

      <div class="input-group">
        <label for="species">Species <span style="color:red">*</span></label>
        <input type="text" id="species" placeholder="e.g. coli" />
      </div>

      <div class="input-group">
        <label for="strain-name">Strain Name <span style="color:red">*</span></label>
        <input type="text" id="strain-name" placeholder="e.g. MG1655 or ATCC 25922" />
      </div>

      <div class="input-group">
        <label for="genotype">Genotype</label>
        <input type="text" id="genotype" placeholder="e.g. F- lambda- ilvG- rfb-50" />
      </div>

      <div class="input-group">
        <label for="parent-strain-container">Parent Strain</label>
        <div id="parent-strain-container"></div>
      </div>

      <div class="input-group">
        <label for="notes">Notes</label>
        <textarea id="notes" rows="3" placeholder="Additional details..."></textarea>
      </div>

      <button id="save-button" class="button">Save Strain</button>
    </div>
  `;

  const genusInput = document.getElementById('genus');
  const speciesInput = document.getElementById('species');
  const strainInput = document.getElementById('strain-name');
  const genotypeInput = document.getElementById('genotype');
  const parentContainer = document.getElementById('parent-strain-container');
  const notesInput = document.getElementById('notes');
  const saveButton = document.getElementById('save-button');

  // 1. Populate Parent Strain Dropdown
  const parentAutocomplete = new AutocompleteComponent(parentContainer, {
      placeholder: 'Search for a parent strain...',
      fetchItems: async (query) => {
          const res = await fetch(`/api/v1/strains?search=${encodeURIComponent(query)}`);
          const json = await res.json();
          return json.data.map(other => ({
              id: other.id,
              label: `[${other.strain_name}] ${other.genus} ${other.species}`
          }));
      }
  });

  // 2. Handle Save
  saveButton.addEventListener('click', async () => {
    const genusName = genusInput.value.trim();
    const speciesName = speciesInput.value.trim();
    const strainName = strainInput.value.trim();
    const genotype = genotypeInput.value.trim();
    const parentId = parentAutocomplete.getValue();
    const notes = notesInput.value.trim();

    if (!genusName) return UIUtils.showToast('Please enter a Genus.', "error");
    if (!speciesName) return UIUtils.showToast('Please enter a Species.', "error");
    if (!strainName) return UIUtils.showToast('Please enter a Strain Name.', "error");

    const payload = {
      genus: genusName,
      species: speciesName,
      strain_name: strainName,
      genotype: genotype || null,
      parent_strain_id: parentId ? parseInt(parentId) : null,
      notes: notes || null
    };

    Object.keys(payload).forEach(k => payload[k] === null && delete payload[k]);

    try {
      const res = await fetch('/api/v1/strains', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });

      if (res.ok) {
        history.pushState({}, "", "/strains");
        handleRouting();
      } else {
        const err = await res.json();
        UIUtils.showToast(`Error: ${err.error || 'Failed to create strain'}`, "error");
      }
    } catch (error) {
      console.error('Error saving strain:', error);
      UIUtils.showToast('Network error occurred.', "error");
    }
  });

  // Attach special character helper to the genotype input
  if (window.attachSpecialCharHelper) {
    window.attachSpecialCharHelper('genotype');
    window.attachSpecialCharHelper('notes');
  }
}
