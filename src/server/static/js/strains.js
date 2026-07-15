/* src/server/static/js/strains.js */

async function loadStrainsPage(mainElement) {
  mainElement.innerHTML = `
    <div class="page-header">
      <div style="flex-grow: 1;">
        <h2>All Strains</h2>
      </div>
      <div style="display: flex; gap: 10px;">
        <a href="/strains/new" class="button">Create Strain</a>
      </div>
    </div>

    <div id="collapsible-bar" class="collapsible-bar">
      <span class="collapsible-chevron">▸</span> <span>Show actions</span>
    </div>
    <div id="collapsible-actions" class="collapsible-content">
      <a href="/api/v1/strains?format=csv" class="button secondary-button">Export All CSV</a>
      <button onclick="UploadService.openWizard('strains')" class="button secondary-button">Bulk Import Strains</button>
    </div>

    <div class="form-container" style="max-width: 800px; margin: 2rem auto; background: var(--bg-light); border: 1px solid var(--border-color); border-radius: 8px; padding: 2rem; box-shadow: 0 4px 6px rgba(0,0,0,0.05);">
        <h3 style="margin-top: 0; margin-bottom: 0.5rem; color: var(--primary-color);">Find Strains</h3>
        <p class="text-secondary" style="margin-bottom: 1.5rem;">Search across all strains. Results are limited to the top 50 matches for performance.</p>
        
        <form id="strain-search-form" style="display: flex; flex-direction: column; gap: 15px;">
            <div style="display: flex; gap: 15px; flex-wrap: wrap;">
                <div class="input-group" style="flex: 1; min-width: 200px;">
                    <label style="font-size: 0.85rem; color: var(--text-secondary); margin-bottom: 4px; display: block;">Genus</label>
                    <div class="select-wrapper">
                        <select id="search-genus-filter" style="width: 100%;">
                            <option value="">All Genera</option>
                        </select>
                    </div>
                </div>
                <div class="input-group" style="flex: 1; min-width: 200px;">
                    <label style="font-size: 0.85rem; color: var(--text-secondary); margin-bottom: 4px; display: block;">Species</label>
                    <div class="select-wrapper">
                        <select id="search-species-filter" style="width: 100%;">
                            <option value="">All Species</option>
                        </select>
                    </div>
                </div>
            </div>
            
            <div style="display: flex; gap: 10px; margin-top: 10px;">
                <input type="text" id="search-input" placeholder="Search by Strain Name, Genotype, or Notes..." style="flex: 1; font-size: 1.1rem; padding: 12px 15px;" autocomplete="off">
                <button type="submit" id="search-btn" class="button" style="padding: 0 30px; font-size: 1.1rem;">Search</button>
            </div>
        </form>
    </div>

    <div id="search-results-container" style="display:none;">
        <table id="strains-table" class="display" style="width:100%;">
            <thead>
                <tr>
                    <th>Genus</th>
                    <th>Species</th>
                    <th>Strain Name</th>
                    <th>Genotype</th>
                    <th class="col-parent">Parent Strain</th>
                    <th class="col-notes">Notes</th>
                    <th class="col-created">Created At</th>
                    <th class="col-updated">Updated At</th>
                </tr>
            </thead>
            <tbody></tbody>
        </table>
    </div>
  `;

  const tableElement = $('#strains-table');
  const searchForm = document.getElementById('strain-search-form');
  const searchInput = document.getElementById('search-input');
  const genusFilter = document.getElementById('search-genus-filter');
  const speciesFilter = document.getElementById('search-species-filter');
  const searchResultsContainer = document.getElementById('search-results-container');

  // Collapsible
  const collapsibleBar = document.getElementById('collapsible-bar');
  const collapsibleContent = document.getElementById('collapsible-actions');
  const collapsibleChevron = document.querySelector('.collapsible-chevron');
  function toggleCollapsible() {
    const isOpen = collapsibleContent.classList.toggle('open');
    collapsibleChevron.textContent = isOpen ? '▾' : '▸';
    collapsibleBar.querySelector('span:last-child').textContent = isOpen ? 'Hide actions' : 'Show actions';
  }
  collapsibleBar.onclick = toggleCollapsible;

  let parentMap = {};
  let dataTable = null;

  try {
    // We only need to fetch strains once initially to populate the unique dropdowns
    // (Assuming <5000 strains so a full fetch isn't terrible just for dropdowns, 
    // but ideally we'd have a specific /api/v1/strains/genera endpoint)
    const response = await fetch('/api/v1/strains');
    if (response.ok) {
        const json = await response.json();
        const strains = json.data || [];
        
        strains.forEach(s => parentMap[s.id] = s.strain_name);
        
        const uniqueGenera = [...new Set(strains.map(s => s.genus))].filter(Boolean).sort();
        const uniqueSpecies = [...new Set(strains.map(s => s.species))].filter(Boolean).sort();
        
        uniqueGenera.forEach(g => {
            const opt = document.createElement('option');
            opt.value = g;
            opt.textContent = g;
            genusFilter.appendChild(opt);
        });
        
        uniqueSpecies.forEach(s => {
            const opt = document.createElement('option');
            opt.value = s;
            opt.textContent = s;
            speciesFilter.appendChild(opt);
        });
    }
  } catch (err) {
      console.error("Failed to load reference data", err);
  }

  searchForm.addEventListener('submit', async (e) => {
    e.preventDefault();
    const query = searchInput.value.trim();
    
    let url = `/api/v1/strains?search=${encodeURIComponent(query)}`;
    if (genusFilter.value) url += `&genus=${encodeURIComponent(genusFilter.value)}`;
    if (speciesFilter.value) url += `&species=${encodeURIComponent(speciesFilter.value)}`;
    
    const searchBtn = document.getElementById('search-btn');
    const originalText = searchBtn.textContent;
    searchBtn.textContent = 'Searching...';
    searchBtn.disabled = true;

    try {
        const res = await fetch(url);
        if (!res.ok) throw new Error("Search request failed");
        const json = await res.json();
        const strains = json.data || [];

        // Ensure parent map is updated just in case new strains appeared
        strains.forEach(s => parentMap[s.id] = s.strain_name);

        if (!dataTable) {
            dataTable = tableElement.DataTable({
                order: [[6, "desc"]], // Sort by created_at by default
                pageLength: 25,
                lengthChange: false,
                data: [], // start empty
                dom: '<"top"lBf>rt<"bottom"ip><"clear">',
                buttons: [
                  {
                      extend: 'colvis',
                      text: '⚙️ Columns',
                      collectionLayout: 'fixed columns',
                      popoverTitle: 'Toggle Columns Visibility'
                  }
                ],
                columnDefs: [
                  { targets: 'col-parent', visible: false },
                  { targets: 'col-notes', visible: false },
                  { targets: 'col-created', visible: false },
                  { targets: 'col-updated', visible: false }
                ],
                columns: [
                    { data: 'genus' },
                    { data: 'species' },
                    { data: 'strain_name' },
                    { data: 'genotype' },
                    { data: 'parent' },
                    { data: 'notes' },
                    { data: 'created', type: 'num', render: { _: 'display', sort: 'raw_created' } },
                    { data: 'updated', type: 'num', render: { _: 'display', sort: 'raw_updated' } }
                ]
            });
        }

        const tableData = strains.map(strain => {
            const createdDate = new Date(strain.created_at * 1000).toLocaleDateString();
            const updatedTime = strain.updated_at ? strain.updated_at : strain.created_at;
            const updatedDate = new Date(updatedTime * 1000).toLocaleDateString();
            
            let parentLink = '<span class="text-secondary">—</span>';
            if (strain.parent_strain_id) {
                const pName = UIUtils.escapeHTML(parentMap[strain.parent_strain_id] || `ID ${strain.parent_strain_id}`);
                parentLink = `<a href="/strains/${strain.parent_strain_id}" class="badge"><span class="emoji">🧬</span> ${pName}</a>`;
            }
            
            const genotypeHtml = strain.genotype ? UIUtils.escapeHTML(strain.genotype) : '<span class="text-secondary">—</span>';
            const notesHtml = strain.notes ? `<div style="white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 250px;" title="${UIUtils.escapeHTML(strain.notes)}">${UIUtils.escapeHTML(strain.notes)}</div>` : '<span class="text-secondary">—</span>';

            return {
                genus: `<em>${UIUtils.escapeHTML(strain.genus)}</em>`,
                species: `<em>${UIUtils.escapeHTML(strain.species)}</em>`,
                strain_name: `<a href="/strains/${strain.id}" class="id-link">${UIUtils.escapeHTML(strain.strain_name)}</a>`,
                genotype: genotypeHtml,
                parent: parentLink,
                notes: notesHtml,
                created: { display: createdDate, raw_created: strain.created_at },
                updated: { display: updatedDate, raw_updated: updatedTime }
            };
        });

        dataTable.clear().rows.add(tableData).draw();
        searchResultsContainer.style.display = 'block';

        if (strains.length === 0) {
            UIUtils.showToast("No strains found matching your criteria.", "info");
        }

    } catch (err) {
        console.error(err);
        UIUtils.showToast("Failed to perform search.", "error");
    } finally {
        searchBtn.textContent = originalText;
        searchBtn.disabled = false;
    }
  });
}
