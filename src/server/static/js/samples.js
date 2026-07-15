/* src/server/static/js/samples.js */

async function loadSamplesPage(mainElement) {
  mainElement.innerHTML = `
    <div class="page-header">
      <div style="flex-grow: 1;">
        <h2>All Samples</h2>
      </div>
      <div style="display: flex; gap: 10px;">
        <a href="/samples/new" class="button">Create Sample</a>
      </div>
    </div>

    <div id="collapsible-bar" class="collapsible-bar">
      <span class="collapsible-chevron">▸</span> <span>Show actions</span>
    </div>
    <div id="collapsible-actions" class="collapsible-content">
      <a href="/api/v1/samples?format=csv" class="button secondary-button">Export All CSV</a>
      <button onclick="UploadService.openWizard('results')" class="button secondary-button">Bulk Import Results</button>
      <button onclick="UploadService.openWizard('bulk-samples')" class="button secondary-button">Bulk Import Samples</button>
    </div>

    <div class="form-container" style="max-width: 800px; margin: 2rem auto; background: var(--bg-light); border: 1px solid var(--border-color); border-radius: 8px; padding: 2rem; box-shadow: 0 4px 6px rgba(0,0,0,0.05);">
        <h3 style="margin-top: 0; margin-bottom: 0.5rem; color: var(--primary-color);">Find Samples</h3>
        <p class="text-secondary" style="margin-bottom: 1.5rem;">Search across all projects. Results are limited to the top 50 matches for performance.</p>
        
        <form id="sample-search-form" style="display: flex; flex-direction: column; gap: 15px;">
            <div style="display: flex; gap: 15px; flex-wrap: wrap;">
                <div class="input-group" style="flex: 1; min-width: 200px;">
                    <label style="font-size: 0.85rem; color: var(--text-secondary); margin-bottom: 4px; display: block;">Project</label>
                    <div class="select-wrapper">
                        <select id="search-project-filter" style="width: 100%;">
                            <option value="">All Projects</option>
                        </select>
                    </div>
                </div>
                <div class="input-group" style="flex: 1; min-width: 200px;">
                    <label style="font-size: 0.85rem; color: var(--text-secondary); margin-bottom: 4px; display: block;">Category</label>
                    <div class="select-wrapper">
                        <select id="search-category-filter" style="width: 100%;">
                            <option value="">All Categories</option>
                            <option value="Source">Source</option>
                            <option value="Experimental">Experimental</option>
                        </select>
                    </div>
                </div>
                <div class="input-group" style="flex: 1; min-width: 200px;">
                    <label style="font-size: 0.85rem; color: var(--text-secondary); margin-bottom: 4px; display: block;">Sample Type</label>
                    <div class="select-wrapper">
                        <select id="search-type-filter" style="width: 100%;">
                            <option value="">All Sample Types</option>
                            <option value="Unknown">Unknown</option>
                            <option value="Liquid Cell Culture">Liquid Cell Culture</option>
                            <option value="Solid Cell Culture">Solid Cell Culture</option>
                        </select>
                    </div>
                </div>
                <div class="input-group" style="flex: 1; min-width: 200px;">
                    <label style="font-size: 0.85rem; color: var(--text-secondary); margin-bottom: 4px; display: block;">Strain</label>
                    <div id="search-strain-container"></div>
                </div>
            </div>
            
            <div style="display: flex; gap: 10px; margin-top: 10px;">
                <input type="text" id="search-input" placeholder="Search by Short ID or UID (optional)..." style="flex: 1; font-size: 1.1rem; padding: 12px 15px;" autocomplete="off">
                <button type="submit" id="search-btn" class="button" style="padding: 0 30px; font-size: 1.1rem;">Search</button>
            </div>
        </form>
    </div>

    <div id="search-results-container" style="display:none;">
        <table id="search-results-table" class="display" style="width:100%;">
            <thead>
                <tr>
                    <th>Short ID</th>
                    <th>Project</th>
                    <th>Biological Source</th>
                    <th>Type</th>
                    <th>Status</th>
                    <th>Created</th>
                </tr>
            </thead>
            <tbody></tbody>
        </table>
    </div>
  `;

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

  const searchForm = document.getElementById('sample-search-form');
  const searchInput = document.getElementById('search-input');
  const projectFilter = document.getElementById('search-project-filter');
  const categoryFilter = document.getElementById('search-category-filter');
  const typeFilter = document.getElementById('search-type-filter');
  const searchResultsContainer = document.getElementById('search-results-container');
  const tableElement = $('#search-results-table');
  const strainContainer = document.getElementById('search-strain-container');
  
  let projectMap = {};
  let strainMap = {};
  let communityMap = {};
  let dataTable = null;
  let strainFilterAutocomplete = null;

  // Load Reference Data
  try {
    const [projectsRes, strainsRes, communitiesRes] = await Promise.all([
      fetch('/api/v1/projects'),
      fetch('/api/v1/strains'),
      fetch('/api/v1/communities')
    ]);

    const projectsData = await projectsRes.json();
    const strainsData = await strainsRes.json();
    const communitiesData = await communitiesRes.json();

    projectMap = Object.fromEntries((projectsData.data || []).map(p => [p.id, p.name]));
    strainMap = Object.fromEntries((strainsData.data || []).map(s => [s.id, s.strain_name]));
    communityMap = Object.fromEntries((communitiesData.data || []).map(c => [c.id, c.name]));

    (projectsData.data || []).forEach(p => {
        const opt = document.createElement('option');
        opt.value = p.id;
        opt.textContent = p.name;
        projectFilter.appendChild(opt);
    });

    strainFilterAutocomplete = new AutocompleteComponent(strainContainer, {
      placeholder: 'Filter by Strain...',
      fetchItems: async (query) => {
        const res = await fetch(`/api/v1/strains?search=${encodeURIComponent(query)}`);
        const json = await res.json();
        return (json.data || []).map(s => ({
          id: s.id,
          label: `[${s.strain_name}] ${s.genus} ${s.species || 'Unknown'}`
        }));
      },
      onSelect: (item) => {
          // You could automatically trigger search here, or wait for submit
      }
    });

  } catch (err) {
    console.error("Failed to load reference data for search", err);
    UIUtils.showToast("Failed to load some reference data.", "warning");
  }

  searchForm.addEventListener('submit', async (e) => {
    e.preventDefault();
    const query = searchInput.value.trim();
    
    let url = `/api/v1/samples?search=${encodeURIComponent(query)}`;
    if (projectFilter.value) url += `&project_id=${encodeURIComponent(projectFilter.value)}`;
    if (categoryFilter.value) url += `&category=${encodeURIComponent(categoryFilter.value)}`;
    if (typeFilter.value) url += `&sample_type=${encodeURIComponent(typeFilter.value)}`;
    if (strainFilterAutocomplete && strainFilterAutocomplete.getValue()) {
      url += `&strain_id=${encodeURIComponent(strainFilterAutocomplete.getValue())}`;
    }
    
    const searchBtn = document.getElementById('search-btn');
    const originalText = searchBtn.textContent;
    searchBtn.textContent = 'Searching...';
    searchBtn.disabled = true;

    try {
        const res = await fetch(url);
        if (!res.ok) throw new Error("Search request failed");
        const json = await res.json();
        const samples = json.data || [];

        if (!dataTable) {
            dataTable = tableElement.DataTable({
                order: [[5, "desc"]],
                pageLength: 25,
                lengthChange: false,
                data: [], // start empty
                columns: [
                    { data: 'short_id' },
                    { data: 'project' },
                    { data: 'source' },
                    { data: 'type' },
                    { data: 'status' },
                    { data: 'created', type: 'num', render: { _: 'display', sort: 'raw_created' } }
                ]
            });
        }

        const tableData = samples.map(sample => {
            const projectName = UIUtils.escapeHTML(projectMap[sample.project_id] || `ID: ${sample.project_id}`);
            const createdDate = new Date(sample.created_at * 1000).toLocaleDateString();

            let sourceInfo = '<span class="text-secondary">—</span>';
            if (sample.category === 'Source' && sample.strain_id) {
                const sName = UIUtils.escapeHTML(strainMap[sample.strain_id] || 'Unknown');
                sourceInfo = `<a href="/strains/${sample.strain_id}" class="badge badge-strain"><span class="emoji">🧬</span> ${sName}</a>`;
            } else if (sample.category === 'Source' && sample.community_id) {
                const cName = UIUtils.escapeHTML(communityMap[sample.community_id] || 'Unknown');
                sourceInfo = `<a href="/communities/${sample.community_id}" class="badge"><span class="emoji">🦠</span> ${cName}</a>`;
            } else if (sample.category === 'Experimental' && sample.parent_sample_id) {
                // We don't fetch all samples, so parent name isn't readily available without an extra fetch. Just show ID.
                sourceInfo = `<a href="/samples/${sample.parent_sample_id}" class="badge badge-parent"><span class="emoji">🧪</span> Sample ${sample.parent_sample_id}</a>`;
            }

            return {
                short_id: `<a href="/samples/${sample.id}" class="id-link">${UIUtils.escapeHTML(sample.short_id || sample.id)}</a>`,
                project: projectName,
                source: sourceInfo,
                type: UIUtils.escapeHTML(sample.sample_type),
                status: `<span class="status-pill status-${UIUtils.escapeHTML(sample.status.toLowerCase())}">${UIUtils.escapeHTML(sample.status)}</span>`,
                created: {
                    display: createdDate,
                    raw_created: sample.created_at
                }
            };
        });

        dataTable.clear().rows.add(tableData).draw();
        searchResultsContainer.style.display = 'block';

        if (samples.length === 0) {
            UIUtils.showToast("No samples found matching your criteria.", "info");
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
