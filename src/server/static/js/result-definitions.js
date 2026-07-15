/* src/server/static/js/result-definitions.js */

async function loadResultDefinitionsPage(mainElement) {
  mainElement.innerHTML = `
    <div class="page-header">
      <div style="flex-grow: 1; display: flex; align-items: center; gap: 15px;">
        <h2>Result Definitions</h2>
      </div>
      <div style="display: flex; gap: 10px;">
        <a href="/api/v1/result-definitions?format=csv" class="button secondary-button">Export All CSV</a>
        <a href="/result-categories" class="button secondary-button">Manage Categories</a>
        <button onclick="history.pushState({}, '', '/result-definitions/create'); handleRouting();" class="button">Create Definition</button>
      </div>
    </div>

    <div id="loading-indicator">Loading definitions...</div>
    
    <div id="bulk-action-bar" style="display:none; margin-bottom: 1rem; padding: 1rem; background: var(--bg-dark-2); border-radius: 4px; border: 1px solid var(--border-color); align-items: center; gap: 10px;">
      <span style="font-weight: bold; color: var(--text-primary);">Bulk Move Selected To:</span>
      <div class="select-wrapper" style="width: auto; min-width: 200px;">
        <select id="bulk-category-select">
          <option value="">(No Category)</option>
        </select>
      </div>
      <button id="bulk-move-btn" class="button">Move</button>
    </div>

    <div id="tabs-container" class="tab-bar" style="display:none; margin-top: 0; margin-bottom: 1.5rem;"></div>
    <div id="tables-container"></div>
  `;

  const loadingIndicator = document.getElementById('loading-indicator');
  const tabsContainer = document.getElementById('tabs-container');
  const tablesContainer = document.getElementById('tables-container');
  const bulkActionBar = document.getElementById('bulk-action-bar');
  const bulkCategorySelect = document.getElementById('bulk-category-select');
  const bulkMoveBtn = document.getElementById('bulk-move-btn');

  function updateBulkActionBar() {
    // Only check visible inputs to avoid acting on hidden tabs
    const anyChecked = document.querySelectorAll('.tab-content[style*="block"] .row-cb:checked').length > 0;
    bulkActionBar.style.display = anyChecked ? 'flex' : 'none';
  }

  try {
    // Fetch definitions and categories in parallel
    const [resResponse, catResponse] = await Promise.all([
      fetch('/api/v1/result-definitions'),
      fetch('/api/v1/result-categories')
    ]);

    if (!resResponse.ok) throw new Error(`Error fetching result definitions: ${resResponse.statusText}`);
    
    const resJson = await resResponse.json();
    const resultDefinitions = resJson.data;
    
    let categoriesList = [];
    if (catResponse.ok) {
        const catJson = await catResponse.json();
        categoriesList = catJson.data;
        categoriesList.forEach(c => {
            const opt = document.createElement('option');
            opt.value = c.id;
            opt.textContent = c.name;
            bulkCategorySelect.appendChild(opt);
        });
    }

    // Group definitions by category
    const categoriesMap = new Map();
    categoriesMap.set('No Category', []); // Always ensure No Category exists

    // Pre-populate with all known categories so empty ones still get tabs
    categoriesList.forEach(c => {
        categoriesMap.set(c.name, []);
    });

    resultDefinitions.forEach(rd => {
      const catName = rd.category ? rd.category.name : 'No Category';
      if (!categoriesMap.has(catName)) {
        categoriesMap.set(catName, []); // Fallback in case of a desync
      }
      categoriesMap.get(catName).push(rd);
    });

    const uniqueCategories = Array.from(categoriesMap.keys()).sort((a, b) => {
      // Always put "No Category" at the start of the tabs list
      if (a === 'No Category') return -1;
      if (b === 'No Category') return 1;
      return a.localeCompare(b);
    });

    uniqueCategories.forEach((catName, index) => {
      // Create tab button
      const tabBtn = document.createElement('button');
      tabBtn.className = `tab-btn ${index === 0 ? 'active' : ''}`;
      tabBtn.textContent = catName;
      tabBtn.onclick = () => {
          switchTab(index);
          updateBulkActionBar(); // re-check selection state on tab switch
      };
      tabsContainer.appendChild(tabBtn);

      // Create table container
      const tableWrapper = document.createElement('div');
      tableWrapper.className = 'tab-content';
      tableWrapper.id = `tab-content-${index}`;
      tableWrapper.style.display = index === 0 ? 'block' : 'none';

      const tableHtml = `
        <table id="result-definitions-table-${index}" class="display" style="width:100%;">
            <thead>
                <tr>
                    <th style="width:40px; text-align:center;"><input type="checkbox" class="select-all-cb"></th>
                    <th>Name</th>
                    <th>Data Type</th>
                    <th>Units</th>
                    <th>Created At</th>
                </tr>
            </thead>
            <tbody>
                ${categoriesMap.get(catName).map(rd => {
                  const createdDate = new Date(rd.created_at * 1000).toLocaleDateString();
                  return `
                    <tr>
                        <td style="text-align:center;"><input type="checkbox" class="row-cb" value="${rd.id}"></td>
                        <td><a href="/result-definitions/${rd.short_id}" class="id-link">${rd.name}</a></td>
                        <td>${rd.data_type}</td>
                        <td>${rd.unit || 'N/A'}</td>
                        <td data-sort="${rd.created_at}">${createdDate}</td>
                    </tr>`;
                }).join('')}
            </tbody>
        </table>
      `;
      tableWrapper.innerHTML = tableHtml;
      tablesContainer.appendChild(tableWrapper);

      // Initialize DataTable for this tab
      const dt = $(`#result-definitions-table-${index}`).DataTable({
        order: [[1, "asc"]], // Order alphabetically by Name
        pageLength: 25,
        autoWidth: false,
        columnDefs: [
            { orderable: false, targets: 0, width: "40px" } // Strictly enforce checkbox width
        ]
      });

      // Handle Select All
      $(`#result-definitions-table-${index}`).on('change', '.select-all-cb', function() {
        const isChecked = this.checked;
        $('input.row-cb', dt.rows({search: 'applied'}).nodes()).prop('checked', isChecked);
        updateBulkActionBar();
      });

      // Handle individual row select
      $(`#result-definitions-table-${index}`).on('change', '.row-cb', function() {
        updateBulkActionBar();
      });
    });

    function switchTab(activeIndex) {
      document.querySelectorAll('#tabs-container .tab-btn').forEach((btn, idx) => {
        btn.classList.toggle('active', idx === activeIndex);
      });
      document.querySelectorAll('#tables-container .tab-content').forEach((content, idx) => {
        content.style.display = idx === activeIndex ? 'block' : 'none';
      });
    }

    // Handle Bulk Move
    bulkMoveBtn.addEventListener('click', async () => {
      // Only grab selected from the currently active tab to avoid confusion
      const selectedBoxes = document.querySelectorAll('.tab-content[style*="block"] .row-cb:checked');
      const selectedIds = Array.from(selectedBoxes).map(cb => parseInt(cb.value, 10));
      
      if (selectedIds.length === 0) return;
      
      const newCatIdStr = bulkCategorySelect.value;
      const newCatId = newCatIdStr === "" ? null : parseInt(newCatIdStr, 10);
      
      if (!confirm(`Are you sure you want to move ${selectedIds.length} definition(s)?`)) return;
      
      bulkMoveBtn.textContent = 'Moving...';
      bulkMoveBtn.disabled = true;

      let errors = 0;

      for (const id of selectedIds) {
        const rd = resultDefinitions.find(r => r.id === id);
        if (!rd) continue;
        
        const payload = {
          name: rd.name,
          description: rd.description || null,
          data_type: rd.data_type,
          unit: rd.unit || null,
          category_id: newCatId,
          is_required: rd.is_required
        };
        
        try {
          const res = await fetch(`/api/v1/result-definitions/${id}`, {
            method: 'PUT',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(payload)
          });
          if (!res.ok) errors++;
        } catch (e) {
          errors++;
        }
      }
      
      if (errors > 0) {
          UIUtils.showToast(`Finished with ${errors} errors. Proceeding to reload.`, "error");
      }
      
      // Reload the page to reflect new groupings
      loadResultDefinitionsPage(mainElement);
    });

    loadingIndicator.style.display = 'none';
    tabsContainer.style.display = 'flex';

  } catch (error) {
    console.error(error);
    loadingIndicator.style.display = 'none';
    mainElement.innerHTML = `<p class="error">Failed to load result definitions: ${error.message}</p>`;
  }
}
