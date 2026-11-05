/* src/server/static/js/result-definitions.js */

async function loadResultDefinitionsPage(mainElement) {
  mainElement.innerHTML = `
    <div class="page-header">
      <div style="flex-grow: 1; display: flex; align-items: center; gap: 15px;">
        <h2>Data Definitions</h2>
      </div>
      <div style="display: flex; gap: 10px;">
        <button id="export-csv-btn" class="button secondary-button">Export CSV</button>
        <a href="/result-categories" class="button secondary-button">Manage Data Categories</a>
        <button onclick="history.pushState({}, '', '/result-definitions/create'); handleRouting();" class="button">Create Data Definition</button>
      </div>
    </div>

    <div id="loading-indicator">Loading definitions...</div>
    
    <div id="filter-and-bulk-bar" style="display:none; margin-bottom: 1.25rem; flex-wrap: wrap; justify-content: space-between; align-items: center; gap: 15px;">
      <div style="display: flex; align-items: center; gap: 10px;">
        <label for="category-filter-select" style="font-weight: bold; color: var(--text-primary); white-space: nowrap;">Filter by Category:</label>
        <div class="select-wrapper" style="width: auto; min-width: 240px;">
          <select id="category-filter-select">
            <option value="__ALL__">All Categories</option>
          </select>
        </div>
      </div>

      <div id="bulk-action-bar" style="display:none; padding: 0.6rem 1rem; background: var(--bg-dark-2); border-radius: 4px; border: 1px solid var(--border-color); align-items: center; gap: 10px;">
        <span style="font-weight: bold; color: var(--text-primary);">Bulk Move Selected To:</span>
        <div class="select-wrapper" style="width: auto; min-width: 200px;">
          <select id="bulk-category-select">
            <option value="">(No Category)</option>
          </select>
        </div>
        <button id="bulk-move-btn" class="button">Move</button>
      </div>
    </div>

    <div id="tables-container"></div>
  `;

  const loadingIndicator = document.getElementById('loading-indicator');
  const filterAndBulkBar = document.getElementById('filter-and-bulk-bar');
  const categoryFilterSelect = document.getElementById('category-filter-select');
  const tablesContainer = document.getElementById('tables-container');
  const bulkActionBar = document.getElementById('bulk-action-bar');
  const bulkCategorySelect = document.getElementById('bulk-category-select');
  const bulkMoveBtn = document.getElementById('bulk-move-btn');
  const exportCsvBtn = document.getElementById('export-csv-btn');

  let dt = null;

  function getCheckedBoxes() {
    if (!dt) return [];
    return Array.from($('input.row-cb:checked', dt.rows({ search: 'applied' }).nodes()));
  }

  function updateBulkActionBar() {
    const anyChecked = getCheckedBoxes().length > 0;
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

    // Group definitions by category to compute counts
    const categoriesMap = new Map();
    categoriesMap.set('No Category', []); // Always ensure No Category exists

    categoriesList.forEach(c => {
        categoriesMap.set(c.name, []);
    });

    resultDefinitions.forEach(rd => {
      const catName = rd.category ? rd.category.name : 'No Category';
      if (!categoriesMap.has(catName)) {
        categoriesMap.set(catName, []);
      }
      categoriesMap.get(catName).push(rd);
    });

    const uniqueCategories = Array.from(categoriesMap.keys()).sort((a, b) => {
      if (a === 'No Category') return -1;
      if (b === 'No Category') return 1;
      return a.localeCompare(b);
    });

    // Populate category filter dropdown with counts
    categoryFilterSelect.innerHTML = `<option value="__ALL__">All Categories (${resultDefinitions.length})</option>`;
    uniqueCategories.forEach(catName => {
      const count = categoriesMap.get(catName).length;
      const opt = document.createElement('option');
      opt.value = catName;
      opt.textContent = `${catName} (${count})`;
      categoryFilterSelect.appendChild(opt);
    });

    const tableHtml = `
      <table id="result-definitions-table" class="display" style="width:100%;">
          <thead>
              <tr>
                  <th style="width:40px; text-align:center;"><input type="checkbox" class="select-all-cb"></th>
                  <th>Short ID</th>
                  <th>Name</th>
                  <th>Category</th>
                  <th>Data Type</th>
                  <th>Units</th>
                  <th>Created At</th>
              </tr>
          </thead>
          <tbody>
              ${resultDefinitions.map(rd => {
                const catName = rd.category ? rd.category.name : 'No Category';
                const createdDate = new Date(rd.created_at * 1000).toLocaleDateString();
                const linkTarget = rd.short_id || rd.id;
                return `
                  <tr>
                      <td style="text-align:center;"><input type="checkbox" class="row-cb" value="${rd.id}"></td>
                      <td><a href="/result-definitions/${linkTarget}" class="id-link">${rd.short_id || 'N/A'}</a></td>
                      <td><a href="/result-definitions/${linkTarget}" class="id-link">${rd.name}</a></td>
                      <td>${catName}</td>
                      <td>${rd.data_type}</td>
                      <td>${rd.unit || 'N/A'}</td>
                      <td data-sort="${rd.created_at}">${createdDate}</td>
                  </tr>`;
              }).join('')}
          </tbody>
      </table>
    `;
    tablesContainer.innerHTML = tableHtml;

    dt = $('#result-definitions-table').DataTable({
      order: [[2, "asc"]], // Order alphabetically by Name
      pageLength: 25,
      autoWidth: false,
      columnDefs: [
          { orderable: false, targets: 0, width: "40px" }
      ]
    });

    // Handle Category Filter Dropdown change
    categoryFilterSelect.addEventListener('change', () => {
      const selectedCat = categoryFilterSelect.value;
      // Uncheck any previously selected rows when switching category filter
      $('input.row-cb', dt.rows().nodes()).prop('checked', false);
      $('#result-definitions-table .select-all-cb').prop('checked', false);

      if (selectedCat === '__ALL__') {
        dt.column(3).search('').draw();
      } else {
        const escaped = $.fn.dataTable.util.escapeRegex(selectedCat);
        dt.column(3).search(`^${escaped}$`, true, false).draw();
      }
      updateBulkActionBar();
    });

    // Handle Select All (applies to currently filtered rows)
    $('#result-definitions-table').on('change', '.select-all-cb', function() {
      const isChecked = this.checked;
      $('input.row-cb', dt.rows({ search: 'applied' }).nodes()).prop('checked', isChecked);
      updateBulkActionBar();
    });

    // Handle individual row select
    $('#result-definitions-table').on('change', '.row-cb', function() {
      updateBulkActionBar();
    });

    // Handle Export CSV of current filtered view
    exportCsvBtn.addEventListener('click', () => {
      if (!dt) return;
      const visibleNodes = Array.from(dt.rows({ search: 'applied' }).nodes());
      const visibleIds = visibleNodes.map(tr => {
        const cb = tr.querySelector('.row-cb');
        return cb ? parseInt(cb.value, 10) : null;
      }).filter(id => id !== null);

      const escapeCsv = (val) => {
        const s = val === null || val === undefined ? '' : String(val);
        if (s.includes(',') || s.includes('"') || s.includes('\n')) {
          return `"${s.replace(/"/g, '""')}"`;
        }
        return s;
      };

      const rows = ['ID,Short ID,Name,Category,Required,Data Type,Unit,Created At'];
      visibleIds.forEach(id => {
        const rd = resultDefinitions.find(r => r.id === id);
        if (!rd) return;
        const catName = rd.category ? rd.category.name : 'No Category';
        const createdIso = ApiUtils.formatIsoDate(rd.created_at, 'Datetime');
        rows.push([
          rd.id,
          escapeCsv(rd.short_id || ''),
          escapeCsv(rd.name),
          escapeCsv(catName),
          rd.is_required ? 'True' : 'False',
          escapeCsv(rd.data_type),
          escapeCsv(rd.unit || ''),
          escapeCsv(createdIso)
        ].join(','));
      });

      const selectedCat = categoryFilterSelect.value;
      const slug = selectedCat === '__ALL__'
        ? 'all'
        : selectedCat.toLowerCase().replace(/[^a-z0-9]+/g, '_').replace(/^_|_$/g, '');
      const filename = `result_definitions_${slug || 'filtered'}.csv`;

      const blob = new Blob([rows.join('\n') + '\n'], { type: 'text/csv;charset=utf-8;' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = filename;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    });

    // Handle Bulk Move
    bulkMoveBtn.addEventListener('click', async () => {
      const selectedBoxes = getCheckedBoxes();
      const selectedIds = selectedBoxes.map(cb => parseInt(cb.value, 10));
      
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
          short_id: rd.short_id || String(rd.id),
          name: rd.name,
          description: rd.description || null,
          data_type: rd.data_type,
          unit: rd.unit || null,
          category_id: newCatId,
          is_required: Boolean(rd.is_required)
        };
        Object.keys(payload).forEach(k => (payload[k] === null || payload[k] === undefined) && delete payload[k]);
        
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
    filterAndBulkBar.style.display = 'flex';

  } catch (error) {
    console.error(error);
    loadingIndicator.style.display = 'none';
    mainElement.innerHTML = `<p class="error">Failed to load result definitions: ${error.message}</p>`;
  }
}
