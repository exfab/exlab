/* src/server/static/js/plates.js */

async function loadPlatesPage(mainElement) {
  mainElement.innerHTML = `
    <div class="page-header">
      <div style="flex-grow: 1;">
        <h2>Plates</h2>
      </div>
      <div style="display: flex; gap: 10px;">
        <button id="create-plate-btn" class="button">Create Plate</button>
      </div>
    </div>

    <div id="collapsible-bar" class="collapsible-bar">
      <span class="collapsible-chevron">▸</span> <span>Show actions</span>
    </div>
    <div id="collapsible-actions" class="collapsible-content">
      <button onclick="UploadService.openWizard('bulk-plates')" class="button secondary-button">Bulk Plate Actions</button>
      <button onclick="UploadService.openWizard('results')" class="button secondary-button">Bulk Import Results</button>
    </div>

    <div id="loading-indicator">Loading plates...</div>
    
    <table id="plates-table" class="display" style="width:100%; display:none;">
        <thead>
            <tr>
                <th>Short ID</th>
                <th>Name</th>
                <th>Project</th>
                <th>Format</th>
                <th>Created</th>
            </tr>
        </thead>
        <tbody></tbody>
    </table>

    <div id="create-plate-modal" class="modal" style="display:none;">
      <div class="modal-content">
        <span class="close-button">&times;</span>
        <h3>Create New Plate</h3>
        <div class="form-container">
            <div class="input-group">
                <label for="plate-project-select">Project</label>
                <div class="select-wrapper">
                    <select id="plate-project-select">
                        <option value="">Loading projects...</option>
                    </select>
                </div>
            </div>

            <div class="input-group">
                <label for="plate-name">Plate Name</label>
                <input type="text" id="plate-name" placeholder="e.g. DNA Extraction Run 1" />
            </div>

            <div class="input-group">
                <label for="plate-category-select">Plate Category</label>
                <div class="select-wrapper">
                    <select id="plate-category-select">
                        <option value="Experimental">Experimental (P-)</option>
                        <option value="Source">Source (S-)</option>
                    </select>
                </div>
            </div>

            <div class="input-group">
                <label for="plate-format-select">Format</label>
                <div class="select-wrapper">
                    <select id="plate-format-select">
                        <option value="96-well">96-well</option>
                        <option value="24-well">24-well</option>
                        <option value="48-well">48-well</option>
                        <option value="384-well">384-well</option>
                    </select>
                </div>
            </div>

            <button id="save-plate-btn" class="button">Save Plate</button>
        </div>
      </div>
    </div>
  `;

  const tableElement = $('#plates-table');
  const loadingIndicator = document.getElementById('loading-indicator');

  const createBtn = document.getElementById('create-plate-btn');
  const modal = document.getElementById('create-plate-modal');
  const closeBtn = modal.querySelector('.close-button');
  const saveBtn = document.getElementById('save-plate-btn');

  // Collapsible
  const collapsibleBar = document.getElementById('collapsible-bar');
  const collapsibleContent = document.getElementById('collapsible-actions');
  const collapsibleChevron = document.querySelector('.collapsible-chevron');
  function toggleCollapsible(expand) {
    const isOpen = collapsibleContent.classList.toggle('open', expand);
    collapsibleChevron.textContent = isOpen ? '▾' : '▸';
    collapsibleBar.querySelector('span:last-child').textContent = isOpen ? 'Hide actions' : 'Show actions';
  }
  collapsibleBar.onclick = () => toggleCollapsible();


  const projectSelect = document.getElementById('plate-project-select');
  const nameInput = document.getElementById('plate-name');
  const categorySelect = document.getElementById('plate-category-select');
  const formatSelect = document.getElementById('plate-format-select');

  let allProjects = [];

  try {
    const [projectsRes, platesRes] = await Promise.all([
      fetch('/api/v1/projects'),
      fetch('/api/v1/plates')
    ]);

    if (!projectsRes.ok || !platesRes.ok) throw new Error("Failed to fetch data");

    const projectsData = await projectsRes.json();
    const platesData = await platesRes.json();

    allProjects = projectsData.data;
    const plates = platesData.data;

    const projectMap = Object.fromEntries(allProjects.map(p => [p.id, p.name]));

    const tableBody = plates.map(plate => {
      const projectName = projectMap[plate.project_id] || `Project #${plate.project_id}`;
      const createdDate = new Date(plate.created_at * 1000).toLocaleDateString();

      return `
            <tr>
                <td>${plate.short_id || plate.id}</td>
                <td><a href="/plates/${plate.id}" class="id-link">${plate.name}</a></td>
                <td><a href="/projects/${plate.project_id}?from=plates&context_name=Plates" class="badge"><span class="emoji">📂</span> ${projectName}</a></td>
                <td>${plate.plate_format}</td>
                <td data-sort="${plate.created_at}">${createdDate}</td>
            </tr>`;
    }).join('');

    tableElement.find('tbody').html(tableBody);

    loadingIndicator.style.display = 'none';
    tableElement.show();

    tableElement.DataTable({
      order: [[4, "desc"]],
      pageLength: 25,
    });

  } catch (error) {
    console.error(error);
    loadingIndicator.style.display = 'none';
    mainElement.innerHTML = `<p class="error">Error loading plates: ${error.message}</p>`;
    return;
  }

  createBtn.addEventListener('click', () => {
    if (projectSelect.options.length <= 1) {
      projectSelect.innerHTML = allProjects.map(p =>
        `<option value="${p.id}">${p.name}</option>`
      ).join('');
    }
    modal.style.display = 'block';
    nameInput.focus();
  });

  closeBtn.addEventListener('click', () => modal.style.display = 'none');
  window.addEventListener('click', (e) => {
    if (e.target === modal) modal.style.display = 'none';
  });

  saveBtn.addEventListener('click', async () => {
    const name = nameInput.value.trim();
    const projectId = projectSelect.value;
    const category = categorySelect.value;
    const format = formatSelect.value;

    if (!name) return UIUtils.showToast("Please enter a plate name", "error");
    if (!projectId) return UIUtils.showToast("Please select a project", "error");

    try {
      const res = await fetch('/api/v1/plates', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: name,
          project_id: parseInt(projectId, 10),
          category: category,
          plate_format: format
        })
      });

      if (res.ok) {
        loadPlatesPage(mainElement);
      } else {
        const err = await res.json();
        UIUtils.showToast("Error creating plate: " + (err.error || "Unknown error"), "error");
      }
    } catch (e) {
      console.error(e);
      UIUtils.showToast("Network error creating plate", "error");
    }
  });
}
