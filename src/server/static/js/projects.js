/* src/server/static/js/projects.js */

async function loadProjectsPage(mainElement) {
  mainElement.innerHTML = `
    <div class="page-header">
      <div style="flex-grow: 1;">
        <h2>All Projects</h2>
      </div>
      <label id="wrap-show-archived-projects" class="show-archived-toggle" style="display:none; margin-right: 14px;">
        <input type="checkbox" id="cb-show-archived-projects">
        <span id="lbl-show-archived-projects">Show archived</span>
      </label>
      <a href="/api/v1/projects?format=csv" class="button secondary-button" style="margin-right: 10px;">Export CSV</a>
      <a href="/projects/new" class="button">Create Project</a>
    </div>

    <div id="loading-indicator">Loading projects...</div>
    
    <div id="table-container" style="display:none; position: relative;">
        <table id="projects-table" class="display" style="width:100%;">
            <thead>
                <tr>
                    <!-- Headers injected dynamically -->
                </tr>
            </thead>
            <tbody></tbody>
        </table>
    </div>
  `;

  const tableElement = $('#projects-table');
  const loadingIndicator = document.getElementById('loading-indicator');
  const tableContainer = document.getElementById('table-container');

  try {
    const response = await fetch('/api/v1/projects');
    if (!response.ok) {
      throw new Error(`Error fetching projects: ${response.statusText}`);
    }
      const json = await response.json();
      const allProjects = json.data || [];
      const archivedWrap = document.getElementById('wrap-show-archived-projects');
      const archivedCb = document.getElementById('cb-show-archived-projects');
      const archivedLbl = document.getElementById('lbl-show-archived-projects');
      const archivedCount = allProjects.filter(p => p.status === 'Archived').length;

      if (archivedCount > 0 && archivedWrap && archivedLbl) {
        archivedLbl.textContent = `Show archived (${archivedCount})`;
        archivedWrap.style.display = 'inline-flex';
      }
  
      // --- Discover dynamic metadata keys ---
      const metadataKeys = new Set();
      allProjects.forEach(project => {
        if (project.metadata && typeof project.metadata === 'object') {
          Object.keys(project.metadata).forEach(key => metadataKeys.add(key));
        }
      });
      const sortedMetadataKeys = Array.from(metadataKeys).sort();
  
      // --- Inject headers ---
      const theadTr = tableElement.find('thead tr');
      
      // 1. Static Initial Headers
      let headerHtml = `
          <th>Name</th>
          <th>Status</th>
          <th>Short ID</th>
      `;
      
      // 2. Dynamic Metadata Headers
      sortedMetadataKeys.forEach(key => {
        headerHtml += `<th>${key}</th>`;
      });
      
      // 3. Static Trailing Headers
      headerHtml += `
          <th>Owner</th>
          <th>Contact</th>
          <th>Created</th>
          <th class="col-description">Description</th>
          <th class="col-updated">Updated</th>
      `;
      
      theadTr.html(headerHtml);
      const createdColumnIndex = 3 + sortedMetadataKeys.length + 2;

      const renderProjectsTable = () => {
        const includeArchived = archivedCb && archivedCb.checked;
        const projects = includeArchived
          ? allProjects
          : allProjects.filter(p => p.status !== 'Archived');

        if ($.fn.DataTable.isDataTable('#projects-table')) {
          tableElement.DataTable().destroy();
        }

        const tableBody = projects.map(project => {
          const createdDate = new Date(project.created_at * 1000).toLocaleDateString();
          const updatedTime = project.updated_at ? project.updated_at : project.created_at;
          const updatedDate = new Date(updatedTime * 1000).toLocaleDateString();

          let rowHtml = `
                <tr>
                    <td><a href="/projects/${project.short_id}" class="id-link">${project.name}</a></td>
                    <td><span class="status-pill status-${project.status.toLowerCase()}">${project.status}</span></td>
                    <td><code>${project.short_id || project.id}</code></td>`;

          sortedMetadataKeys.forEach(key => {
              const val = (project.metadata && project.metadata[key]) ? project.metadata[key] : '';
              const displayVal = typeof val === 'object' ? JSON.stringify(val) : val;
              rowHtml += `<td>${displayVal}</td>`;
          });

          rowHtml += `
                    <td>${project.owner || ''}</td>
                    <td>${project.contact_name || ''}</td>
                    <td data-sort="${project.created_at}">${createdDate}</td>
                    <td>${project.description || ''}</td>
                    <td data-sort="${updatedTime}">${updatedDate}</td>
                </tr>`;

          return rowHtml;
        }).join('');

        tableElement.find('tbody').html(tableBody);
        loadingIndicator.style.display = 'none';
        tableContainer.style.display = 'block';

        tableElement.DataTable({
          order: [[ createdColumnIndex, "desc" ]],
          pageLength: 25,
          stateSave: true,
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
            { type: 'natural', targets: [0, 2] },
            { targets: 'col-updated', visible: false },
            { targets: 'col-description', orderable: false }
          ]
        });
      };

      renderProjectsTable();
      if (archivedCb) {
        archivedCb.addEventListener('change', renderProjectsTable);
      }
    
  } catch (error) {
    console.error(error);
    loadingIndicator.style.display = 'none';
    mainElement.innerHTML = `<p class="error">Failed to load projects: ${error.message}</p>`;
  }
}
