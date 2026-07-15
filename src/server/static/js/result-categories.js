/* src/server/static/js/result-categories.js */

async function loadResultCategoriesPage(mainElement) {
  const backLinkHtml = generateBackArrow('/result-definitions');

  mainElement.innerHTML = `
    <div class="page-header">
      <div style="flex-grow: 1; display: flex; align-items: center; gap: 15px;">
        ${backLinkHtml}
        <h2>Result Categories</h2>
      </div>
      <button id="create-category-btn" class="button">Create Category</button>
    </div>

    <div id="loading-indicator">Loading categories...</div>
    
    <table id="result-categories-table" class="display" style="width:100%; display:none;">
        <thead>
            <tr>
                <th>Name</th>
                <th>Description</th>
                <th>Actions</th>
            </tr>
        </thead>
        <tbody></tbody>
    </table>

    <!-- Create/Edit Modal -->
    <div id="category-modal" class="modal" style="display:none;">
        <div class="modal-content">
            <span class="close-button" id="close-modal-btn">&times;</span>
            <h2 id="modal-title">Create Category</h2>
            <div class="form-container">
                <input type="hidden" id="modal-category-id" />
                <div class="input-group">
                    <label for="modal-category-name">Category Name</label>
                    <input type="text" id="modal-category-name" placeholder="Enter category name" />
                </div>
                <div class="input-group">
                    <label for="modal-category-description">Description (optional)</label>
                    <textarea id="modal-category-description" placeholder="Enter a description"></textarea>
                </div>
                <button id="save-category-btn" class="button">Save Category</button>
            </div>
        </div>
    </div>
  `;

  const tableElement = $('#result-categories-table');
  const loadingIndicator = document.getElementById('loading-indicator');
  const createBtn = document.getElementById('create-category-btn');
  const modal = document.getElementById('category-modal');
  const closeBtn = document.getElementById('close-modal-btn');
  const saveBtn = document.getElementById('save-category-btn');
  const modalTitle = document.getElementById('modal-title');
  const idInput = document.getElementById('modal-category-id');
  const nameInput = document.getElementById('modal-category-name');
  const descInput = document.getElementById('modal-category-description');

  let dt; // DataTable instance
  let allCategories = [];

  async function fetchAndRender() {
    loadingIndicator.style.display = 'block';
    tableElement.hide();
    
    try {
      const response = await fetch('/api/v1/result-categories');
      if (!response.ok) throw new Error(`Error: ${response.statusText}`);
      
      const json = await response.json();
      allCategories = json.data;

      const tableBody = allCategories.map(cat => {
        return `
          <tr>
              <td style="font-weight: bold;">${cat.name}</td>
              <td>${cat.description || 'N/A'}</td>
              <td><button class="button secondary-button edit-cat-btn" data-id="${cat.id}">Edit</button></td>
          </tr>`;
      }).join('');

      // Destroy previous DataTable if it exists
      if ($.fn.DataTable.isDataTable('#result-categories-table')) {
          $('#result-categories-table').DataTable().destroy();
      }

      tableElement.find('tbody').html(tableBody);
      loadingIndicator.style.display = 'none';
      tableElement.show();

      dt = tableElement.DataTable({
        pageLength: 25
      });

      // Attach edit listeners
      document.querySelectorAll('.edit-cat-btn').forEach(btn => {
          btn.addEventListener('click', (e) => {
              const id = parseInt(e.target.getAttribute('data-id'), 10);
              const cat = allCategories.find(c => c.id === id);
              if (cat) openModal(cat);
          });
      });

    } catch (error) {
      console.error(error);
      loadingIndicator.style.display = 'none';
      mainElement.innerHTML += `<p class="error">Failed to load categories: ${error.message}</p>`;
    }
  }

  function openModal(cat = null) {
      if (cat) {
          modalTitle.textContent = "Edit Category";
          idInput.value = cat.id;
          nameInput.value = cat.name;
          descInput.value = cat.description || '';
      } else {
          modalTitle.textContent = "Create Category";
          idInput.value = '';
          nameInput.value = '';
          descInput.value = '';
      }
      modal.style.display = 'block';
  }

  function closeModal() {
      modal.style.display = 'none';
  }

  createBtn.addEventListener('click', () => openModal(null));
  closeBtn.addEventListener('click', closeModal);
  window.addEventListener('click', (event) => {
    if (event.target == modal) closeModal();
  });

  saveBtn.addEventListener('click', async () => {
      const id = idInput.value;
      const name = nameInput.value.trim();
      const description = descInput.value.trim() || null;

      if (!name) return UIUtils.showToast("Category Name is required.", "error");

      const payload = { name, description };
      Object.keys(payload).forEach(k => payload[k] === null && delete payload[k]);

      const isEdit = id !== '';
      const url = isEdit ? `/api/v1/result-categories/${id}` : '/api/v1/result-categories';
      const method = isEdit ? 'PUT' : 'POST';

      try {
          const res = await fetch(url, {
              method,
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify(payload)
          });

          if (res.ok) {
              closeModal();
              fetchAndRender();
          } else {
              const err = await res.json();
              UIUtils.showToast(`Error: ${err.error || 'Failed to save'}`, "error");
          }
      } catch(e) {
          console.error(e);
          UIUtils.showToast("Network error.", "error");
      }
  });

  if (window.attachSpecialCharHelper) {
      window.attachSpecialCharHelper('modal-category-description');
  }

  fetchAndRender();
}
