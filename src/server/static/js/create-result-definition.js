/* src/server/static/js/create-result-definition.js */

async function loadCreateResultDefinitionPage(mainElement) {
  const backLinkHtml = generateBackArrow('/result-definitions');

  mainElement.innerHTML = `
    <div class="page-header">
      ${backLinkHtml}
      <h2>Create Result Definition</h2>
    </div>
    <div class="form-container">
      <div class="input-group">
        <label for="result-definition-name">Result Definition Name</label>
        <input type="text" id="result-definition-name" placeholder="Enter result definition name" />
      </div>

      <div class="input-group">
        <label for="result-definition-short-id">Short ID (Code Name)</label>
        <input type="text" id="result-definition-short-id" placeholder="e.g. imgcpttime" />
      </div>

      <div class="input-group">
        <label for="result-definition-category">Category</label>
        <div class="input-with-action">
            <div class="select-wrapper">
                <select id="result-definition-category">
                    <option value="">No Category</option>
                </select>
            </div>
            <button id="add-category-btn" class="button secondary-button" title="Add New Category">+</button>
        </div>
      </div>
      
      <div class="input-group">
        <label for="result-definition-data-type">Data Type</label>
        <div class="select-wrapper">
            <select id="result-definition-data-type">
                <option value="String">String</option>
                <option value="Integer">Integer</option>
                <option value="Float">Float</option>
                <option value="Boolean">Boolean</option>
                <option value="Date">Date</option>
                <option value="Datetime">Datetime</option>
                <option value="FileLink">FileLink</option>
                <optgroup label="Time Series">
                    <option value="StringSeries">TimeSeries (String)</option>
                    <option value="IntegerSeries">TimeSeries (Integer)</option>
                    <option value="FloatSeries">TimeSeries (Float)</option>
                    <option value="BooleanSeries">TimeSeries (Boolean)</option>
                    <option value="DateSeries">TimeSeries (Date)</option>
                    <option value="DatetimeSeries">TimeSeries (Datetime)</option>
                    <option value="FileLinkSeries">TimeSeries (FileLink)</option>
                </optgroup>
            </select>
        </div>
      </div>

      <div class="input-group">
        <label for="result-definition-units">Units (optional)</label>
        <input type="text" id="result-definition-units" placeholder="Enter units" />
      </div>

      <div class="input-group">
        <label for="result-definition-description">Description (optional)</label>
        <textarea id="result-definition-description" placeholder="Enter a description"></textarea>
      </div>

      <div class="input-group">
        <label for="result-definition-is-required">Is Required?</label>
        <div class="select-wrapper">
            <select id="result-definition-is-required">
                <option value="false">False</option>
                <option value="true">True</option>
            </select>
        </div>
      </div>

      <button id="save-button" class="button">Save</button>
    </div>

    <div id="add-category-modal" class="modal" style="display:none;">
        <div class="modal-content">
            <span class="close-button">&times;</span>
            <h2>Add New Category</h2>
            <div class="form-container">
                <div class="input-group">
                    <label for="new-category-name">Category Name</label>
                    <input type="text" id="new-category-name" placeholder="Enter category name" />
                </div>
                <div class="input-group">
                    <label for="new-category-description">Description (optional)</label>
                    <textarea id="new-category-description" placeholder="Enter a description"></textarea>
                </div>
                <button id="save-category-button" class="button">Save Category</button>
            </div>
        </div>
    </div>
  `;

  const nameInput = document.getElementById('result-definition-name');
  const shortIdInput = document.getElementById('result-definition-short-id');
  const categorySelect = document.getElementById('result-definition-category');
  const dataTypeSelect = document.getElementById('result-definition-data-type');
  const unitsInput = document.getElementById('result-definition-units');
  const descriptionInput = document.getElementById('result-definition-description');
  const isRequiredInput = document.getElementById('result-definition-is-required');
  const saveButton = document.getElementById('save-button');
  const addCategoryBtn = document.getElementById('add-category-btn');
  const modal = document.getElementById('add-category-modal');
  const closeButton = modal.querySelector('.close-button');
  const saveCategoryButton = document.getElementById('save-category-button');
  const newCategoryNameInput = document.getElementById('new-category-name');
  const newCategoryDescriptionInput = document.getElementById('new-category-description');

  async function loadCategories() {
    try {
      const response = await fetch('/api/v1/result-categories');
      const { data } = await response.json();
      categorySelect.innerHTML = `<option value="">No Category</option>` +
        data.map(cat => `<option value="${cat.id}">${UIUtils.escapeHTML(cat.name)}</option>`).join('');
    } catch (error) {
      console.error('Failed to load categories', error);
    }
  }

  addCategoryBtn.addEventListener('click', () => {
    modal.style.display = 'block';
  });

  closeButton.addEventListener('click', () => {
    modal.style.display = 'none';
  });

  window.addEventListener('click', (event) => {
    if (event.target == modal) {
      modal.style.display = 'none';
    }
  });

  saveCategoryButton.addEventListener('click', async () => {
    const name = newCategoryNameInput.value;
    const description = newCategoryDescriptionInput.value;

    if (!name) {
      UIUtils.showToast('Please enter a category name.', "error");
      return;
    }

    try {
      const response = await fetch('/api/v1/result-categories', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ name, description: description || "" })
      });

      if (response.ok) {
        newCategoryNameInput.value = '';
        newCategoryDescriptionInput.value = '';
        modal.style.display = 'none';
        await loadCategories();
      } else {
        const errorData = await response.json();
        UIUtils.showToast(`Error creating category: ${errorData.error || "Unknown error"}`, "error");
      }
    } catch (error) {
      console.error("Network error:", error);
      UIUtils.showToast("Network error occurred while creating category.", "error");
    }
  });

  saveButton.addEventListener('click', async () => {
    const name = nameInput.value.trim();
    const shortId = shortIdInput.value.trim();
    const dataType = dataTypeSelect.value;
    const unit = unitsInput.value;
    const description = descriptionInput.value;
    const categoryId = categorySelect.value ? parseInt(categorySelect.value, 10) : null;

    if (!name) {
      UIUtils.showToast('Please enter a result definition name.', "error");
      return;
    }

    if (!shortId) {
      UIUtils.showToast('Please enter a short ID (code name).', "error");
      return;
    }

    const payload = {
      name: name,
      short_id: shortId,
      data_type: dataType,
      unit: unit || null,
      description: description || null,
      category_id: categoryId,
      is_required: isRequiredInput.value === "true"
    };

    Object.keys(payload).forEach(k => payload[k] === null && delete payload[k]);

    try {
      const response = await fetch('/api/v1/result-definitions', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });

      if (response.ok) {
        history.pushState({}, "", "/result-definitions");
        handleRouting();
      } else {
        const errorData = await response.json();
        UIUtils.showToast(`Error creating result definition: ${errorData.error || "Unknown error"}`, "error");
      }
    } catch (error) {
      console.error("Network error:", error);
      UIUtils.showToast("Network error occurred while creating the result definition.", "error");
    }
  });

  loadCategories();

  // Attach special character helper to the units input and description textarea
  if (window.attachSpecialCharHelper) {
    window.attachSpecialCharHelper('result-definition-units');
    window.attachSpecialCharHelper('result-definition-description');
    window.attachSpecialCharHelper('new-category-description');
  }
}
