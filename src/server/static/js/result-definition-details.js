/* src/server/static/js/result-definition-details.js */

/**
 * Loads and displays the details for a single result definition.
 */
async function loadResultDefinitionDetailsPage(mainElement, resultDefinitionId) {
  const backLinkHtml = generateBackArrow('/result-definitions');

  mainElement.innerHTML = `
    <div class="page-header">
      <div style="display:flex; align-items:center; gap:15px;">
        ${backLinkHtml}
        <h2>Result Definition Details</h2>
      </div>
      <div id="action-buttons">
          <button id="edit-btn" class="button">Edit</button>
          <button id="save-btn" class="button" style="display:none; background-color: #28a745; border-color: #28a745;">Save Changes</button>
          <button id="cancel-btn" class="secondary-button" style="display:none; margin-left: 10px;">Cancel</button>
      </div>
    </div>
    
    <div id="result-definition-details-container">Loading result definition details...</div>
  `;

  const resultDefinitionDetailsContainer = document.getElementById('result-definition-details-container');
  const editBtn = document.getElementById('edit-btn');
  const saveBtn = document.getElementById('save-btn');
  const cancelBtn = document.getElementById('cancel-btn');

  let currentResultDefinition = null;
  let allCategories = [];

  function renderViewMode() {
    const resultDefinition = currentResultDefinition;
    resultDefinitionDetailsContainer.innerHTML = `
      <div class="info-grid">
        <div class="info-item">
            <span class="info-key">ID</span>
            <span class="info-value">${UIUtils.escapeHTML(resultDefinition.id)}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Short ID</span>
            <span class="info-value">${UIUtils.escapeHTML(resultDefinition.short_id)}</span>
        </div>
        <div class="info-item">
            <span class="info-key">UID</span>
            <span class="info-value">${UIUtils.escapeHTML(resultDefinition.uid)}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Name</span>
            <span class="info-value">${UIUtils.escapeHTML(resultDefinition.name)}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Category</span>
            <span class="info-value">${resultDefinition.category ? UIUtils.escapeHTML(resultDefinition.category.name) : 'N/A'}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Category Description</span>
            <span class="info-value">${resultDefinition.category && resultDefinition.category.description ? UIUtils.escapeHTML(resultDefinition.category.description) : 'N/A'}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Description</span>
            <span class="info-value">${UIUtils.escapeHTML(resultDefinition.description || 'N/A')}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Data Type</span>
            <span class="info-value">${UIUtils.escapeHTML(resultDefinition.data_type)}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Units</span>
            <span class="info-value">${UIUtils.escapeHTML(resultDefinition.unit || 'N/A')}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Required</span>
            <span class="info-value">${resultDefinition.is_required ? 'Yes' : 'No'}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Created</span>
            <span class="info-value">${new Date(resultDefinition.created_at * 1000).toLocaleString()}</span>
        </div>
      </div>
    `;
    editBtn.style.display = 'inline-block';
    saveBtn.style.display = 'none';
    cancelBtn.style.display = 'none';
  }

  function renderEditMode() {
    const rd = currentResultDefinition;

    const categoryOptions = allCategories.map(cat => {
        const isSelected = rd.category && rd.category.id === cat.id ? 'selected' : '';
        return `<option value="${cat.id}" ${isSelected}>${UIUtils.escapeHTML(cat.name)}</option>`;
    }).join('');

    const dtOptionsList = ['String', 'Integer', 'Float', 'Boolean', 'Date', 'Datetime', 'FileLink', 'StringSeries', 'IntegerSeries', 'FloatSeries', 'BooleanSeries', 'DateSeries', 'DatetimeSeries', 'FileLinkSeries'];
    const dtOptions = dtOptionsList.map(dt => {
        const isSelected = rd.data_type === dt ? 'selected' : '';
        return `<option value="${dt}" ${isSelected}>${dt}</option>`;
    }).join('');

    resultDefinitionDetailsContainer.innerHTML = `
      <div class="info-grid">
        <div class="info-item">
            <span class="info-key">ID</span>
            <span class="info-value">${UIUtils.escapeHTML(rd.id)}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Short ID</span>
            <span class="info-value"><input type="text" id="edit-short-id" value="${UIUtils.escapeHTML(rd.short_id)}"></span>
        </div>
        <div class="info-item">
            <span class="info-key">UID</span>
            <span class="info-value">${UIUtils.escapeHTML(rd.uid)}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Name</span>
            <span class="info-value"><input type="text" id="edit-name" value="${UIUtils.escapeHTML(rd.name)}"></span>
        </div>
        <div class="info-item">
            <span class="info-key">Category</span>
            <span class="info-value">
                <div class="select-wrapper" style="width: auto;">
                    <select id="edit-category">
                        <option value="">No Category</option>
                        ${categoryOptions}
                    </select>
                </div>
            </span>
        </div>
        <div class="info-item">
            <span class="info-key">Description</span>
            <span class="info-value"><textarea id="edit-description" rows="3">${UIUtils.escapeHTML(rd.description || '')}</textarea></span>
        </div>
        <div class="info-item">
            <span class="info-key">Data Type</span>
            <span class="info-value">
                <div class="select-wrapper" style="width: auto;">
                    <select id="edit-data-type">
                        ${dtOptions}
                    </select>
                </div>
            </span>
        </div>
        <div class="info-item">
            <span class="info-key">Units</span>
            <span class="info-value"><input type="text" id="edit-units" value="${UIUtils.escapeHTML(rd.unit || '')}"></span>
        </div>
        <div class="info-item">
            <span class="info-key">Required</span>
            <span class="info-value">
                <div class="select-wrapper" style="width: auto;">
                    <select id="edit-is-required">
                        <option value="false" ${!rd.is_required ? 'selected' : ''}>False</option>
                        <option value="true" ${rd.is_required ? 'selected' : ''}>True</option>
                    </select>
                </div>
            </span>
        </div>
        <div class="info-item">
            <span class="info-key">Created</span>
            <span class="info-value">${new Date(rd.created_at * 1000).toLocaleString()}</span>
        </div>
      </div>
    `;

    editBtn.style.display = 'none';
    saveBtn.style.display = 'inline-block';
    cancelBtn.style.display = 'inline-block';

    if (window.attachSpecialCharHelper) {
        window.attachSpecialCharHelper('edit-units');
        window.attachSpecialCharHelper('edit-description');
    }
  }

  async function fetchCategories() {
      try {
          const res = await fetch('/api/v1/result-categories');
          if (res.ok) {
              const data = await res.json();
              allCategories = data.data;
          }
      } catch(e) {
          console.error("Could not fetch categories", e);
      }
  }

  try {
    const resultDefinitionResponse = await fetch(`/api/v1/result-definitions/${resultDefinitionId}`);

    if (!resultDefinitionResponse.ok) throw new Error(`Result Definition not found (ID: ${resultDefinitionId})`);

    currentResultDefinition = await resultDefinitionResponse.json();

    renderViewMode();

    editBtn.onclick = async () => {
        await fetchCategories();
        renderEditMode();
    };

    cancelBtn.onclick = renderViewMode;

    saveBtn.onclick = async () => {
      const catVal = document.getElementById('edit-category').value;
      const isReqVal = document.getElementById('edit-is-required').value === "true";
      const payload = {
        short_id: document.getElementById('edit-short-id').value.trim(),
        name: document.getElementById('edit-name').value,
        description: document.getElementById('edit-description').value || null,
        data_type: document.getElementById('edit-data-type').value,
        unit: document.getElementById('edit-units').value || null,
        category_id: catVal ? parseInt(catVal, 10) : null,
        is_required: isReqVal
      };

      Object.keys(payload).forEach(k => payload[k] === null && delete payload[k]);

      const res = await fetch(`/api/v1/result-definitions/${resultDefinitionId}`, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });

      if (res.ok) {
        currentResultDefinition = await res.json();
        renderViewMode();
      } else {
        let errorMessage = 'Failed to update result definition';
        try {
          const errorData = await res.json();
          errorMessage = errorData.error || errorMessage;
        } catch (err) { }
        UIUtils.showToast(`Error: ${errorMessage}`, "error");
      }
    };

  } catch (error) {
    console.error('Error loading result definition details:', error);
    resultDefinitionDetailsContainer.innerHTML = `<div class="error">Error: ${error.message}</div>`;
  }
}
