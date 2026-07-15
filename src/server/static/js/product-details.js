/* src/server/static/js/product-details.js */

/**
 * Loads and displays the details for a single product.
 */
async function loadProductDetailsPage(mainElement, productId) {
  const backLinkHtml = generateBackArrow('/products');

  mainElement.innerHTML = `
    <div class="page-header">
      <div style="display:flex; align-items:center; gap:15px;">
          ${backLinkHtml}
          <h2>Product Details</h2>
      </div>
      <div id="action-buttons">
          <button id="edit-btn" class="button">Edit</button>
          <button id="save-btn" class="button" style="display:none; background-color: #28a745; border-color: #28a745;">Save Changes</button>
          <button id="cancel-btn" class="secondary-button" style="display:none; margin-left: 10px;">Cancel</button>
      </div>
    </div>
    
    <div id="product-details-container">Loading product details...</div>
  `;

  const productDetailsContainer = document.getElementById('product-details-container');
  const editBtn = document.getElementById('edit-btn');
  const saveBtn = document.getElementById('save-btn');
  const cancelBtn = document.getElementById('cancel-btn');

  let currentProduct = null;

  function renderViewMode() {
    const product = currentProduct;
    productDetailsContainer.innerHTML = `
      <div class="info-grid">
        <div class="info-item">
            <span class="info-key">ID</span>
            <span class="info-value">${UIUtils.escapeHTML(product.short_id || product.id)}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Name</span>
            <span class="info-value">${UIUtils.escapeHTML(product.name)}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Brand</span>
            <span class="info-value">${UIUtils.escapeHTML(product.brand || 'N/A')}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Part Number</span>
            <span class="info-value">${UIUtils.escapeHTML(product.manufacturer_part_number || 'N/A')}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Description</span>
            <span class="info-value">${UIUtils.escapeHTML(product.description || 'N/A')}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Created</span>
            <span class="info-value">${new Date(product.created_at * 1000).toLocaleString()}</span>
        </div>
      </div>
      `;
    editBtn.style.display = 'inline-block';
    saveBtn.style.display = 'none';
    cancelBtn.style.display = 'none';
  }

  function renderEditMode() {
    const product = currentProduct;
    productDetailsContainer.innerHTML = `
      <div class="info-grid">
        <div class="info-item">
            <span class="info-key">ID</span>
            <span class="info-value">${UIUtils.escapeHTML(product.short_id || product.id)}</span>
        </div>
        <div class="info-item">
            <span class="info-key">Name</span>
            <span class="info-value"><input type="text" id="edit-name" value="${UIUtils.escapeHTML(product.name)}"></span>
        </div>
        <div class="info-item">
            <span class="info-key">Brand</span>
            <span class="info-value"><input type="text" id="edit-brand" value="${UIUtils.escapeHTML(product.brand || '')}"></span>
        </div>
        <div class="info-item">
            <span class="info-key">Part Number</span>
            <span class="info-value"><input type="text" id="edit-part-number" value="${UIUtils.escapeHTML(product.manufacturer_part_number || '')}"></span>
        </div>
        <div class="info-item">
            <span class="info-key">Description</span>
            <span class="info-value"><textarea id="edit-description" rows="3">${UIUtils.escapeHTML(product.description || '')}</textarea></span>
        </div>
        <div class="info-item">
            <span class="info-key">Created</span>
            <span class="info-value">${new Date(product.created_at * 1000).toLocaleString()}</span>
        </div>
      </div>
      `;
    editBtn.style.display = 'none';
    saveBtn.style.display = 'inline-block';
    cancelBtn.style.display = 'inline-block';
    
    if (window.attachSpecialCharHelper) {
      window.attachSpecialCharHelper('edit-description');
    }
  }

  try {
    const productResponse = await fetch(`/api/v1/products/${productId}`);

    if (!productResponse.ok) throw new Error(`Product not found (ID: ${productId})`);

    currentProduct = await productResponse.json();

    renderViewMode();

    editBtn.onclick = renderEditMode;
    cancelBtn.onclick = renderViewMode;

    saveBtn.onclick = async () => {
      const payload = {
        name: document.getElementById('edit-name').value,
        brand: document.getElementById('edit-brand').value || null,
        manufacturer_part_number: document.getElementById('edit-part-number').value || null,
        description: document.getElementById('edit-description').value || null
      };

      Object.keys(payload).forEach(k => payload[k] === null && delete payload[k]);

      const res = await fetch(`/api/v1/products/${productId}`, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });

      if (res.ok) {
        currentProduct = await res.json();
        renderViewMode();
      } else {
        let errorMessage = 'Failed to update product';
        try {
          const errorData = await res.json();
          errorMessage = errorData.error || errorMessage;
        } catch (err) { }
        UIUtils.showToast(`Error: ${errorMessage}`, "error");
      }
    };

  } catch (error) {
    console.error('Error loading product details:', error);
    productDetailsContainer.innerHTML = `<div class="error">Error: ${error.message}</div>`;
  }
}
