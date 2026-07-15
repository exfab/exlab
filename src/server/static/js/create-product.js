/* src/server/static/js/create-product.js */

async function loadCreateProductPage(mainElement) {
  const backLinkHtml = generateBackArrow('/products');

  mainElement.innerHTML = `
    <div class="page-header">
      ${backLinkHtml}
      <h2>Create Product</h2>
    </div>
    <div class="form-container">
      
      <div class="input-group">
        <label for="product-name">Product Name <span style="color:red">*</span></label>
        <input type="text" id="product-name" placeholder="e.g. 96-well Microplate" />
      </div>

      <div class="input-group">
        <label for="product-brand">Brand</label>
        <input type="text" id="product-brand" placeholder="e.g. Corning" />
      </div>

      <div class="input-group">
        <label for="product-part-number">Manufacturer Part Number</label>
        <input type="text" id="product-part-number" placeholder="e.g. 3370" />
      </div>

      <div class="input-group">
        <label for="product-description">Description</label>
        <textarea id="product-description" rows="3" placeholder="Additional details..."></textarea>
      </div>

      <button id="save-button" class="button">Save Product</button>
    </div>
  `;

  const nameInput = document.getElementById('product-name');
  const brandInput = document.getElementById('product-brand');
  const partNumberInput = document.getElementById('product-part-number');
  const descriptionInput = document.getElementById('product-description');
  const saveButton = document.getElementById('save-button');

  // Handle Save
  saveButton.addEventListener('click', async () => {
    const name = nameInput.value.trim();
    const brand = brandInput.value.trim();
    const partNumber = partNumberInput.value.trim();
    const description = descriptionInput.value.trim();

    if (!name) return UIUtils.showToast('Please enter a Product Name.', "error");

    const payload = {
      name: name,
      brand: brand || null,
      manufacturer_part_number: partNumber || null,
      description: description || null
    };

    Object.keys(payload).forEach(k => payload[k] === null && delete payload[k]);

    try {
      const res = await fetch('/api/v1/products', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });

      if (res.ok) {
        history.pushState({}, "", "/products");
        handleRouting();
      } else {
        const err = await res.json();
        UIUtils.showToast(`Error: ${err.error || 'Failed to create product'}`, "error");
      }
    } catch (error) {
      console.error('Error saving product:', error);
      UIUtils.showToast('Network error occurred.', "error");
    }
  });
}
