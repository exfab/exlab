/* src/server/static/js/products.js */

async function loadProductsPage(mainElement) {
  mainElement.innerHTML = `
    <div class="page-header">
      <div style="flex-grow: 1;">
        <h2>All Products</h2>
      </div>
      <a href="/products/new" class="button">Create Product</a>
    </div>

    <div id="loading-indicator">Loading products...</div>
    
    <table id="products-table" class="display" style="width:100%; display:none;">
        <thead>
            <tr>
                <th>Short ID</th>
                <th>Name</th>
                <th>Created At</th>
                <th>Price</th>
            </tr>
        </thead>
        <tbody></tbody>
    </table>
  `;

  const tableElement = $('#products-table');
  const loadingIndicator = document.getElementById('loading-indicator');

  try {
    const response = await fetch('/api/v1/products');
    if (!response.ok) {
      throw new Error(`Error fetching products: ${response.statusText}`);
    }
    const json = await response.json();
    const products = json.data;

    const tableBody = products.map(product => {
      const createdDate = new Date(product.created_at * 1000).toLocaleDateString();
      const identifier = product.short_id || product.id;
      return `
            <tr>
                <td>${identifier}</td>
                <td><a href="/products/${identifier}" class="id-link">${product.name}</a></td>
                <td data-sort="${product.created_at}">${createdDate}</td>
                <td>${product.price || '0.00'}</td>
            </tr>`;
    }).join('');

    tableElement.find('tbody').html(tableBody);
    loadingIndicator.style.display = 'none';
    tableElement.show();

    tableElement.DataTable({
      order: [[2, "desc"]],
      pageLength: 25
    });

  } catch (error) {
    console.error(error);
    loadingIndicator.style.display = 'none';
    mainElement.innerHTML = `<p class="error">Failed to load products: ${error.message}</p>`;
  }
}
