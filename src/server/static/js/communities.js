/* src/server/static/js/communities.js */

async function loadCommunitiesPage(mainElement) {
  mainElement.innerHTML = `
    <div class="page-header">
      <div style="flex-grow: 1;">
        <h2>All Communities</h2>
      </div>
      <div style="display: flex; gap: 10px;">
        <a href="/communities/new" class="button">Create Community</a>
      </div>
    </div>

    <div id="loading-indicator">Loading communities...</div>

    <table id="communities-table" class="display" style="width:100%; display:none;">
        <thead>
            <tr>
                <th>Name</th>
                <th>Notes</th>
                <th>Created At</th>
            </tr>
        </thead>
        <tbody></tbody>
    </table>
  `;

  const tableElement = $('#communities-table');
  const loadingIndicator = document.getElementById('loading-indicator');

  try {
    const response = await fetch('/api/v1/communities');
    if (!response.ok) {
      throw new Error(`Error fetching communities: ${response.statusText}`);
    }
    const json = await response.json();
    const communities = json.data;

    const tableBody = communities.map(c => {
      const createdDate = new Date(c.created_at * 1000).toLocaleDateString();
      return `
            <tr>
                <td><a href="/communities/${c.id}" class="id-link">${c.name}</a></td>
                <td>${c.notes || ''}</td>
                <td data-sort="${c.created_at}">${createdDate}</td>
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
    mainElement.innerHTML = `<p class="error">Failed to load communities: ${error.message}</p>`;
  }
}
