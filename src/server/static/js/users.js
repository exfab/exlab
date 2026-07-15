/* src/server/static/js/users.js */

async function loadUsersPage(mainElement) {
  mainElement.innerHTML = `
    <div class="page-header">
      <div style="flex-grow: 1;">
        <h2>System Users</h2>
      </div>
    </div>

    <div id="loading-indicator">Loading users...</div>
    
    <table id="users-table" class="display" style="width:100%; display:none;">
        <thead>
            <tr>
                <th>Email</th>
                <th>Role</th>
                <th>Created At</th>
                <th>Actions</th>
            </tr>
        </thead>
        <tbody></tbody>
    </table>

    <!-- Edit User Modal -->
    <div id="edit-user-modal" class="modal" style="display: none;">
      <div class="modal-content" style="max-width: 400px;">
        <span class="close-button" id="close-edit-modal">&times;</span>
        <h3>Edit User: <span id="edit-user-email-display"></span></h3>
        <form id="edit-user-form">
          <input type="hidden" id="edit-user-id">
          <input type="hidden" id="edit-user-email">
          
          <div class="form-group">
            <label for="edit-user-role">Role</label>
            <select id="edit-user-role" name="role">
              <option value="admin">Admin</option>
              <option value="lab_manager">Lab Manager</option>
              <option value="project_manager">Project Manager</option>
              <option value="project_user">Project User</option>
            </select>
          </div>

          <div class="form-group" style="margin-top: 1.5rem; border-top: 1px solid var(--border-color); padding-top: 1rem;">
            <label for="edit-user-password">Force Password Reset (Optional)</label>
            <input type="password" id="edit-user-password" name="password" placeholder="Leave blank to keep current">
            <small style="color: var(--text-secondary);">If provided, must be at least 8 characters.</small>
          </div>

          <button type="submit" class="button" id="save-user-btn" style="width: 100%; margin-top: 1rem;">Save Changes</button>
        </form>
      </div>
    </div>
  `;

  const tableElement = $('#users-table');
  const loadingIndicator = document.getElementById('loading-indicator');

  const fetchUsers = async () => {
    try {
      const response = await fetch('/api/v1/users');
      if (!response.ok) {
        throw new Error(`Error fetching users: ${response.statusText}`);
      }
      const json = await response.json();
      return json.data;
    } catch (error) {
      console.error(error);
      loadingIndicator.style.display = 'none';
      mainElement.innerHTML += `<p class="error">Failed to load users: ${error.message}</p>`;
      return [];
    }
  };

  const renderTable = (users) => {
    const tableBody = users.map(user => {
      const createdDate = new Date(user.created_at * 1000).toLocaleDateString();
      // Store user data in data attributes for easy retrieval
      const userDataStr = encodeURIComponent(JSON.stringify(user));
      return `
            <tr>
                <td>${user.email}</td>
                <td><span class="badge">${user.role}</span></td>
                <td data-sort="${user.created_at}">${createdDate}</td>
                <td>
                    <button class="button small-button edit-user-btn" data-user="${userDataStr}">Edit</button>
                    <button class="button small-button delete-user-btn" data-id="${user.id}" data-email="${user.email}" style="background-color: var(--danger-color); border-color: var(--danger-color);">Delete</button>
                </td>
            </tr>`;
    }).join('');

    if ($.fn.DataTable.isDataTable(tableElement)) {
        tableElement.DataTable().destroy();
    }

    tableElement.find('tbody').html(tableBody);
    loadingIndicator.style.display = 'none';
    tableElement.show();

    tableElement.DataTable({
      order: [[2, "desc"]],
      pageLength: 25,
      columnDefs: [
        { orderable: false, targets: 3 } // Disable sorting on actions column
      ]
    });

    attachEventListeners();
  };

  const users = await fetchUsers();
  if (users.length > 0) {
      renderTable(users);
  }

  // Edit Modal Logic
  const editModal = document.getElementById('edit-user-modal');
  const closeEditBtn = document.getElementById('close-edit-modal');
  const editForm = document.getElementById('edit-user-form');

  closeEditBtn.addEventListener('click', () => {
    editModal.style.display = 'none';
  });

  window.addEventListener('click', (e) => {
    if (e.target == editModal) {
      editModal.style.display = 'none';
    }
  });

  function attachEventListeners() {
    // Edit buttons
    document.querySelectorAll('.edit-user-btn').forEach(btn => {
        btn.addEventListener('click', (e) => {
            const user = JSON.parse(decodeURIComponent(e.target.dataset.user));
            document.getElementById('edit-user-id').value = user.id;
            document.getElementById('edit-user-email').value = user.email;
            document.getElementById('edit-user-email-display').textContent = user.email;
            document.getElementById('edit-user-role').value = user.role;
            document.getElementById('edit-user-password').value = ''; // Reset password field
            
            editModal.style.display = 'flex';
        });
    });

    // Delete buttons
    document.querySelectorAll('.delete-user-btn').forEach(btn => {
        btn.addEventListener('click', async (e) => {
            const userId = e.target.dataset.id;
            const userEmail = e.target.dataset.email;
            
            if (confirm(`Are you sure you want to permanently delete the user ${userEmail}? This action cannot be undone.`)) {
                try {
                    await ApiUtils.request(`/api/v1/users/${userId}`, { method: 'DELETE' });
                    UIUtils.showToast("User deleted successfully", "success");
                    // Refresh table
                    const updatedUsers = await fetchUsers();
                    renderTable(updatedUsers);
                } catch (error) {
                    UIUtils.handleError(error, "Failed to delete user");
                }
            }
        });
    });
  }

  // Handle Edit Form Submission
  editForm.addEventListener('submit', async (e) => {
    e.preventDefault();
    
    const id = document.getElementById('edit-user-id').value;
    const email = document.getElementById('edit-user-email').value; // Keep email same for now, API requires it
    const role = document.getElementById('edit-user-role').value;
    const password = document.getElementById('edit-user-password').value;
    const btn = document.getElementById('save-user-btn');

    const payload = {
        email: email,
        role: role
    };

    if (password) {
        if (password.length < 8) {
            UIUtils.showToast("Password must be at least 8 characters", "error");
            return;
        }
        payload.password = password;
    }

    try {
        btn.disabled = true;
        btn.textContent = 'Saving...';
        
        await ApiUtils.request(`/api/v1/users/${id}`, {
            method: 'PUT',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(payload)
        });

        UIUtils.showToast("User updated successfully", "success");
        editModal.style.display = 'none';
        
        // Refresh table
        const updatedUsers = await fetchUsers();
        renderTable(updatedUsers);

    } catch (error) {
        UIUtils.handleError(error, "Failed to update user");
    } finally {
        btn.disabled = false;
        btn.textContent = 'Save Changes';
    }
  });

}

