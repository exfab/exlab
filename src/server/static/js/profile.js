/* src/server/static/js/profile.js */

async function loadProfilePage(mainElement) {
  mainElement.innerHTML = `
    <div class="page-header">
      <div style="flex-grow: 1;">
        <h2>My Profile</h2>
      </div>
    </div>

    <div class="container" style="max-width: 600px; margin: 2rem auto;">
      <div class="card" style="padding: 2rem;">
        <h3 style="margin-top: 0;">Account Details</h3>
        <div id="profile-info" style="margin-bottom: 2rem; padding-bottom: 2rem; border-bottom: 1px solid var(--border-color);">
          <p><strong>Email:</strong> <span id="profile-email">Loading...</span></p>
          <p><strong>Role:</strong> <span id="profile-role" class="badge">...</span></p>
          <p><strong>Joined:</strong> <span id="profile-joined">...</span></p>
        </div>

        <h3 style="margin-top: 0;">Change Password</h3>
        <form id="change-password-form">
          <div class="form-group">
            <label for="current-password">Current Password</label>
            <input type="password" id="current-password" name="current-password" required>
          </div>
          <div class="form-group">
            <label for="new-password">New Password</label>
            <input type="password" id="new-password" name="new-password" required minlength="8">
            <small style="color: var(--text-secondary);">Must be at least 8 characters.</small>
          </div>
          <div class="form-group">
            <label for="confirm-password">Confirm New Password</label>
            <input type="password" id="confirm-password" name="confirm-password" required minlength="8">
          </div>
          
          <button type="submit" class="button" id="change-pwd-btn">Update Password</button>
        </form>
      </div>
    </div>
  `;

  // Fetch current user details
  try {
    const res = await ApiUtils.request('/api/v1/me');
    document.getElementById('profile-email').textContent = res.email;
    document.getElementById('profile-role').textContent = res.role;
    
    if (res.created_at) {
        document.getElementById('profile-joined').textContent = new Date(res.created_at * 1000).toLocaleDateString();
    } else {
        document.getElementById('profile-joined').textContent = 'N/A';
    }
  } catch (error) {
    UIUtils.handleError(error, "Failed to load profile data");
  }

  // Handle password change form submission
  const form = document.getElementById('change-password-form');
  form.addEventListener('submit', async (e) => {
    e.preventDefault();
    
    const currentPassword = document.getElementById('current-password').value;
    const newPassword = document.getElementById('new-password').value;
    const confirmPassword = document.getElementById('confirm-password').value;
    const btn = document.getElementById('change-pwd-btn');

    if (newPassword !== confirmPassword) {
      UIUtils.showToast("New passwords do not match", "error");
      return;
    }

    if (newPassword.length < 8) {
        UIUtils.showToast("New password must be at least 8 characters", "error");
        return;
    }

    try {
      btn.disabled = true;
      btn.textContent = "Updating...";

      await ApiUtils.request('/api/v1/me/password', {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          current_password: currentPassword,
          new_password: newPassword
        })
      });

      UIUtils.showToast("Password updated successfully", "success");
      form.reset();
    } catch (error) {
      UIUtils.handleError(error, "Failed to update password");
    } finally {
      btn.disabled = false;
      btn.textContent = "Update Password";
    }
  });
}
