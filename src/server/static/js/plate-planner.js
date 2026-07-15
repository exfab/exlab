/* src/server/static/js/plate-planner.js */

async function loadPlatePlannerPage(mainElement, plateId) {
  const backLinkHtml = generateBackArrow(`/plates/${plateId}`);

  mainElement.innerHTML = `
    <div class="page-header">
      <div>
        ${backLinkHtml}
        <h2 id="plate-title">Plate Planner</h2>
      </div>
      <div class="header-actions">
        <div class="btn-group" id="planner-tool-group" style="margin-right: 20px;">
            <button class="filter-btn active" data-tool="assign">🟢 Assign</button>
            <button class="filter-btn" data-tool="unassign">🔴 Unassign</button>
            <button class="filter-btn" data-tool="control">⚪ Mark Control</button>
        </div>
        <button id="commit-layout-button" class="button" style="background-color: #28a745; border-color: #28a745; display:none;">Save Layout</button>
      </div>
    </div>

    <div style="display: grid; grid-template-columns: 2fr 1fr; gap: 20px;">
      <!-- LEFT PANE: The Grid -->
      <div class="plate-stage" style="padding: 0; justify-content: flex-start;">
        <div class="plate-frame">
          <div id="well-grid">Loading...</div>
        </div>
        <div style="margin-top: 15px;">
            <p id="selection-status" class="text-secondary">0 wells selected</p>
        </div>
      </div>

      <!-- RIGHT PANE: The Sample Pool -->
      <div class="sample-pool-panel" style="background: var(--bg-dark-2); padding: 20px; border-radius: 8px; border: 1px solid var(--border-color); display: flex; flex-direction: column; height: 600px;">
          <h3>Source Pool</h3>
          <p class="text-secondary" style="font-size: 0.9em; margin-bottom: 15px;">Select unassigned samples from the project to auto-fill into the highlighted wells.</p>
          
          <div style="margin-bottom: 15px; display:flex; gap: 10px;">
              <select id="strategy-select" style="flex: 1;">
                <option value="sequential">Sequential Fill (Option B)</option>
              </select>
              <button id="auto-fill-btn" class="button">Auto-Fill Selected</button>
          </div>

          <div style="flex-grow: 1; overflow-y: auto; border: 1px solid var(--border-color); background: var(--bg-dark); padding: 10px; border-radius: 4px;">
              <table id="sample-pool-table" style="width: 100%; border-collapse: collapse; text-align: left;">
                  <thead>
                      <tr style="border-bottom: 1px solid var(--border-color);">
                          <th style="padding: 8px;"><input type="checkbox" id="select-all-pool"></th>
                          <th style="padding: 8px;">Short ID</th>
                          <th style="padding: 8px;">Type</th>
                      </tr>
                  </thead>
                  <tbody id="sample-pool-list">
                      <tr><td colspan="3" style="text-align: center; padding: 15px;">Loading samples...</td></tr>
                  </tbody>
              </table>
          </div>
      </div>
    </div>
  `;

  // UI Elements
  const wellGrid = document.getElementById('well-grid');
  const plateTitle = document.getElementById('plate-title');
  const samplePoolList = document.getElementById('sample-pool-list');
  const selectAllPool = document.getElementById('select-all-pool');
  const autoFillBtn = document.getElementById('auto-fill-btn');
  const strategySelect = document.getElementById('strategy-select');
  const selectionStatus = document.getElementById('selection-status');
  const toolButtons = document.querySelectorAll('#planner-tool-group .filter-btn');

  let currentTool = 'assign';
  let wells = [];
  let unassignedSamples = [];
  let isDragging = false;
  
  toolButtons.forEach(btn => {
      btn.addEventListener('click', (e) => {
          toolButtons.forEach(b => b.classList.remove('active'));
          e.target.classList.add('active');
          currentTool = e.target.dataset.tool;
          
          document.querySelectorAll('.well').forEach(w => w.classList.remove('selected'));
          updateSelectionStatus();
          
          if (currentTool === 'unassign') {
              autoFillBtn.textContent = 'Unassign Selected';
              autoFillBtn.style.backgroundColor = '#dc3545';
              autoFillBtn.style.borderColor = '#dc3545';
          } else {
              autoFillBtn.textContent = 'Auto-Fill Selected';
              autoFillBtn.style.backgroundColor = '';
              autoFillBtn.style.borderColor = '';
          }
      });
  });

  // Fetch Data
  try {
    const plateRes = await fetch(`/api/v1/plates/${plateId}`);
    if (!plateRes.ok) throw new Error("Plate not found");
    const plateData = await plateRes.json();
    const plate = plateData.plate;
    wells = plateData.wells;

    plateTitle.textContent = `Plan [${plate.short_id || plate.id}] ${plate.name}`;

    const samplesRes = await fetch(`/api/v1/projects/${plate.project_id}/samples`);
    const samplesData = await samplesRes.json();
    const allSamples = samplesData.data;

    // Filter out samples already assigned to ANY well on THIS plate
    const assignedSampleIds = new Set(wells.filter(w => w.well.sample_id).map(w => w.well.sample_id));
    unassignedSamples = allSamples.filter(s => !assignedSampleIds.has(s.id) && s.status !== 'Archived');

    renderGrid(plate.plate_format);
    renderSamplePool();

  } catch (e) {
    console.error(e);
    mainElement.innerHTML = `<p class="error">Failed to load plate planner: ${e.message}</p>`;
    return;
  }

  function renderSamplePool() {
      if (unassignedSamples.length === 0) {
          samplePoolList.innerHTML = '<tr><td colspan="3" style="text-align: center; padding: 15px; color: var(--text-secondary);">No unassigned samples available.</td></tr>';
          return;
      }
      
      samplePoolList.innerHTML = unassignedSamples.map(s => `
        <tr style="border-bottom: 1px solid var(--border-color); cursor: pointer;" class="pool-row">
            <td style="padding: 8px;"><input type="checkbox" class="pool-cb" value="${s.short_id || s.id}"></td>
            <td style="padding: 8px;">${s.short_id || s.id}</td>
            <td style="padding: 8px;">${s.sample_type}</td>
        </tr>
      `).join('');

      // Row click toggles checkbox
      document.querySelectorAll('.pool-row').forEach(row => {
          row.addEventListener('click', (e) => {
              if (e.target.type !== 'checkbox') {
                  const cb = row.querySelector('.pool-cb');
                  cb.checked = !cb.checked;
              }
          });
      });
  }

  selectAllPool.addEventListener('change', (e) => {
      document.querySelectorAll('.pool-cb').forEach(cb => cb.checked = e.target.checked);
  });

  function renderGrid(plate_format) {
    let rows, cols;
    wellGrid.className = 'planner-mode';
    if (plate_format.startsWith('24')) { [rows, cols] = [4, 6]; wellGrid.classList.add('density-low'); }
    else if (plate_format.startsWith('48')) { [rows, cols] = [6, 8]; wellGrid.classList.add('density-low'); }
    else if (plate_format.startsWith('96')) { [rows, cols] = [8, 12]; wellGrid.classList.add('density-medium'); }
    else if (plate_format.startsWith('384')) { [rows, cols] = [16, 24]; wellGrid.classList.add('density-high'); }
    else { [rows, cols] = [0, 0]; }
    
    wellGrid.style.gridTemplateRows = `repeat(${rows}, 1fr)`;
    wellGrid.style.gridTemplateColumns = `repeat(${cols}, 1fr)`;
    wellGrid.style.gridAutoFlow = 'column';
    wellGrid.style.userSelect = 'none'; // Prevent text selection while dragging

    wellGrid.innerHTML = wells.map(item => {
      const well = item.well;
      const isEdge = item.is_edge;
      return `
        <div class="well ${well.sample_id ? 'assigned' : 'selectable'}" 
             data-coordinate="${well.coordinate}">
          <div class="well-coord">${well.coordinate}</div>
          ${well.sample_id ? `<div class="well-sample-id" style="font-size: 0.7em;">Assigned</div>` : ''}
        </div>`;
    }).join('');

    attachDragListeners();
  }

  function attachDragListeners() {
      const allWells = document.querySelectorAll('.well');
      
      allWells.forEach(well => {
          const handleInteract = () => {
              if (currentTool === 'assign') {
                  if (!well.classList.contains('assigned') && !well.classList.contains('is-control')) {
                      well.classList.toggle('selected');
                  }
              } else if (currentTool === 'unassign') {
                  if (well.classList.contains('assigned')) {
                      well.classList.toggle('selected');
                  }
              } else if (currentTool === 'control') {
                  if (!well.classList.contains('assigned')) {
                      well.classList.toggle('is-control');
                      well.classList.remove('selected');
                  }
              }
              updateSelectionStatus();
          };

          well.addEventListener('mousedown', (e) => {
              isDragging = true;
              handleInteract();
          });
          
          well.addEventListener('mouseenter', (e) => {
              if (isDragging) {
                  handleInteract();
              }
          });
      });

      document.addEventListener('mouseup', () => {
          isDragging = false;
      });
  }

  function updateSelectionStatus() {
      const count = document.querySelectorAll('.well.selected').length;
      let msg = "0 wells selected";
      
      if (currentTool === 'assign') msg = `${count} empty well${count !== 1 ? 's' : ''} selected`;
      else if (currentTool === 'unassign') msg = `${count} assigned well${count !== 1 ? 's' : ''} selected`;
      else if (currentTool === 'control') msg = `Masking controls...`;
      
      selectionStatus.textContent = msg;
      if (count > 0) {
          selectionStatus.style.color = 'var(--color-accent)';
          selectionStatus.style.fontWeight = 'bold';
      } else {
          selectionStatus.style.color = '';
          selectionStatus.style.fontWeight = '';
      }
  }

  autoFillBtn.addEventListener('click', async () => {
      const selectedWellDivs = Array.from(document.querySelectorAll('.well.selected'));
      const selectedWellCoords = selectedWellDivs.map(el => el.dataset.coordinate);

      if (selectedWellDivs.length === 0) return UIUtils.showToast("Please highlight wells on the plate first.", "error");

      let endpoint, method, payload;

      if (currentTool === 'unassign') {
          if (!confirm(`Are you sure you want to unassign ${selectedWellCoords.length} wells?`)) return;
          
          endpoint = `/api/v1/plates/${plateId}/unassign-bulk`;
          method = 'POST';
          payload = { wells: selectedWellCoords };
          
          autoFillBtn.disabled = true;
          autoFillBtn.textContent = "Unassigning...";
      } else {
          const checkedBoxes = Array.from(document.querySelectorAll('.pool-cb:checked'));
          if (checkedBoxes.length === 0) return UIUtils.showToast("Please select samples from the Source Pool.", "error");

          const selectedSampleShortIds = checkedBoxes.map(cb => cb.value);
          const strategy = strategySelect.value;

          endpoint = `/api/v1/plates/${plateId}/auto-fill`;
          method = 'POST';
          payload = {
              wells: selectedWellCoords,
              sample_short_ids: selectedSampleShortIds,
              strategy: strategy
          };
          
          autoFillBtn.disabled = true;
          autoFillBtn.textContent = "Processing...";
      }

      try {
          const res = await fetch(endpoint, {
              method: method,
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify(payload)
          });

          if (res.ok) {
              window.location.reload();
          } else {
              const err = await res.json();
              UIUtils.showToast(`Action failed: ${err.error}`, "error");
          }
      } catch (e) {
          console.error(e);
          UIUtils.handleError(error, "Network error");
      } finally {
          autoFillBtn.disabled = false;
          autoFillBtn.textContent = currentTool === 'unassign' ? "Unassign Selected" : "Auto-Fill Selected";
      }
  });
}
