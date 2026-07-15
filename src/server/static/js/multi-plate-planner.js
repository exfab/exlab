/* src/server/static/js/run-generator.js */

window.MultiPlatePlanner = {
  openWizard: async function (projectId) {
    const modalHtml = `
        <div id="multiPlatePlannerWizard" class="modal">
            <div class="modal-content" style="width: 95vw; max-width: 1600px; height: 95vh; max-height: 95vh; display: flex; flex-direction: column; padding: 30px;">
                <span class="close-button" onclick="document.getElementById('multiPlatePlannerWizard').remove()" style="font-size: 2rem;">&times;</span>
                <h2 style="margin-top: 0;">Plan Multiple Plates</h2>
                
                <div style="display: flex; gap: 40px; flex-grow: 1; overflow: hidden; margin-top: 20px;">
                    <!-- LEFT PANE: Settings & Grid -->
                    <div style="flex: 2; overflow-y: auto; padding-right: 20px; display: flex; flex-direction: column;">
                        <div class="form-container" style="display: flex; gap: 25px; margin-bottom: 25px;">
                            <div class="input-group" style="flex: 2;">
                                <label style="font-size: 1.1em; margin-bottom: 8px;">Plate Name Prefix</label>
                                <input type="text" id="rg-prefix" placeholder="e.g. Exp Alpha" style="padding: 10px; font-size: 1.1em;">
                            </div>
                            
                            <div class="input-group" style="flex: 1.5;">
                                <label style="font-size: 1.1em; margin-bottom: 8px;">Plate Size</label>
                                <div class="select-wrapper">
                                    <select id="rg-format" style="padding: 10px; font-size: 1.1em;">
                                        <option value="24-well">24-well</option>
                                        <option value="48-well">48-well</option>
                                        <option value="96-well" selected>96-well</option>
                                        <option value="384-well">384-well</option>
                                    </select>
                                </div>
                            </div>
                        </div>

                        <div class="form-container" style="display: flex; gap: 25px; margin-bottom: 25px;">
                            <div class="input-group" style="flex: 1;">
                                <label style="font-size: 1.1em; margin-bottom: 8px;">Replicates</label>
                                <input type="number" id="rg-replicates" value="3" min="1" step="1" title="Replicates per plate (for shuffled) or total replicates (for round robin)" style="padding: 10px; font-size: 1.1em;">
                            </div>
                            
                            <div class="input-group" style="flex: 1;">
                                <label style="font-size: 1.1em; margin-bottom: 8px;">Plates</label>
                                <input type="number" id="rg-num-plates" value="1" min="1" step="1" title="Number of plates to generate (for shuffled)" style="padding: 10px; font-size: 1.1em;">
                            </div>
                            
                            <div class="input-group" style="flex: 1;">
                                <label style="font-size: 1.1em; margin-bottom: 8px;">Blanks</label>
                                <input type="number" id="rg-num-blanks" value="0" min="0" step="1" title="Number of free-floating blanks per plate" style="padding: 10px; font-size: 1.1em;">
                            </div>
                        </div>
                        
                        <div class="form-container" style="margin-bottom: 25px;">
                            <label style="font-size: 1.1em; margin-bottom: 8px;">Distribution Strategy</label>
                            <div class="select-wrapper">
                                <select id="rg-strategy" style="padding: 10px; font-size: 1.1em;">
                                    <option value="simple">Shuffled (Simple Edge Minimization)</option>
                                    <option value="neighbor_aware">Shuffled (Neighbor Aware)</option>
                                    <option value="round_robin">Round Robin (Sequential Fill)</option>
                                </select>
                            </div>
                        </div>
                        
                        <div style="margin-bottom: 15px;">
                            <label style="font-weight: bold; display: block; margin-bottom: 8px; font-size: 1.2em;">Reserved Wells (Blanks/Controls)</label>
                            <p class="text-secondary" style="font-size: 1em; margin: 0;">Drag or click wells below to mask them as controls. They will be left empty on every generated plate.</p>
                        </div>
                        
                        <div class="plate-stage planner-mode" style="padding: 25px; border-radius: 8px; background: var(--bg-dark-2); border: 1px solid var(--border-color); flex-grow: 1; display: flex; align-items: flex-start; justify-content: center; overflow: auto; min-height: 400px;">
                            <div class="plate-frame" style="margin: 0 auto; transform: scale(1.1); transform-origin: top center;">
                                <div id="rg-well-grid"></div>
                            </div>
                        </div>
                        
                        <button id="rg-submit-btn" class="button" style="width: 100%; margin-top: 25px; padding: 15px; font-size: 1.3em; background-color: #28a745; border-color: #28a745;">Plan Plates</button>
                    </div>
                    
                    <!-- RIGHT PANE: Source Samples -->
                    <div style="flex: 1; display: flex; flex-direction: column; border-left: 1px solid var(--border-color); padding-left: 25px;">
                        <h3 style="margin-top: 0; margin-bottom: 15px;">Select Source Samples</h3>
                        <div style="flex-grow: 1; overflow-y: auto; border: 1px solid var(--border-color); background: var(--bg-dark); padding: 15px; border-radius: 4px;">
                            <table style="width: 100%; border-collapse: collapse; text-align: left; font-size: 1.1em;">
                                <thead>
                                    <tr style="border-bottom: 2px solid var(--border-color);">
                                        <th style="padding: 12px;"><input type="checkbox" id="rg-select-all" style="transform: scale(1.2);"></th>
                                        <th style="padding: 12px;">Short ID</th>
                                        <th style="padding: 12px;">Type</th>
                                    </tr>
                                </thead>
                                <tbody id="rg-sample-list">
                                    <tr><td colspan="3" style="text-align: center; padding: 20px;">Loading samples...</td></tr>
                                </tbody>
                            </table>
                        </div>
                        <p id="rg-sample-count" class="text-secondary" style="font-size: 1.1em; margin-top: 15px; font-weight: bold;">0 selected</p>
                    </div>
                </div>
            </div>
        </div>`;

    document.body.insertAdjacentHTML('beforeend', modalHtml);
    
    // Fetch project samples
    try {
        const res = await fetch(`/api/v1/projects/${projectId}/samples`);
        if (res.ok) {
            const data = await res.json();
            const sourceSamples = data.data.filter(s => s.category === 'Source' && s.status !== 'Archived');
            const listEl = document.getElementById('rg-sample-list');
            
            if (sourceSamples.length === 0) {
                listEl.innerHTML = '<tr><td colspan="3" style="text-align: center; padding: 15px; color: var(--text-secondary);">No Source samples available in this project.</td></tr>';
            } else {
                listEl.innerHTML = sourceSamples.map(s => `
                    <tr style="border-bottom: 1px solid var(--border-color); cursor: pointer;" class="rg-row">
                        <td style="padding: 12px;"><input type="checkbox" class="rg-cb" value="${s.short_id || s.id}" style="transform: scale(1.2);"></td>
                        <td style="padding: 12px;">${s.short_id || s.id}</td>
                        <td style="padding: 12px;">${s.sample_type}</td>
                    </tr>
                `).join('');

                // Setup row click
                document.querySelectorAll('.rg-row').forEach(row => {
                    row.addEventListener('click', (e) => {
                        if (e.target.type !== 'checkbox') {
                            const cb = row.querySelector('.rg-cb');
                            cb.checked = !cb.checked;
                        }
                        updateCount();
                    });
                });
                
                document.querySelectorAll('.rg-cb').forEach(cb => {
                    cb.addEventListener('change', updateCount);
                });
            }
        }
    } catch (e) {
        document.getElementById('rg-sample-list').innerHTML = '<tr><td colspan="3" style="text-align: center; padding: 15px; color: red;">Failed to load samples.</td></tr>';
    }

    const selectAll = document.getElementById('rg-select-all');
    selectAll.addEventListener('change', (e) => {
        document.querySelectorAll('.rg-cb').forEach(cb => cb.checked = e.target.checked);
        updateCount();
    });

    function updateCount() {
        const count = document.querySelectorAll('.rg-cb:checked').length;
        document.getElementById('rg-sample-count').textContent = `${count} selected`;
    }

    const rgWellGrid = document.getElementById('rg-well-grid');
    const rgFormatSelect = document.getElementById('rg-format');
    let isDragging = false;

    function generateCoords(format) {
        let rows, cols;
        if (format.startsWith('24')) { [rows, cols] = [4, 6]; }
        else if (format.startsWith('48')) { [rows, cols] = [6, 8]; }
        else if (format.startsWith('96')) { [rows, cols] = [8, 12]; }
        else if (format.startsWith('384')) { [rows, cols] = [16, 24]; }
        else { [rows, cols] = [0, 0]; }

        const coords = [];
        for (let r = 0; r < rows; r++) {
            const rowChar = String.fromCharCode(65 + r);
            for (let c = 1; c <= cols; c++) {
                coords.push(`${rowChar}${c}`);
            }
        }
        return { rows, cols, coords };
    }

    function renderGrid() {
        const format = rgFormatSelect.value;
        const { rows, cols, coords } = generateCoords(format);
        
        rgWellGrid.className = '';
        if (format.startsWith('24')) rgWellGrid.classList.add('density-low');
        else if (format.startsWith('48')) rgWellGrid.classList.add('density-low');
        else if (format.startsWith('96')) rgWellGrid.classList.add('density-medium');
        else if (format.startsWith('384')) rgWellGrid.classList.add('density-high');
        
        rgWellGrid.style.gridTemplateRows = `repeat(${rows}, 1fr)`;
        rgWellGrid.style.gridTemplateColumns = `repeat(${cols}, 1fr)`;
        rgWellGrid.style.gridAutoFlow = 'row';
        rgWellGrid.style.userSelect = 'none';

        rgWellGrid.innerHTML = coords.map(c => `
            <div class="well selectable" data-coordinate="${c}">
                <div class="well-coord">${c}</div>
            </div>
        `).join('');

        attachDragListeners();
    }

    function attachDragListeners() {
        const wells = document.querySelectorAll('#rg-well-grid .well');
        wells.forEach(well => {
            const toggleControl = () => {
                well.classList.toggle('is-control');
            };

            well.addEventListener('mousedown', (e) => {
                isDragging = true;
                toggleControl();
            });
            well.addEventListener('mouseenter', (e) => {
                if (isDragging) toggleControl();
            });
        });
        document.addEventListener('mouseup', () => { isDragging = false; });
    }

    rgFormatSelect.addEventListener('change', renderGrid);
    renderGrid(); // Initial render

    const submitBtn = document.getElementById('rg-submit-btn');
    submitBtn.addEventListener('click', async () => {
        const prefix = document.getElementById('rg-prefix').value.trim();
        const replicates = parseInt(document.getElementById('rg-replicates').value, 10);
        const numPlates = parseInt(document.getElementById('rg-num-plates').value, 10);
        const numBlanks = parseInt(document.getElementById('rg-num-blanks').value, 10);
        const format = document.getElementById('rg-format').value;
        const strategy = document.getElementById('rg-strategy').value;
        
        const checkedBoxes = Array.from(document.querySelectorAll('.rg-cb:checked'));
        if (checkedBoxes.length === 0) return UIUtils.showToast("Please select at least one Source sample.", "error");
        if (!prefix) return UIUtils.showToast("Please enter a plate name prefix.", "error");
        if (isNaN(replicates) || replicates < 1) return UIUtils.showToast("Replicates must be a valid number >= 1.", "error");
        if (isNaN(numPlates) || numPlates < 1) return UIUtils.showToast("Plates must be a valid number >= 1.", "error");
        if (isNaN(numBlanks) || numBlanks < 0) return UIUtils.showToast("Blanks must be a valid number >= 0.", "error");

        const sourceIds = checkedBoxes.map(cb => cb.value);
        
        // Grab reserved wells directly from the visual grid!
        const reservedDivs = Array.from(document.querySelectorAll('#rg-well-grid .well.is-control'));
        const reservedWells = reservedDivs.map(el => el.dataset.coordinate);

        const payload = {
            source_sample_short_ids: sourceIds,
            replicates: replicates,
            num_plates: numPlates,
            num_blanks: numBlanks,
            plate_name_prefix: prefix,
            plate_format: format,
            reserved_wells: reservedWells,
            strategy: strategy
        };

        submitBtn.disabled = true;
        submitBtn.textContent = "Generating...";

        try {
            const res = await fetch(`/api/v1/projects/${projectId}/plates/plan`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(payload)
            });

            if (res.ok) {
                UIUtils.showToast("Plates Planned Successfully!", "success");
                window.location.reload();
            } else {
                const err = await res.json();
                UIUtils.showToast(`Generation failed: ${err.error}`, "error");
            }
        } catch (e) {
            console.error(e);
            UIUtils.handleError(e, "Network error");
        } finally {
            submitBtn.disabled = false;
            submitBtn.textContent = "Plan Plates";
        }
    });
  }
};
