/* src/server/static/js/print-service.js */

window.PrintService = {
  renderPreview: function (type, items) {
    if (!items || items.length === 0) return;

    // Create modal elements dynamically so we don't have to embed HTML on every page
    const modal = document.createElement('div');
    modal.className = 'modal';
    modal.style.display = 'block';

    const content = document.createElement('div');
    content.className = 'modal-content';
    content.style.maxWidth = '400px';

    const closeBtn = document.createElement('span');
    closeBtn.className = 'close-button';
    closeBtn.innerHTML = '&times;';

    const title = document.createElement('h3');
    title.textContent = 'Print Labels';

    const desc = document.createElement('p');
    desc.textContent = `Printing ${items.length} label(s). What should be printed on the text label?`;

    const selectWrapper = document.createElement('div');
    selectWrapper.style.display = 'flex';
    selectWrapper.style.flexDirection = 'column';
    selectWrapper.style.gap = '10px';
    selectWrapper.style.marginTop = '15px';
    selectWrapper.innerHTML = `
        <div style="display: flex; flex-direction: column; gap: 4px;">
            <label style="font-size: 12px; font-weight: bold; color: var(--text-muted, #888);">Label Format</label>
            <div class="select-wrapper" style="width: 100%;">
                <select id="print-label-format-select" style="width: 100%;">
                    <option value="linear_strip">1.5" x 0.25" Linear Barcode / Text Strip</option>
                    <option value="cryo_combo" selected>Cryo Vial Combo (Circle Data Matrix + Text)</option>
                </select>
            </div>
        </div>
        <div style="display: flex; flex-direction: column; gap: 4px;">
            <label style="font-size: 12px; font-weight: bold; color: var(--text-muted, #888);">Text Content</label>
            <div class="select-wrapper" style="width: 100%;">
                <select id="print-label-content-select" style="width: 100%;">
                    <option value="short_id">Short ID (e.g. ${items[0].short_id})</option>
                    <option value="name">Name (e.g. ${items[0].name || 'Unknown'})</option>
                    <option value="date">Date</option>
                </select>
            </div>
        </div>
        <div id="print-custom-date-container" style="display: none; width: 100%;">
            <input type="date" id="print-custom-date" style="width: 100%; box-sizing: border-box; padding: 8px; border: 1px solid var(--border-color); border-radius: 4px; background: var(--bg-dark); color: var(--text-color);">
        </div>
    `;

    const toggleDiv = document.createElement('div');
    toggleDiv.id = 'print-toggles-container';
    toggleDiv.style.marginTop = '1rem';
    toggleDiv.style.display = 'flex';
    toggleDiv.style.gap = '1.5rem';
    toggleDiv.innerHTML = `
        <label style="display:flex;align-items:center;gap:0.5rem;cursor:pointer;">
            <input type="checkbox" id="print-barcode" checked> Barcode
        </label>
        <label style="display:flex;align-items:center;gap:0.5rem;cursor:pointer;">
            <input type="checkbox" id="print-text" checked> Text Label
        </label>
    `;

    const actionDiv = document.createElement('div');
    actionDiv.style.marginTop = '1.5rem';
    actionDiv.style.display = 'flex';
    actionDiv.style.justifyContent = 'flex-end';
    actionDiv.style.gap = '10px';

    const cancelBtn = document.createElement('button');
    cancelBtn.className = 'button secondary-button';
    cancelBtn.textContent = 'Cancel';

    const confirmBtn = document.createElement('button');
    confirmBtn.className = 'button';
    confirmBtn.textContent = 'Print';

    actionDiv.appendChild(cancelBtn);
    actionDiv.appendChild(confirmBtn);

    content.appendChild(closeBtn);
    content.appendChild(title);
    content.appendChild(desc);
    content.appendChild(selectWrapper);
    content.appendChild(toggleDiv);
    content.appendChild(actionDiv);
    modal.appendChild(content);
    document.body.appendChild(modal);

    const formatSelectEl = document.getElementById('print-label-format-select');
    const toggleContainer = document.getElementById('print-toggles-container');
    const selectEl = document.getElementById('print-label-content-select');
    const dateContainer = document.getElementById('print-custom-date-container');
    const dateInput = document.getElementById('print-custom-date');
    
    formatSelectEl.onchange = (e) => {
        if (e.target.value === 'cryo_combo') {
            toggleContainer.style.display = 'none';
        } else {
            toggleContainer.style.display = 'flex';
        }
    };
    // Initialize toggles display based on default selection
    if (formatSelectEl.value === 'cryo_combo') {
        toggleContainer.style.display = 'none';
    }

    selectEl.onchange = (e) => {
        if (e.target.value === 'date') {
            dateContainer.style.display = 'block';
            
            // Set default date to local timezone date YYYY-MM-DD
            const today = new Date();
            const year = today.getFullYear();
            const month = String(today.getMonth() + 1).padStart(2, '0');
            const day = String(today.getDate()).padStart(2, '0');
            dateInput.value = `${year}-${month}-${day}`;
        } else {
            dateContainer.style.display = 'none';
        }
    };

    const cleanup = () => {
        document.body.removeChild(modal);
    };

    closeBtn.onclick = cleanup;
    cancelBtn.onclick = cleanup;
    window.onclick = (e) => {
        if (e.target === modal) cleanup();
    };

    confirmBtn.onclick = () => {
        const labelFormat = document.getElementById('print-label-format-select').value;
        const labelValue = document.getElementById('print-label-content-select').value;
        const showBarcode = document.getElementById('print-barcode').checked;
        const showText = document.getElementById('print-text').checked;
        
        let customDateValue = '';
        const dateInputEl = document.getElementById('print-custom-date');
        if (dateInputEl) {
            customDateValue = dateInputEl.value;
        }
        
        cleanup();
        
        // Execute the actual print logic
        const iframe = document.createElement('iframe');
        iframe.style.position = 'fixed';
        iframe.style.right = '0';
        iframe.style.bottom = '0';
        iframe.style.width = '0';
        iframe.style.height = '0';
        iframe.style.border = 'none';
        document.body.appendChild(iframe);

        const doc = iframe.contentWindow.document;

        const isCryoCombo = labelFormat === 'cryo_combo';

        const style = isCryoCombo ? `
            <style>
                @page { size: 38mm 9.5mm; margin: 0; }
                body { margin: 0; padding: 0; background: white; font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif; }
                .combo-container {
                    width: 38mm;
                    height: 9.5mm;
                    display: flex;
                    flex-direction: row;
                    align-items: center;
                    page-break-after: always;
                    overflow: hidden;
                    box-sizing: border-box;
                    padding-left: 1.0mm;
                }
                .cap-circle-zone {
                    width: 9.5mm;
                    height: 9.5mm;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    flex-shrink: 0;
                }
                .cap-circle-zone canvas {
                    width: 6.2mm;
                    height: 6.2mm;
                    image-rendering: pixelated;
                }
                .inter-label-gap {
                    width: 2.2mm;
                    flex-shrink: 0;
                }
                .body-rect-zone {
                    width: 24.8mm;
                    height: 9.5mm;
                    display: flex;
                    flex-direction: column;
                    justify-content: center;
                    align-items: flex-start;
                    overflow: hidden;
                    box-sizing: border-box;
                    padding-left: 0.8mm;
                    padding-right: 0.4mm;
                }
                .rect-text-primary {
                    font-weight: bold;
                    line-height: 1.15;
                    word-break: break-word;
                    overflow: hidden;
                    max-width: 100%;
                }
                .rect-text-secondary {
                    font-size: 6.5pt;
                    line-height: 1.1;
                    color: #222;
                    white-space: nowrap;
                    overflow: hidden;
                    text-overflow: ellipsis;
                    max-width: 100%;
                }
            </style>
        ` : `
            <style>
                @page { size: 1.5in 0.25in; margin: 0; }
                body { margin: 0; padding: 0; background: white; font-family: sans-serif; }
                .label-container { 
                    width: 1.5in; 
                    height: 0.25in; 
                    display: flex; 
                    align-items: center; 
                    justify-content: center;
                    page-break-after: always;
                    overflow: hidden;
                    text-align: center;
                }
                /* Barcode specific sizing */
                canvas { 
                    max-width: 1.4in; 
                    max-height: 0.2in; 
                }
                /* Plain text specific sizing */
                .text-label-id {
                    font-size: 14pt; 
                    font-weight: bold;
                    letter-spacing: 1px;
                }
                .text-label-name {
                    font-size: 10pt; 
                    font-weight: bold;
                    white-space: nowrap;
                    overflow: hidden;
                    text-overflow: ellipsis;
                    max-width: 100%;
                    padding: 0 4px;
                }
            </style>
        `;

        doc.open();
        doc.write(`<html><head>${style}</head><body></body></html>`);
        doc.close();

        const body = doc.body;

        const getTextContent = (item) => {
          if (labelValue === 'name') {
            return item.name || 'Unknown';
          } else if (labelValue === 'date') {
            if (customDateValue) return customDateValue;
            const today = new Date();
            const year = today.getFullYear();
            const month = String(today.getMonth() + 1).padStart(2, '0');
            const day = String(today.getDate()).padStart(2, '0');
            return `${year}-${month}-${day}`;
          }
          return item.short_id || item.id || '';
        };

        if (isCryoCombo) {
          items.forEach((item) => {
            const pageDiv = doc.createElement('div');
            pageDiv.className = 'combo-container';

            // Circle zone (Data Matrix)
            const circleDiv = doc.createElement('div');
            circleDiv.className = 'cap-circle-zone';

            const barcodeText = String(item.short_id || item.id || '');
            const bwip = window.bwipjs || (typeof bwipjs !== 'undefined' ? bwipjs : null);

            if (bwip && barcodeText) {
              try {
                // Create off-screen canvas in main window context
                const tempCanvas = document.createElement('canvas');
                bwip.toCanvas(tempCanvas, {
                  bcid: 'datamatrix',
                  text: barcodeText,
                  scale: 4,
                  includetext: false,
                  rotate: 'N'
                });
                
                // Convert canvas to image for rock-solid iframe print rendering
                const img = doc.createElement('img');
                img.src = tempCanvas.toDataURL('image/png');
                img.style.width = '6.2mm';
                img.style.height = '6.2mm';
                img.style.display = 'block';
                circleDiv.appendChild(img);
              } catch (e) {
                console.error('Error generating Data Matrix with bwip-js:', e);
              }
            } else {
              console.warn('bwip-js library not detected on window:', bwip);
            }

            // Gap
            const gapDiv = doc.createElement('div');
            gapDiv.className = 'inter-label-gap';

            // Rectangle zone (Text)
            const rectDiv = doc.createElement('div');
            rectDiv.className = 'body-rect-zone';

            const primarySpan = doc.createElement('div');
            primarySpan.className = 'rect-text-primary';

            if (labelValue === 'name') {
              const name = item.name || 'Unknown';
              const len = name.length;
              // Scale font size according to length and allow multi-line wrapping
              const fontSize = len > 45 ? '5pt' : len > 30 ? '5.5pt' : len > 18 ? '6.5pt' : len > 10 ? '7.5pt' : '8.5pt';
              primarySpan.style.fontSize = fontSize;
              primarySpan.textContent = name;
              rectDiv.appendChild(primarySpan);
              // When printing 'name', omit redundant secondary short_id
            } else if (labelValue === 'date') {
              primarySpan.style.fontSize = '8pt';
              primarySpan.textContent = getTextContent(item);
              rectDiv.appendChild(primarySpan);

              // Keep short_id as secondary line when printing Date
              if (item.short_id) {
                const secondarySpan = doc.createElement('div');
                secondarySpan.className = 'rect-text-secondary';
                secondarySpan.textContent = item.short_id;
                rectDiv.appendChild(secondarySpan);
              }
            } else {
              // 'short_id'
              primarySpan.style.fontSize = '9pt';
              primarySpan.textContent = item.short_id || item.id || '';
              rectDiv.appendChild(primarySpan);
            }

            pageDiv.appendChild(circleDiv);
            pageDiv.appendChild(gapDiv);
            pageDiv.appendChild(rectDiv);
            body.appendChild(pageDiv);
          });
        } else {
          items.forEach((item) => {
            // Label 1 - barcode
            if (showBarcode) {
              const barcodeDiv = doc.createElement('div');
              barcodeDiv.className = 'label-container';
              const canvas = doc.createElement('canvas');
              barcodeDiv.appendChild(canvas);
              body.appendChild(barcodeDiv);

              window.JsBarcode(canvas, item.short_id, {
                format: "CODE128",
                width: 1,
                height: 20,
                displayValue: false,
                margin: 0
              });
            }

            // Label 2 - plain text
            if (showText) {
              const textDiv = doc.createElement('div');
              textDiv.className = 'label-container';
              const span = doc.createElement('span');
              if (labelValue === 'name') {
                const name = item.name || 'Unknown';
                const len = name.length;
                const fontSize = len > 20 ? '7pt' : len > 14 ? '8pt' : len > 10 ? '9pt' : '10pt';
                span.style.fontSize = fontSize;
                span.textContent = name;
              } else if (labelValue === 'date') {
                span.className = 'text-label-id';
                span.textContent = getTextContent(item);
              } else {
                span.className = 'text-label-id';
                span.textContent = item.short_id || item.id;
              }
              textDiv.appendChild(span);
              body.appendChild(textDiv);
            }
          });
        }

        setTimeout(() => {
          iframe.contentWindow.focus();
          iframe.contentWindow.print();
          setTimeout(() => document.body.removeChild(iframe), 1000);
        }, 200);
    };
  }
};
