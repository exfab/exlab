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
        <div class="select-wrapper" style="width: 100%;">
            <select id="print-label-content-select" style="width: 100%;">
                <option value="short_id">Short ID (e.g. ${items[0].short_id})</option>
                <option value="name">Name (e.g. ${items[0].name || 'Unknown'})</option>
                <option value="date">Date</option>
            </select>
        </div>
        <div id="print-custom-date-container" style="display: none; width: 100%;">
            <input type="date" id="print-custom-date" style="width: 100%; box-sizing: border-box; padding: 8px; border: 1px solid var(--border-color); border-radius: 4px; background: var(--bg-dark); color: var(--text-color);">
        </div>
    `;

    const toggleDiv = document.createElement('div');
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

    const selectEl = document.getElementById('print-label-content-select');
    const dateContainer = document.getElementById('print-custom-date-container');
    const dateInput = document.getElementById('print-custom-date');
    
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

        const style = `
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
              
              if (customDateValue) {
                span.textContent = customDateValue;
              } else {
                const today = new Date();
                const year = today.getFullYear();
                const month = String(today.getMonth() + 1).padStart(2, '0');
                const day = String(today.getDate()).padStart(2, '0');
                span.textContent = `${year}-${month}-${day}`;
              }
            } else {
              span.className = 'text-label-id';
              span.textContent = item.short_id || item.id;
            }
            textDiv.appendChild(span);
            body.appendChild(textDiv);
          }
        });

        setTimeout(() => {
          iframe.contentWindow.focus();
          iframe.contentWindow.print();
          setTimeout(() => document.body.removeChild(iframe), 1000);
        }, 200);
    };
  }
};
