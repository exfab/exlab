/* src/server/static/js/api-utils.js */

const ApiUtils = {
    /**
     * Wrapper around fetch that automatically handles non-200 responses
     * by attempting to parse a JSON {error: "..."} payload.
     * 
     * @throws {Error} Throws an error with the server's message if the request fails.
     */
    async request(url, options = {}) {
        const response = await fetch(url, options);
        
        if (!response.ok) {
            let errorMsg = `HTTP Error ${response.status}`;
            try {
                const errData = await response.json();
                if (errData.error) errorMsg = errData.error;
            } catch (e) {
                // If it's not JSON, we stick with the default HTTP status message
            }
            throw new Error(errorMsg);
        }

        // Return empty object for 204 No Content
        if (response.status === 204) return {};
        
        return await response.json();
    },

    /**
     * Parses a string value from a UI input into the standardized JSON structure
     * required by the backend, validating against the provided data_type.
     * 
     * @param {string} value - The raw string value from the input.
     * @param {string} dataType - The Result Definition's data_type (e.g., 'Integer', 'TimeSeries:Float')
     * @returns {Object} The parsed value object {type: string, value: any}
     * @throws {Error} If the value cannot be parsed into the expected type.
     */
    parseResultInput(value, dataType) {
        let parsedValue = null;

        if (dataType.endsWith('Series')) {
            const arr = JSON.parse(value); // Throws if invalid JSON array
            if (!Array.isArray(arr)) throw new Error("TimeSeries must be a JSON array");
            
            const innerType = dataType.replace('Series', '');
            const val = arr.map(pt => {
                let v = pt[1];
                v = this._parsePrimitive(v, innerType);
                return { time: parseFloat(pt[0]), value: v };
            });
            parsedValue = { type: dataType, value: val };
        } else {
            const v = this._parsePrimitive(value, dataType);
            parsedValue = { type: dataType, value: v };
        }

        return parsedValue;
    },

    /**
     * Internal helper to parse primitives strictly.
     * @throws {Error} If parsing fails strict validation
     */
    _parsePrimitive(value, type) {
        if (value === null || value === undefined || value === '') {
            throw new Error(`Value cannot be empty for type ${type}`);
        }

        switch (type) {
            case 'Integer':
                const strVal = String(value);
                if (strVal.includes('.')) throw new Error('Invalid Integer: contains a decimal.');
                const intVal = parseInt(strVal, 10);
                if (isNaN(intVal)) throw new Error(`Invalid Integer: "${value}" is not a number.`);
                return intVal;
                
            case 'Float':
                const floatVal = parseFloat(value);
                if (isNaN(floatVal)) throw new Error(`Invalid Float: "${value}" is not a number.`);
                return floatVal;
                
            case 'Boolean':
                return ['true', '1', 'yes'].includes(String(value).toLowerCase());
                
            case 'Date':
            case 'Datetime':
                const d = new Date(value);
                if (isNaN(d.getTime())) throw new Error(`Invalid Date/Time: "${value}"`);
                return d.getTime() / 1000;
                
            default:
                return value; // String or FileLink
        }
    }
};

window.ApiUtils = ApiUtils;
