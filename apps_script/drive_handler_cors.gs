const FILE_ID = '1dMiPZqpcj7sMr9aKMxNhWKQNc2vzcJJD';

function corsWrap(callback) {
  // Generic wrapper that handles CORS and JSON response for any endpoint
  return (e) => {
    const output = ContentService.createTextOutput();
    
    // Add CORS headers to all responses FIRST
    output.setHeaders({
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type',
      'Access-Control-Max-Age': '3600'
    });
    
    // Handle preflight OPTIONS request
    if (e.parameter && e.parameter.method === 'options') {
      output.setContent('');
      output.setMimeType(ContentService.MimeType.TEXT);
      return output;
    }
    
    // Normal request - call the actual handler
    try {
      const result = callback(e);
      output.setContent(typeof result === 'string' ? result : JSON.stringify(result));
      output.setMimeType(ContentService.MimeType.JSON);
    } catch (err) {
      output.setContent(JSON.stringify({ ok: false, error: String(err) }));
      output.setMimeType(ContentService.MimeType.JSON);
    }
    
    return output;
  };
}

const doPost = corsWrap((e) => {
  try {
    const body = e?.postData?.contents;
    if (!body) return { ok: false, error: 'empty body' };

    const f = DriveApp.getFileById(FILE_ID);
    f.setContent(body);

    console.log('WROTE bytes:', body.length, 'at', new Date().toISOString());
    return { ok: true, bytes: body.length, ts: new Date().toISOString() };
  } catch (err) {
    console.error('WRITE ERROR:', err);
    return { ok: false, error: String(err) };
  }
});

const doGet = corsWrap((e) => {
  try {
    const f = DriveApp.getFileById(FILE_ID);
    return f.getBlob().getDataAsString();
  } catch (err) {
    return { ok: false, error: String(err) };
  }
});

// Handle OPTIONS requests for CORS preflight
const doOptions = corsWrap((e) => {
  return { ok: true, message: 'CORS preflight' };
});