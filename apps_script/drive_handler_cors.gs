const FILE_ID = '1dMiPZqpcj7sMr9aKMxNhWKQNc2vzcJJD';

function corsWrap(callback) {
  // Generic wrapper that handles CORS and JSON response for any endpoint
  return (e) => {
    const origin = e?.parameter?.origin || '*';
    const output = ContentService.createTextOutput();
    
    // Handle preflight OPTIONS request
    if (e?.parameter?.method === 'options') {
      output.setContent('');
      output.setMimeType(ContentService.MimeType.TEXT);
    } else {
      // Normal request - call the actual handler
      const result = callback(e);
      output.setContent(typeof result === 'string' ? result : JSON.stringify(result));
      output.setMimeType(ContentService.MimeType.JSON);
    }

    // Add CORS headers to all responses
    const headers = output.getHeaders() || {};
    headers['Access-Control-Allow-Origin'] = '*';
    headers['Access-Control-Allow-Methods'] = 'GET, POST, OPTIONS';
    headers['Access-Control-Allow-Headers'] = 'Content-Type';
    headers['Access-Control-Max-Age'] = '3600';
    output.setHeaders(headers);
    
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