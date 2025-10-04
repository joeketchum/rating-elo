const FILE_ID = '1dMiPZqpcj7sMr9aKMxNhWKQNc2vzcJJD';

function doGet(e) {
  try {
    const f = DriveApp.getFileById(FILE_ID);
    const content = f.getBlob().getDataAsString();
    const output = ContentService.createTextOutput(content);
    output.setMimeType(ContentService.MimeType.JSON);
    return output;
  } catch (err) {
    const output = ContentService.createTextOutput(JSON.stringify({ ok: false, error: String(err) }));
    output.setMimeType(ContentService.MimeType.JSON);
    return output;
  }
}

function doPost(e) {
  // Add CORS headers first
  const headers = {
    'Access-Control-Allow-Origin': '*',
    'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
    'Access-Control-Allow-Headers': 'Content-Type'
  };
  
  try {
    const body = e.postData ? e.postData.contents : null;
    if (!body) {
      const output = ContentService.createTextOutput(JSON.stringify({ ok: false, error: 'empty body' }));
      output.setMimeType(ContentService.MimeType.JSON);
      Object.keys(headers).forEach(key => output.setHeader ? output.setHeader(key, headers[key]) : null);
      return output;
    }

    const f = DriveApp.getFileById(FILE_ID);
    f.setContent(body);

    const result = { ok: true, bytes: body.length, ts: new Date().toISOString() };
    const output = ContentService.createTextOutput(JSON.stringify(result));
    output.setMimeType(ContentService.MimeType.JSON);
    
    // Try to set CORS headers (Apps Script API varies)
    try {
      Object.keys(headers).forEach(key => output.setHeader(key, headers[key]));
    } catch (e) {
      // Fallback - some Apps Script versions don't support setHeader
      console.log('Could not set headers:', e);
    }
    
    return output;
  } catch (err) {
    const output = ContentService.createTextOutput(JSON.stringify({ ok: false, error: String(err) }));
    output.setMimeType(ContentService.MimeType.JSON);
    return output;
  }
}

function doOptions(e) {
  const output = ContentService.createTextOutput('');
  output.setMimeType(ContentService.MimeType.TEXT);
  
  // Try to set CORS headers
  try {
    output.setHeader('Access-Control-Allow-Origin', '*');
    output.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
    output.setHeader('Access-Control-Allow-Headers', 'Content-Type');
  } catch (e) {
    console.log('Could not set headers:', e);
  }
  
  return output;
}