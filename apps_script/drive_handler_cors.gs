const FILE_ID = '1dMiPZqpcj7sMr9aKMxNhWKQNc2vzcJJD';

function doOptions(e) {
  return ContentService.createTextOutput('')
    .setMimeType(ContentService.MimeType.TEXT)
    .setHeader('Access-Control-Allow-Origin', 'https://joeketchum.github.io')
    .setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
    .setHeader('Access-Control-Allow-Headers', 'Content-Type')
    .setHeader('Access-Control-Max-Age', '3600');
}

function doPost(e) {
  try {
    const body = e?.postData?.contents ?? '';
    if (!body) return respondWithCors({ ok:false, error:'empty body' });

    const f = DriveApp.getFileById(FILE_ID);
    f.setContent(body);

    console.log('WROTE bytes:', body.length, 'at', new Date().toISOString());
    return respondWithCors({ ok:true, bytes: body.length, ts: new Date().toISOString() });
  } catch (err) {
    console.error('WRITE ERROR:', err);
    return respondWithCors({ ok:false, error: String(err) });
  }
}

function doGet(e) {
  try {
    const f = DriveApp.getFileById(FILE_ID);
    const text = f.getBlob().getDataAsString();
    return ContentService.createTextOutput(text)
      .setMimeType(ContentService.MimeType.JSON)
      .setHeader('Access-Control-Allow-Origin', 'https://joeketchum.github.io');
  } catch (err) {
    return respondWithCors({ ok:false, error: String(err) });
  }
}

function respondWithCors(obj) {
  // Same as your respond() but adds CORS headers
  return ContentService.createTextOutput(JSON.stringify(obj))
    .setMimeType(ContentService.MimeType.JSON)
    .setHeader('Access-Control-Allow-Origin', 'https://joeketchum.github.io');
}