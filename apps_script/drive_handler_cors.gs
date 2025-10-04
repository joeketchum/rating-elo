const SHEET_ID = '1dMiPZqpcj7sMr9aKMxNhWKQNc2vzcJJD'; // You'll need to create a Google Sheet and update this ID

function doGet(e) {
  try {
    const sheet = SpreadsheetApp.openById(SHEET_ID);
    const dataSheet = sheet.getSheetByName('Data') || sheet.getSheets()[0];
    
    // Get the JSON data from cell A1
    const jsonData = dataSheet.getRange('A1').getValue();
    const lastModified = dataSheet.getRange('B1').getValue();
    
    const response = {
      data: jsonData || '{}',
      lastModified: lastModified ? lastModified.toISOString() : new Date().toISOString()
    };
    
    const output = ContentService.createTextOutput(JSON.stringify(response));
    output.setMimeType(ContentService.MimeType.JSON);
    return output;
  } catch (err) {
    const output = ContentService.createTextOutput(JSON.stringify({ 
      ok: false, 
      error: String(err),
      data: '{}',
      lastModified: new Date().toISOString()
    }));
    output.setMimeType(ContentService.MimeType.JSON);
    return output;
  }
}

function doPost(e) {
  try {
    const body = e.postData ? e.postData.contents : null;
    if (!body) {
      const output = ContentService.createTextOutput(JSON.stringify({ ok: false, error: 'empty body' }));
      output.setMimeType(ContentService.MimeType.JSON);
      return output;
    }

    const sheet = SpreadsheetApp.openById(SHEET_ID);
    const dataSheet = sheet.getSheetByName('Data') || sheet.getSheets()[0];
    
    // Store JSON data in A1, timestamp in B1
    const now = new Date();
    dataSheet.getRange('A1').setValue(body);
    dataSheet.getRange('B1').setValue(now);
    
    // Log the change in a history sheet (optional)
    const historySheet = sheet.getSheetByName('History') || sheet.insertSheet('History');
    if (historySheet.getLastRow() === 0) {
      historySheet.getRange('1:1').setValues([['Timestamp', 'Data Size', 'User']]);
    }
    historySheet.appendRow([now, body.length, 'Web App']);

    const result = { 
      ok: true, 
      bytes: body.length, 
      ts: now.toISOString(),
      lastModified: now.toISOString()
    };
    
    const output = ContentService.createTextOutput(JSON.stringify(result));
    output.setMimeType(ContentService.MimeType.JSON);
    
    return output;
  } catch (err) {
    console.error('Write error:', err);
    const output = ContentService.createTextOutput(JSON.stringify({ 
      ok: false, 
      error: String(err)
    }));
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