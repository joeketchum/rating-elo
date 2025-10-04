function doGet() {
  const FILE_ID = '1dMiPZqpcj7sMr9aKMxNhWKQNc2vzcJJD';
  
  try {
    const file = DriveApp.getFileById(FILE_ID);
    const content = file.getBlob().getDataAsString();
    const jsonData = JSON.parse(content);
    
    return createCorsResponse(jsonData);
  } catch (error) {
    console.error('Error reading file:', error);
    return createCorsResponse({error: 'Failed to read data', details: error.toString()});
  }
}

function doPost(e) {
  const FILE_ID = '1dMiPZqpcj7sMr9aKMxNhWKQNc2vzcJJD';
  
  try {
    const jsonData = JSON.parse(e.postData.contents);
    const jsonString = JSON.stringify(jsonData, null, 2);
    
    const file = DriveApp.getFileById(FILE_ID);
    const blob = Utilities.newBlob(jsonString, 'application/json', 'data.json');
    file.setContent(blob.getDataAsString());
    
    const response = {
      ok: true,
      bytes: jsonString.length,
      ts: new Date().toISOString(),
      fileId: FILE_ID
    };
    
    return createCorsResponse(response);
  } catch (error) {
    console.error('Error saving file:', error);
    return createCorsResponse({error: 'Failed to save data', details: error.toString()});
  }
}

function createCorsResponse(data) {
  const response = ContentService.createTextOutput(JSON.stringify(data))
    .setMimeType(ContentService.MimeType.JSON);
    
  // Add CORS headers
  response.setHeaders({
    'Access-Control-Allow-Origin': '*',
    'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
    'Access-Control-Allow-Headers': 'Content-Type, Authorization',
    'Access-Control-Max-Age': '86400'
  });
  
  return response;
}

function doOptions() {
  // Handle preflight requests
  return createCorsResponse({});
}

function testJsonFileAccess() {
  const FILE_ID = '1dMiPZqpcj7sMr9aKMxNhWKQNc2vzcJJD';
  try {
    const file = DriveApp.getFileById(FILE_ID);
    const content = file.getBlob().getDataAsString();
    console.log('File content:', content);
    return content;
  } catch (error) {
    console.error('Error:', error);
    return error.toString();
  }
}
