function doGet(e) {
  const FILE_ID = '1dMiPZqpcj7sMr9aKMxNhWKQNc2vzcJJD';
  
  try {
    const file = DriveApp.getFileById(FILE_ID);
    const content = file.getBlob().getDataAsString();
    const jsonData = JSON.parse(content);
    
    const output = ContentService.createTextOutput(JSON.stringify(jsonData));
    output.setMimeType(ContentService.MimeType.JSON);
    return output;
  } catch (error) {
    console.error('Error reading file:', error);
    const errorResponse = {error: 'Failed to read data', details: error.toString()};
    const output = ContentService.createTextOutput(JSON.stringify(errorResponse));
    output.setMimeType(ContentService.MimeType.JSON);
    return output;
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
    
    const output = ContentService.createTextOutput(JSON.stringify(response));
    output.setMimeType(ContentService.MimeType.JSON);
    return output;
  } catch (error) {
    console.error('Error saving file:', error);
    const errorResponse = {error: 'Failed to save data', details: error.toString()};
    const output = ContentService.createTextOutput(JSON.stringify(errorResponse));
    output.setMimeType(ContentService.MimeType.JSON);
    return output;
  }
}
