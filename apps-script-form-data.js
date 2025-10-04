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
    // Handle both form data and direct JSON
    let jsonString;
    if (e.parameter && e.parameter.data) {
      // Form data submission
      jsonString = e.parameter.data;
    } else if (e.postData && e.postData.contents) {
      // Direct JSON submission
      jsonString = e.postData.contents;
    } else {
      throw new Error('No data received');
    }
    
    // Parse and re-stringify to validate JSON
    const jsonData = JSON.parse(jsonString);
    const formattedJsonString = JSON.stringify(jsonData, null, 2);
    
    const file = DriveApp.getFileById(FILE_ID);
    const blob = Utilities.newBlob(formattedJsonString, 'application/json', 'data.json');
    file.setContent(blob.getDataAsString());
    
    const response = {
      ok: true,
      bytes: formattedJsonString.length,
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

function testFormData() {
  // Test function to verify form data handling
  const testData = '{"players":[{"id":1,"name":"Test","rating":1200,"matches":1}],"ignored":[]}';
  const mockEvent = {
    parameter: {
      data: testData
    }
  };
  
  return doPost(mockEvent);
}
