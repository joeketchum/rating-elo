// Apps Script (drive_handler.gs)
// Safe Drive write handler for rating-elo
// Replace FILE_ID below with the Drive file ID you want to manage
const FILE_ID = '1dMiPZqpcj7sMr9aKMxNhWKQNc2vzcJJD';

function _jsonResponse(obj) {
  return ContentService.createTextOutput(JSON.stringify(obj)).setMimeType(ContentService.MimeType.JSON);
}

function _isInteger(n) {
  return typeof n === 'number' && isFinite(n) && Math.floor(n) === n;
}

function _validatePlayer(p) {
  if (!p || typeof p !== 'object') return { ok: false, msg: 'Player not an object' };
  if (typeof p.name !== 'string' || p.name.trim() === '') return { ok: false, msg: 'Player missing name' };
  if (typeof p.rating !== 'number' || !_isInteger(p.rating)) return { ok: false, msg: 'Player.rating must be an integer' };
  if (typeof p.matches !== 'number' || !_isInteger(p.matches)) return { ok: false, msg: 'Player.matches must be an integer' };
  if (p.id !== undefined && (typeof p.id !== 'number' || !_isInteger(p.id))) return { ok: false, msg: 'Player.id must be an integer if present' };
  return { ok: true };
}

function _normalizeIncoming(rawObj) {
  var players = [];
  var ignored = [];

  if (rawObj && Array.isArray(rawObj.players)) {
    players = rawObj.players;
    if (Array.isArray(rawObj.ignored)) ignored = rawObj.ignored;
  } else if (rawObj && typeof rawObj === 'object') {
    var maybePlayers = [];
    for (var k in rawObj) {
      if (!rawObj.hasOwnProperty(k)) continue;
      var v = rawObj[k];
      if (v && typeof v === 'object' && (v.name !== undefined || v.rating !== undefined)) {
        maybePlayers.push(v);
      }
    }
    if (maybePlayers.length > 0) {
      players = maybePlayers;
      if (Array.isArray(rawObj.ignored)) ignored = rawObj.ignored;
    }
  }

  return { players: players, ignored: ignored };
}

function doGet(e) {
  try {
    var file = DriveApp.getFileById(FILE_ID);
    var contents = file.getBlob().getDataAsString();
    return _jsonResponse({ ok: true, bytes: contents.length, content: JSON.parse(contents) });
  } catch (err) {
    return _jsonResponse({ ok: false, message: 'Failed to read Drive file: ' + String(err) });
  }
}

function doPost(e) {
  try {
    var raw = e && e.postData && e.postData.contents ? e.postData.contents : '';
    if (!raw) return _jsonResponse({ ok: false, message: 'Empty request body' });

    var parsed;
    try { parsed = JSON.parse(raw); } catch (err) { return _jsonResponse({ ok: false, message: 'Invalid JSON: ' + String(err) }); }

    var normalized = _normalizeIncoming(parsed);
    if (!normalized.players || !Array.isArray(normalized.players) || normalized.players.length === 0) {
      return _jsonResponse({ ok: false, message: 'No players array found' });
    }

    for (var i = 0; i < normalized.players.length; i++) {
      var v = normalized.players[i];
      var ok = _validatePlayer(v);
      if (!ok.ok) {
        return _jsonResponse({ ok: false, message: 'Player validation failed at index ' + i + ': ' + ok.msg, player: v });
      }
    }

    if (normalized.ignored && !Array.isArray(normalized.ignored)) {
      return _jsonResponse({ ok: false, message: 'ignored must be an array of integers' });
    }
    if (normalized.ignored) {
      for (var j = 0; j < normalized.ignored.length; j++) {
        if (!_isInteger(normalized.ignored[j])) {
          return _jsonResponse({ ok: false, message: 'ignored elements must be integers' });
        }
      }
    }

    var canonical = {
      players: normalized.players.map(function(p) {
        var obj = { name: String(p.name), rating: Math.floor(Number(p.rating)), matches: Math.floor(Number(p.matches)) };
        if (p.id !== undefined) obj.id = Math.floor(Number(p.id));
        return obj;
      }),
      ignored: normalized.ignored || []
    };

    var content = JSON.stringify(canonical, null, 2);
    var file = DriveApp.getFileById(FILE_ID);
    file.setContent(content);

    return _jsonResponse({ ok: true, bytes: content.length, message: 'Saved to Drive' });
  } catch (err) {
    return _jsonResponse({ ok: false, message: 'Server error: ' + String(err) });
  }
}
