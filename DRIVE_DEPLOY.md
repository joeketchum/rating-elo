# Deploying the Apps Script drive handler

1. Open Google Apps Script: https://script.google.com/
2. Create a new project and paste the contents of `apps_script/drive_handler.gs` into the editor.
3. Edit the `FILE_ID` constant to point to your Drive file id (or leave as-is if already correct).
4. Save the project and choose Deploy → New deployment → Web app.
   - Execute as: Me
   - Who has access: Anyone (or Anyone, even anonymous) — this avoids browser CORS issues for public clients.
5. After deployment you'll get a Web app exec URL. Copy that URL and open `docs/index.html`.
   - Update the `SAVE_URL` constant in the host JS to this exec URL.
6. Redeploy the static site (push to GitHub Pages or upload `docs/`), then open the page and test Save to Drive.

Testing via PowerShell (from repo dir):

```powershell
$body = Get-Content .\docs\sample-standings.json -Raw
Invoke-RestMethod -Uri 'https://script.google.com/macros/s/XXXX/exec' -Method POST -Body $body -ContentType 'application/json'
```

If the web app returns `{ "ok": true }` the file was written. If it returns `ok:false` the response will include a `message` explaining why the payload failed validation.
