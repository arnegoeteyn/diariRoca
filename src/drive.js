import { API_KEY, CLIENT_ID } from "./tokens";

// Discovery doc URL for APIs used by the quickstart
const DISCOVERY_DOC = 'https://www.googleapis.com/discovery/v1/apis/drive/v3/rest';

// Authorization scopes required by the API; multiple scopes can be
// included, separated by spaces.
const SCOPES = 'https://www.googleapis.com/auth/drive.file';

let tokenClient;
let accessToken = null;
let pickerInited = false;
let gisInited = false;
let gapiInited = false;


/**
 * Callback after api.js is loaded.
 */
export function gapiLoaded() {
    gapi.load('picker', onPickerApiLoad);
    gapi.load('client', intializeGapiClient);
}

/**
 * Callback after the API client is loaded. Loads the
 * discovery doc to initialize the API.
 */
async function intializeGapiClient() {
    await gapi.client.init({
        apiKey: API_KEY,
        discoveryDocs: [DISCOVERY_DOC],
    });
    gapiInited = true;
}

async function onPickerApiLoad() {
    pickerInited = true;
}

/**
 * Callback after Google Identity Services are loaded.
 */
export function gisLoaded() {
    tokenClient = google.accounts.oauth2.initTokenClient({
        client_id: CLIENT_ID,
        scope: SCOPES,
        callback: '', // defined later
    });
    gisInited = true;
}

/**
 *  Sign in the user upon button click.
 */
export function handleAuthClick(callback) {
    tokenClient.callback = async (resp) => {
        if (resp.error !== undefined) {
            throw (resp);
        }
        callback();
    };

    if (gapi.client.getToken() === null) {
        // Prompt the user to select a Google Account and ask for consent to share their data
        // when establishing a new session.
        tokenClient.requestAccessToken({ prompt: 'consent' });
    } else {
        // Skip display of account chooser and consent dialog for an existing session.
        tokenClient.requestAccessToken({ prompt: '' });
    }
}

/**
 *  Sign out the user upon button click.
 */
export function handleSignoutClick() {
    const token = gapi.client.getToken();
    if (token !== null) {
        google.accounts.oauth2.revoke(token.access_token);
        gapi.client.setToken('');
    }
}

// Create and render a Picker object for selecting from Google Drive
export function createPicker(callback) {
    const showPicker = () => {
        const view = new google.picker.DocsView(google.picker.ViewId.DOCS);
        view.setMimeTypes("application/json");
        view.setMode(google.picker.DocsViewMode.LIST);
        const picker = new google.picker.PickerBuilder()
            .addView(view)
            .enableFeature(google.picker.Feature.NAV_HIDDEN)
            .setOAuthToken(accessToken)
            .setDeveloperKey(API_KEY)
            .setCallback(function (data) { pickerCallback(data, callback) })
            .build();

        picker.setVisible(true);
    }

    // Use Google Identity Services to request an access token
    tokenClient.callback = async (response) => {
        if (response.error !== undefined) {
            throw (response);
        }
        accessToken = response.access_token;
        showPicker();
    };

    tokenClient.requestAccessToken({ prompt: '' });
}

// A simple callback implementation.
function pickerCallback(data, callback) {
    let url = 'nothing';
    let fileId = -1;
    if (data[google.picker.Response.ACTION] == google.picker.Action.PICKED) {
        let doc = data[google.picker.Response.DOCUMENTS][0];
        url = doc[google.picker.Document.URL];
        fileId = doc[google.picker.Document.ID];
        gapi.client.drive.files.get({ fileId, alt: 'media' }, { responseType: 'stream' }).then(result => {
            callback(result.result);
        }).catch(console.error);
    }
}

export async function saveFile(content) {
    const date = new Date();
    const name = "routes_" + date.toISOString().split('T')[0] + '.json';
    console.log(name);
    const file = new Blob([content], { type: "application/json" });
    var metadata = {
        "name": name,
        "mimeType": "application/json",
        "parents": ["root"],
    };

    var accessToken = gapi.auth.getToken().access_token;
    var form = new FormData();
    form.append('metadata', new Blob([JSON.stringify(metadata)], { type: 'application/json' }));
    form.append('file', file);

    fetch("https://www.googleapis.com/upload/drive/v3/files?uploadType=multipart&supportsAllDrives=true", {
        method: 'POST',
        headers: new Headers({ 'Authorization': 'Bearer ' + accessToken }),
        body: form,
    }).then((res) => {
        return res.json();
    }).then(function (val) {
        console.log(val);
    });
}
