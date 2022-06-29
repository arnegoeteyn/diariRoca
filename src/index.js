import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import * as drive from './drive';

const storageKey = "store";
var savedDB = localStorage.getItem(storageKey);
savedDB = savedDB ? savedDB : "{}"

const gapiScript = document.createElement('script');
gapiScript.onload = drive.gapiLoaded;
gapiScript.src = 'https://apis.google.com/js/api.js';

const gsiScript = document.createElement('script');
gsiScript.onload = drive.gisLoaded;
gsiScript.src = 'https://accounts.google.com/gsi/client';

document.head.appendChild(gapiScript);
document.head.appendChild(gsiScript);

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: { storageCache: savedDB, posixTime: Date.parse(new Date()) }
});


app.ports.storeCache.subscribe(function (val) {
  if (val === null) {
    localStorage.removeItem(storageKey);
  } else {
    localStorage.setItem(storageKey, JSON.stringify(val));
  }
});

app.ports.googleDriveCommandPort.subscribe(function (request) {
  switch (request.type_.toLowerCase()) {
    case "authorize":
      drive.handleAuthClick(onGoogleDriveAuthorized);
      break;

    case "showpicker":
      drive.createPicker(onDriveFileChosen);
      break;

    case "save":
      drive.saveFile(request.argument, onDriveFileUploaded);
      break;

    default:
      break;
  }
});

function onGoogleDriveAuthorized() {
  app.ports.googleDriveSubscriptionPort.send({ type_: "Authorized", argument: null });
}

function onDriveFileChosen(content) {
  app.ports.googleDriveSubscriptionPort.send({ type_: "FileChosen", argument: JSON.stringify(content) });
}

function onDriveFileUploaded() {
  alert("uploaded!");
}

serviceWorker.register();
