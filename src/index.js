import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const storageKey = "store";
var flags = localStorage.getItem(storageKey);
flags = flags ? flags : "{}"

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags
});

app.ports.storeCache.subscribe(function (val) {

  if (val === null) {
    localStorage.removeItem(storageKey);
  } else {
    localStorage.setItem(storageKey, JSON.stringify(val));
  }

  // Report that the new session was stored successfully.
  // setTimeout(function () { app.ports.onStoreChange.send(val); }, 0);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
