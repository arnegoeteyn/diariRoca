import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const storageKey = "store";
var savedDB = localStorage.getItem(storageKey);
savedDB = savedDB ? savedDB : "{}"

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

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();
