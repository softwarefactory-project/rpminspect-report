'use strict';

var Fetch = require("bs-fetch/src/Fetch.bs.js");
var React = require("react");
var Js_exn = require("bs-platform/lib/js/js_exn.js");
var App$SFUI = require("./App.bs.js");
var ReactDom = require("react-dom");
var Caml_option = require("bs-platform/lib/js/caml_option.js");

function handleAPICallError(promise) {
  return promise.then(function (r) {
              if (r.ok) {
                return Promise.resolve({
                            TAG: /* Ok */0,
                            _0: r
                          });
              } else {
                return Promise.resolve({
                            TAG: /* Error */1,
                            _0: "API call failed: " + r.statusText
                          });
              }
            });
}

function extractJson(promise) {
  return promise.then(function (result) {
              if (result.TAG) {
                return Promise.resolve({
                            TAG: /* Error */1,
                            _0: result._0
                          });
              } else {
                return result._0.json().then(function (decoded) {
                              return Promise.resolve({
                                          TAG: /* Ok */0,
                                          _0: Caml_option.some(decoded)
                                        });
                            }).catch(function (param) {
                            return Promise.resolve({
                                        TAG: /* Ok */0,
                                        _0: undefined
                                      });
                          });
              }
            });
}

function raiseOnNok(promise) {
  return promise.then(function (r) {
              if (r.ok) {
                return promise;
              } else {
                return Js_exn.raiseError(r.statusText);
              }
            });
}

function promiseToOptionalJson(promise) {
  return raiseOnNok(promise).then(function (prim) {
                  return prim.json();
                }).then(function (v) {
                return Promise.resolve(Caml_option.some(v));
              }).catch(function (e) {
              console.log("Unexpected error: ", e);
              return Promise.resolve(undefined);
            });
}

function $$fetch$1(url) {
  return promiseToOptionalJson(fetch(url));
}

function postOrPut(verb, url, body) {
  var headers = {
    Accept: "*",
    "Content-Type": "application/json"
  };
  var req = body !== undefined ? Fetch.RequestInit.make(verb, Caml_option.some(headers), Caml_option.some(JSON.stringify(Caml_option.valFromOption(body))), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined)(undefined) : Fetch.RequestInit.make(verb, Caml_option.some(headers), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined)(undefined);
  return extractJson(handleAPICallError(fetch(url, req)));
}

function put(param, param$1) {
  return postOrPut(/* Put */3, param, param$1);
}

function post(param, param$1) {
  return postOrPut(/* Post */2, param, param$1);
}

function $$delete(url) {
  var req = Fetch.RequestInit.make(/* Delete */4, {
          Accept: "*"
        }, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined)(undefined);
  return handleAPICallError(fetch(url, req));
}

var BsFetch = {
  handleAPICallError: handleAPICallError,
  extractJson: extractJson,
  raiseOnNok: raiseOnNok,
  promiseToOptionalJson: promiseToOptionalJson,
  $$fetch: $$fetch$1,
  postOrPut: postOrPut,
  put: put,
  post: post,
  $$delete: $$delete
};

var RealApp = App$SFUI.Main({
      $$fetch: $$fetch$1,
      post: post,
      put: put,
      $$delete: $$delete
    });

var root = document.querySelector("#root");

if (root == null) {
  console.log("Can't find #root element!");
} else {
  ReactDom.render(React.createElement(RealApp.make, {}), root);
}

exports.BsFetch = BsFetch;
exports.RealApp = RealApp;
/* RealApp Not a pure module */
