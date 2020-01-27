"use strict";

var elm = require('../../../elm/main.js');

exports.init = () => Elm.Main.init({ node: document.getElementById('elm') });
exports.send = app => port => app.ports[port].send;
exports.subscribe = app => port => app.ports[port].subscribe;

