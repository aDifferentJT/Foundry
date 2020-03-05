"use strict";

exports._requestDevice = vendorId => productId => () => navigator.usb.requestDevice({ filters: [{ vendorId: vendorId, productId: productId }] });

exports._openDevice = dev => () => dev.open();
exports._closeDevice = dev => () => dev.close();

exports._selectConfiguration = dev => n => () => dev.selectConfiguration(n);

exports._claimInterface = dev => n => () => dev.claimInterface(n);
exports._releaseInterface = dev => n => () => dev.releaseInterface(n);

exports._controlTransferIn = dev => reqType => recipient => request => val => index => len => () =>
  dev.controlTransferIn({ requestType: reqType, recipient: recipient, request: request, value: val, index: index }, len)
    .then(result => Array.from(new Uint8Array(result.data.buffer)));
exports._controlTransferOut = dev => reqType => recipient => request => val => index => d => () =>
  dev.controlTransferOut({ requestType: reqType, recipient: recipient, request: request, value: val, index: index }, UInt8Array.from(d).buffer);

exports._transferIn = dev => endP => len => () =>
  dev.transferIn(endP, len).then(result => Array.from(new Uint8Array(result.data.buffer)));
exports._transferOut = dev => endP => d => () => dev.transferOut(endP, Uint8Array.from(d).buffer);

