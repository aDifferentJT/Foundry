"use strict";

exports._requestDevice = (vendorId, productId) => navigator.usb.requestDevice({ filters: [{ vendorId: vendorId, productId: productId }] });
exports._openDevice = dev => dev.open();
exports._closeDevice = dev => dev.close();
exports._selectConfiguration = (dev, n) => dev.selectConfiguration(n);
exports._claimInterface = (dev, n) => dev.claimInterface(n);
exports._releaseInterface = (dev, n) => dev.releaseInterface(n);
exports._controlTransferIn = function(nothing, just, dev, reqType, recipient, request, val, index, len) {
  return dev.controlTransferIn({ requestType: reqType, recipient: recipient, request: request, value: val, index: index }, len);
}
exports._controlTransferOut = function(dev, reqType, recipient, request, val, index, d) {
  return dev.controlTransferOut({ requestType: reqType, recipient: recipient, request: request, value: val, index: index }, UInt8Array.from(d));
}
exports._transferIn = function(nothing, just, dev, endP, len) {
  const data = dev.transferIn(endP, len).data;
  if (!data) return nothing;
};
exports._transferOut = (dev, endP, d) => dev.transferOut(endP, UInt8Array.from(d));

