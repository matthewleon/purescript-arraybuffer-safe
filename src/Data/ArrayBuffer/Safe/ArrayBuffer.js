"use strict";

exports.byteLength = function(a) {
  return a.byteLength;
}

exports.sliceImpl = function(s, e, a) {
  return a.slice(s, e);
}
