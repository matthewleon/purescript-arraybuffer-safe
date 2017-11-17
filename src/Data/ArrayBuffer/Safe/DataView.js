"use strict";

exports.fromArrayBuffer = function(arrayBuffer) {
  return new DataView(arrayBuffer);
};

exports.fromArrayBufferWithOffsetImpl = function(just, nothing, arrayBuffer, byteOffset) {
  try {
    return just(new DataView(arrayBuffer, byteOffset));
  }
  catch (e) {
    if (e instanceof RangeError) return nothing;
    else throw e;
  }
};

exports.fromArrayBufferWithOffsetAndLengthImpl = function(just, nothing, arrayBuffer, byteOffset, length) {
  try {
    return just(new DataView(arrayBuffer, byteOffset, length));
  }
  catch (e) {
    if (e instanceof RangeError) return nothing;
    else throw e;
  }
};

exports.getterImpl = function(just, nothing, getterName, endianness, dataView, offset) {
  try {
    return just(dataView[getterName](offset, endianness));
  }
  catch (e) {
    if (e instanceof RangeError) return nothing;
    else throw e;
  }
};

exports.buffer = function(dataView) {
  return dataView.buffer;
};

exports.byteLength = function(dataView) {
  return dataView.byteLength;
};

exports.byteOffset = function(dataView) {
  return dataView.byteOffset;
};
