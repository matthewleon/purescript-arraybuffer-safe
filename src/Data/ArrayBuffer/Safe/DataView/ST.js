"use strict";

exports.fromArrayBuffer = function (arrayBuffer) {
  return function() {
    return new DataView(arrayBuffer.slice());
  };
};

exports.setter = function(setterName) {
  return function (endianness) {
    return function (stDataView) {
      return function (offset) {
        return function (val) {
          return function() {
            try {
              stDataView[setterName](offset, endianness, val);
            }
            catch (e) {
              if (e instanceof RangeError) return false;
              else throw e;
            }
            return true;
          };
        };
      };
    };
  };
};
