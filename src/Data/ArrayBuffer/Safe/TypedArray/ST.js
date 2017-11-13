"use strict";

exports.peekSTTypedArrayImpl = function(dictIsArrayType) {
  return function (just) {
    return function (nothing) {
      return function (xs) {
        return function (i) {
          return function () {
            return i >= 0 && i < xs.length ? just(xs[i]) : nothing;
          };
        };
      };
    };
  };
};

exports.pokeSTTypedArray = function (dictIsArrayType) {
  return function (xs) {
    return function (i) {
      return function (a) {
        return function () {
          var ret = i >= 0 && i < xs.length;
          if (ret) xs[i] = a;
          return ret;
        };
      };
    };
  };
};

exports.copyImpl = function (xs) {
  return function () {
    return xs.slice();
  };
};
