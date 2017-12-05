"use strict";

exports.newSTTypedArrayImpl = function(just) {
  return function (nothing) {
    return function (constructor) {
      return function (length) {
        return function () {
          try {
            return just(new constructor(length));
          }
          catch (e) {
            if (e instanceof RangeError) return nothing;
            else throw e;
          }
        };
      };
    };
  };
};

exports.peekImpl = function(dictIsArrayType) {
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

exports.poke = function (dictIsArrayType) {
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
