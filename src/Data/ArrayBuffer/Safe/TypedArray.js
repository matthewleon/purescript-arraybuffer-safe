"use strict";

var ctor = function (dictIsArrayType) {
  return function (arr) {
    return new dictIsArrayType.constructor(arr);
  }
};

exports.fromArray = ctor;

exports.fromArrayBufferImpl = function (dictIsArrayType) {
  return function (just) {
    return function (nothing) {
      return function (ab) {
        try {
          return just(new dictIsArrayType.constructor(ab));
        }
        catch (e) {
          if (e instanceof RangeError) return nothing;
          else throw e;
        }
      };
    };
  };
};

exports.fromArrayBufferWithOffsetImpl = function (dictIsArrayType) {
  return function (just) {
    return function (nothing) {
      return function (ab) {
        return function (byteOffset) {
          try {
            return just(new dictIsArrayType.constructor(ab, byteOffset));
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

exports.fromArrayBufferWithOffsetAndLengthImpl = function (dictIsArrayType) {
  return function (just) {
    return function (nothing) {
      return function (ab) {
        return function (byteOffset) {
          return function (length) {
            try {
              return just(new dictIsArrayType.constructor(ab, byteOffset, length));
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
};
exports.fromTypedArray = ctor;

exports.buffer = function (av) {
  return av.buffer;
};

exports.byteLength = function (av) {
  return av.byteLength;
};

exports.byteOffset = function (av) {
  return av.byteOffset;
};

exports.length = function (av) {
  return av.length;
};

exports.empty = function(dictIsArrayType) {
  return new dictIsArrayType.constructor();
};

exports.every = function(dictIsArrayType) {
  return function(callback) {
    return function(av) {
      return av.every(callback);
    };
  };
};

exports.filter = function(dictIsArrayType) {
  return function(callback) {
    return function(av) {
      return av.filter(callback);
    };
  };
};

exports.slice = function(xs) {
  return function(begin) {
    return function(end) {
      return xs.slice(begin, end);
    };
  };
};

exports.drop = function(xs) {
  return function(length) {
    return xs.slice(length);
  };
};

exports.map = function(dictIsArrayType) {
  return function(callback) {
    return function(av) {
      return av.map(callback);
    };
  };
};

exports.foldl = function(dictIsArrayType) {
  return function(callback) {
    return function(initialValue) {
      return function(av) {
        function uncurriedCallback(previousValue, currentValue) {
          return callback(previousValue)(currentValue);
        }
        return av.reduce(uncurriedCallback, initialValue);
      };
    };
  };
};

exports.unsafeIndexImpl = function(dictIsArrayType) {
  return function (xs) {
    return function (n) {
      return xs[n];
    };
  };
};

/* TODO
exports.bytesPerElement = function (isArrayType) {
  return isArrayType.constructor.BYTES_PER_ELEMENT;
};
*/
