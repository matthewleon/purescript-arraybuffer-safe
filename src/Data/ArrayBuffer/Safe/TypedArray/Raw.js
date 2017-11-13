"use strict";

exports.every = function(dictIsArrayType) {
  return function(callback) {
    return function(av) {
      function uncurriedCallback(currentValue, index, array) {
        return callback(currentValue)(index)(array);
      };
      return av.every(uncurriedCallback);
    };
  };
};

exports.filter = function(dictIsArrayType) {
  return function(callback) {
    return function(av) {
      function uncurriedCallback(currentValue, index, array) {
        return callback(currentValue)(index)(array);
      };
      return av.filter(uncurriedCallback);
    };
  };
};

exports.find = function(dictIsArrayType) {
  return function(callback) {
    return function(av) {
      function uncurriedCallback(currentValue, index, array) {
        return callback(currentValue)(index)(array);
      };
      return av.find(uncurriedCallback);
    };
  };
};

exports.findIndex = function(dictIsArrayType) {
  return function(callback) {
    return function(av) {
      function uncurriedCallback(currentValue, index, array) {
        return callback(currentValue)(index)(array);
      };
      return av.findIndex(uncurriedCallback);
    };
  };
};

exports.indexOf = function(dictIsArrayType) {
  return function(searchElement) {
    return function(fromIndex) {
      return function(av) {
        return av.indexOf(searchElement, fromIndex);
      };
    };
  };
};

exports.lastIndexOf = function(dictIsArrayType) {
  return function(searchElement) {
    return function(fromIndex) {
      return function(av) {
        return av.lastIndexOf(searchElement, fromIndex);
      };
    };
  };
};

exports.includes = function(dictIsArrayType) {
  return function(searchElement) {
    return function(fromIndex) {
      return function(av) {
        return av.includes(searchElement, fromIndex);
      };
    };
  };
};

exports.indexOf = function(dictIsArrayType) {
  return function(searchElement) {
    return function(fromIndex) {
      return function(av) {
        return av.indexOf(searchElement, fromIndex);
      };
    };
  };
};

exports.join = function(separator) {
  return function(av) {
    return av.join(separator);
  };
};

exports.map = function(dictIsArrayType) {
  return function(callback) {
    return function(av) {
      function uncurriedCallback(currentValue, index, array) {
        return callback(currentValue)(index)(array);
      };
      return av.map(uncurriedCallback);
    };
  };
};

exports.reduce = function(dictIsArrayType) {
  return function(callback) {
    return function(initialValue) {
      return function(av) {
        function uncurriedCallback(previousValue, currentValue, index, array) {
          return callback(previousValue)(currentValue)(index)(array);
        }
        return av.reduce(uncurriedCallback, initialValue);
      };
    };
  };
};

exports.reduceRight = function(dictIsArrayType) {
  return function(callback) {
    return function(initialValue) {
      return function(av) {
        function uncurriedCallback(previousValue, currentValue, index, array) {
          return callback(previousValue)(currentValue)(index)(array);
        }
        return av.reduceRight(uncurriedCallback, initialValue);
      };
    };
  };
};

exports.slice = function(begin) {
  return function(end) {
    return function(av) {
      return av.slice(begin, end);
    };
  };
};

exports.some = function(dictIsArrayType) {
  return function(callback) {
    return function(av) {
      function uncurriedCallback(currentValue, index, array) {
        return callback(currentValue)(index)(array);
      };
      return av.some(uncurriedCallback);
    };
  };
};

exports.subarray = function(begin) {
  return function(end) {
    return function(av) {
      return av.subarray(begin, end);
    };
  };
};

exports.toString = function(av) {
  return av.toString();
};
