"use strict";

exports.unsafeInsertAt = function (i) {
  return function (a) {
    return function (l) {
      var l1 = l.slice();
      l1.splice(i, 0, a);
      return l1;
    };
  };
};

exports.unsafeSetAt = function (i) {
  return function (a) {
    return function (l) {
      var l1 = l.slice();
      l1.splice(i, 1, a);
      return l1;
    };
  };
};

exports.unsafeModifyAt = function (i) {
  return function (f) {
    return function (l) {
      var l1 = l.slice();
      l1.splice(i, 1, f(l[i]));
      return l1;
    };
  };
};
