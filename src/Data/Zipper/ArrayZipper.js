export function unsafeInsertAt(i) {
  return function (a) {
    return function (l) {
      var l1 = l.slice();
      l1.splice(i, 0, a);
      return l1;
    };
  };
}

export function unsafeSetAt(i) {
  return function (a) {
    return function (l) {
      var l1 = l.slice();
      l1.splice(i, 1, a);
      return l1;
    };
  };
}

export function unsafeModifyAt(i) {
  return function (f) {
    return function (l) {
      var l1 = l.slice();
      l1.splice(i, 1, f(l[i]));
      return l1;
    };
  };
}
