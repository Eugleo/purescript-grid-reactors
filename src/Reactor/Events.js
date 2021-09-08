"use strict";

exports.windowPerformanceNow = function (window) {
  return function () {
    return window.performance.now();
  };
};

exports.offsetX = function (e) {
  return e.offsetX;
};

exports.offsetY = function (e) {
  return e.offsetY;
};
