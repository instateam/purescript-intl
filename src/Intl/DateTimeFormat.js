/* Data.Intl.DateTimeFormat */
"use strict";

exports.createImpl = function(locale, options) {
  return new Intl.DateTimeFormat(locale, options);
};

exports.supportedLocalesOfImpl = function(locales, options) {
  return Intl.DateTimeFormat.supportedLocalesOf(locales, options);
};

exports.formatImpl = function(date, formatter) {
  return formatter.format(date);
};

exports.formatToPartsImpl = function(date, formatter) {
  return formatter.formatToParts(date);
};

/*
exports.resolvedOptionsImpl = function(formatter) {
  return formatter.resolvedOptions();
};
*/

exports.formatRangeImpl = function(startDate, endDate, formatter) {
  return formatter.formatRange(startDate, endDate);
};

/*
exports.formatRangeToPartsImpl = function(startDate, endDate, formatter) {
  return formatter.formatToRangeParts(startDate, endDate);
};
*/
