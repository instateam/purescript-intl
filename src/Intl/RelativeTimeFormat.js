/* Data.Intl.RelativeTimeFormat */
"use strict";

exports.createImpl = function(locale, options) {
  return new Intl.RelativeTimeFormat(locale, options);
};

exports.supportedLocalesOfImpl = function(locales, options) {
  return Intl.RelativeTimeFormat.supportedLocalesOf(locales, options);
};

exports.formatImpl = function(value, unit, formatter) {
  return formatter.format(value, unit);
};

exports.formatToPartsImpl = function(value, unit, formatter) {
  return formatter.formatToParts(value, unit);
};

exports.resolvedOptionsImpl = function(formatter) {
  return formatter.resolvedOptions();
};
