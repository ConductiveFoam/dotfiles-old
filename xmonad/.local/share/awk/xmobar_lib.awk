#!/usr/bin/awk
function format(l, h, c) {
  if (h < c) {
    col = "#dc322f"
  } else if (l < c) {
    col = "#859900"
  } else {
    col = "#268bd2"
  }
  return "<fc=" col ">" c "</fc>"
}
