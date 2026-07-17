/**
 * Sets the cursor to "pointer" when hovering over any data point in a plotly
 * plot, and resets it on unhover. Handles multi-facet plots by targeting all
 * .nsewdrag layers.
 *
 * @param {HTMLElement} el - The plotly plot element.
 */
function enablePointerCursorOnHover(el) {
  el.on('plotly_hover', function () {
    el.querySelectorAll('.nsewdrag').forEach(function (d) {
      d.style.cursor = 'pointer';
    });
  });
  el.on('plotly_unhover', function () {
    el.querySelectorAll('.nsewdrag').forEach(function (d) {
      d.style.cursor = '';
    });
  });
}
