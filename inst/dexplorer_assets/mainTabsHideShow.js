// Disable tabs, which shall not yet be used by a user
function disableTabs(tabIds) {
  tabIds.forEach(function(tabId) {
    $('#navigation_bar li a[data-value="' + tabId + '"]').css({
      'pointer-events': 'none',
      'opacity': '0.5'
    });
  });
}

// Enable tabs, when a data set is loaded
function enableTabs(tabIds) {
  tabIds.forEach(function(tabId) {
    $('#navigation_bar li a[data-value="' + tabId + '"]').css({
      'pointer-events': 'auto',
      'opacity': '1'
    });
  });
}

// Redirect the user to the data sets selection tab
$(document).on('click', '#goto-datasets-tab', function() {
  var navBar = document.getElementById('navigation_bar');
  if (navBar) {
    var tab = navBar.querySelector('a[data-value="Data sets"]');
    if (tab) tab.click();
  }
});