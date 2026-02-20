function disableTabs(tabIds) {
  tabIds.forEach(function(tabId) {
    $('#navigation_bar li a[data-value="' + tabId + '"]').css({
      'pointer-events': 'none',
      'opacity': '0.5'
    });
  });
}

function enableTabs(tabIds) {
  tabIds.forEach(function(tabId) {
    $('#navigation_bar li a[data-value="' + tabId + '"]').css({
      'pointer-events': 'auto',
      'opacity': '1'
    });
  });
}