// Get the information for the hover of `output$data_sets_table`
function dataSetDetails(row, data) {
  $('td', row).attr('data-toggle', 'tooltip');
// Details in the 5th column (index 4) of the dataSetsTable
  $('td', row).attr('title', data[4]);
  $('td', row).attr('data-tooltip-style', 'dark');
}

// Format the tooltip of `output$data_sets_table`
function dtTooltip(settings) {
  $('[data-toggle="tooltip"]').tooltip({
    container: 'body',
    delay: {show: 0, hide: 0}
  }).on('show.bs.tooltip', function() {
    var styleType = $(this).data('tooltip-style');
    var $tip = $(this).data('bs.tooltip').tip();
    if (styleType) {
      $tip.addClass('tooltip-' + styleType);
    }
  });
}

// When clicking on a row in the modal of the DEG contrast intersection, this opens a new tab redirecting to the gene's NCBI page
function visitNCBI(row, data){
  $(row).on('click', function() {
      var url = data[7];
      if(url && url.length > 0) {
        window.open(url, '_blank');
      }
  });
}