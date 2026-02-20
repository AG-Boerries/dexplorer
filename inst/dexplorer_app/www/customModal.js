// Customize the width of modals from DGEA contrast intersection and GSEA top scoring gene sets
$(document).on('shown.bs.modal', '#shiny-modal', function() {
  var title = $(this).find('.modal-title').text();
  if(title.startsWith('Enriched genes in') || title.startsWith('DEGs at the intersection of')) {
    $(this).addClass('enlarged-modal');
  } else {
    $(this).removeClass('enlarged-modal');
  }
});

// Register a Shiny custom message handler to show the custom modal for the venn diagram
Shiny.addCustomMessageHandler('show-custom-modal-venn', function(message) {
  $('#customModalVenn').modal('show');
});

// Register a Shiny custom message handler to show the custom modal for the volcano plot
Shiny.addCustomMessageHandler('show-custom-modal-volcano', function(message) {
  $('#customModalVolcano').modal('show');
});

// Register a Shiny custom message handler to show the custom modal for the heatmap
Shiny.addCustomMessageHandler('show-custom-modal-heatmap', function(message) {
  $('#customModalHeatmap').modal('show');
});
