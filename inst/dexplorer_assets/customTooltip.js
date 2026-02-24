// ChatGPT's and Copilot's joint effort 
function enableCustomTooltip(el, tooltipType) {
  var tooltipId = "plotly-tooltip";
  var tooltip = document.getElementById(tooltipId);
  if (!tooltip) {
    tooltip = document.createElement("div");
    tooltip.id = tooltipId;
    tooltip.style.pointerEvents = "auto";
    tooltip.style.position = "absolute";
    tooltip.style.zIndex = 9999;
    tooltip.style.display = "none";
    document.body.appendChild(tooltip);
  }

  var tooltipHovered = false;
  var hideTimer = null;

  el.on("plotly_hover", function(d) {
    if (!d || !d.points || !d.points[0]) return;
    var pt = d.points[0];
    var html;
    // `tooltipType` can be either a string or an object with a `tooltipType` property
    // Thus, extract the actual type string
    var type = tooltipType && tooltipType.tooltipType ? tooltipType.tooltipType : tooltipType;
    

    // switch (type) {
    //   case "gene_sets":
    //     html = pt.customdata ? pt.customdata[0] : null;
    //     break;
    //   case "heatmap":
    //     // `heatmaply::heatmaply()` stores the tooltip HTML in `text`
    //   if (pt.data && pt.data.name === "group_row") {
    //     // When hovering over the group row, the tooltip HTML is stored in `data.text` at the index of the hovered point
    //     html = pt.data.text[pt.pointIndex[1]];
    //   } else {
    //     // For regular heatmap cells, the tooltip HTML is stored in `text`
    //     html = pt.text;
    //   }

    //   case "top_genes":
    //   case "raw_data":
    //   case "jaccard":
    //     html = pt.text ? pt.text : pt.data.text;
    //     break;
    //   case "standard":
    //     break;
    //   default:
    //     console.warn("Unknown tooltip type:", type);
    //     return;

    // }
    // Depending on the plot, the tooltip content is stored differently
    if (type === "gene_sets") {
      // The gene sets plot was configured to store the tooltip HTML in customdata[0]
      html = pt.customdata ? pt.customdata[0] : null;
    } else if (type === "heatmap") {
      // `heatmaply::heatmaply()` stores the tooltip HTML in `text`
      if (pt.data && pt.data.name === "group_row") {
        // When hovering over the group row, the tooltip HTML is stored in `data.text` at the index of the hovered point
        html = pt.data.text[pt.pointIndex[1]];
      } else {
        // For regular heatmap cells, the tooltip HTML is stored in `text`
        html = pt.text;
      }
    } else if (type === "top_genes" || type === "raw_data" || type === "jaccard") {
      // The top genes plot has the tooltip HTML in `data.text`, but also this needs some trimming
      html = pt.text ? pt.text : pt.data.text;

    } else if (type == "standard"){
      // This works for the volcano and the venn plot at the moment
      // Ensure to set the tooltip in `plotly::ggplotly()`
      html = pt.text
    }
    // Return early if no tooltip content is found
    if (!html) return;

    tooltip.innerHTML = html;
    tooltip.style.display = "block";
    tooltip.style.pointerEvents = "auto";

    var ev = d.event || window.event;
    var x = ev ? ev.clientX : 0;
    var y = ev ? ev.clientY : 0;
    var scrollX = window.scrollX || window.pageXOffset;
    var scrollY = window.scrollY || window.pageYOffset;

    tooltip.style.left = "-9999px";
    tooltip.style.top = "-9999px";
    var tooltipRect = tooltip.getBoundingClientRect();
    var winWidth = window.innerWidth;
    var winHeight = window.innerHeight;
    var left = x + scrollX;
    var top = y + scrollY;
    if (x + tooltipRect.width > winWidth) {
      left = x - tooltipRect.width + scrollX;
      if (left < 0) left = 0;
    }
    if (y + tooltipRect.height > winHeight) {
      top = y - tooltipRect.height + scrollY;
      if (top < 0) top = 0;
    }
    tooltip.style.left = left + "px";
    tooltip.style.top = top + "px";

    if (hideTimer) {
      clearTimeout(hideTimer);
      hideTimer = null;
    }
  });

  el.on("plotly_unhover", function() {
    hideTimer = setTimeout(function() {
      if (!tooltipHovered) {
        tooltip.style.display = "none";
      }
    }, 100);
  });

  tooltip.addEventListener("mouseenter", function() {
    tooltipHovered = true;
    if (hideTimer) {
      clearTimeout(hideTimer);
      hideTimer = null;
    }
    tooltip.style.pointerEvents = "auto";
  });

  tooltip.addEventListener("mouseleave", function() {
    tooltipHovered = false;
    tooltip.style.display = "none";
  });
}