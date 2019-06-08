function createFilter(table, columns) {
  var input = $('<input type="text"/>').on("keyup", function() {
    table.draw();
  });

  $.fn.dataTable.ext.search.push(function(
    settings,
    searchData,
    index,
    rowData,
    counter
  ) {
    var val = input.val().toLowerCase();

    for (var i = 0, ien = columns.length; i < ien; i++) {
      if (searchData[columns[i]].toLowerCase().indexOf(val) !== -1) {
        return true;
      }
    }

    return false;
  });

  return input;
}




function init_table(options) {

  options = options || {};
  var csv_path = options.csv_path || "";
  var el = options.element || "table-container";
  var allow_download = options.allow_download || false;
  var csv_options = options.csv_options || {};
  var datatables_options = options.datatables_options || {};

  $("#" + el).html("<table class='table table-striped table-hover' id='my-table' style='width: 100%;'></table>");

  $.when($.get(csv_path)).then(
    function(data){
      var csv_data = $.csv.toArrays(data, csv_options);

      var table_head = "<thead><tr>";

      for (head_id = 0; head_id < csv_data[0].length; head_id++) { 
        table_head += "<th>" + csv_data[0][head_id] +  "</th>";
      }

      table_head += "</tr></thead>";
     $('#my-table tfoot tr').appendTo('#my-table thead');
      $('#my-table').append(table_head);

      $('#my-table').append("<tbody></tbody>");

      for (row_id = 1; row_id < csv_data.length; row_id++) { 
        var row_html = "<tr>";

          for (col_id = 0; col_id < csv_data[row_id].length; col_id++) { 
            row_html += "<td style='width:auto'>" + csv_data[row_id][col_id] + "</td>";
          }
          
        row_html += "</tr>";
        $('#my-table tbody').append(row_html);
      }

var table_foot = "<tfoot><tr>";

      for (head_id = 0; head_id < csv_data[0].length; head_id++) { 
        table_foot += "<th>" + csv_data[0][head_id] + "</th>";
      }

      table_foot += "</tr></tfoot>";
     $('#my-table').append(table_foot);   
     
    

$(document).ready(function() {
    // Setup - add a text input to each footer cell
    $('#my-table thead th').each( function () {   // change tfoot here to thead or vice versa to reposition the column search above or below
        var title = $(this).text();
        $(this).html( ' '+ title +' <br> <input type="text" size=11% placeholder="Search '+title+'" />' );
    } );
$('#my-table tfoot th').each( function () {   // change tfoot here to thead or vice versa to reposition the column search above or below
        var title = $(this).text();
        $(this).html( '<input type="text" placeholder="Search '+title+'" />' );
    } );


 
    // DataTable
    var table = $('#my-table').DataTable(datatables_options);
// table.search("2017 Ongoing").draw();
   
 // Apply the footer search
    table.columns().every( function () {
        var that = this;
 
        $( 'input', this.footer() ).on( 'keyup change', function () {
            if ( that.search() !== this.value ) {
                that
                    .search( this.value )
                    .draw();
            }
        } );

    } );

// Apply the header search
table.columns().every( function () {
        var that = this;
 
        $( 'input', this.header() ).on( 'keyup change', function () {
            if ( that.search() !== this.value ) {
                that
                    .search( this.value )
                    .draw();
            }
        } );

    } );


} );




      if (allow_download)
        $("#" + el).append("<p><a class='btn btn-info' href='" + csv_path + "'><i class='glyphicon glyphicon-download'></i> Download as CSV</a></p>");
    });
}