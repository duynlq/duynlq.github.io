//Manual user inputs
let n = 3;
let last_tile = "tile9";

function swapTiles(cell1,cell2) {
  var temp = document.getElementById(cell1).className;
  document.getElementById(cell1).className = document.getElementById(cell2).className;
  document.getElementById(cell2).className = temp;
}

function shuffle() {

for (var row=1;row<=n;row++) {
   for (var column=1;column<=n;column++) {
  
    var row2=Math.floor(Math.random()*n + 1);
    var column2=Math.floor(Math.random()*n + 1);
     
    swapTiles("cell"+row+column,"cell"+row2+column2);
  } 
} 
}

function clickTile(row,column) {
  var cell = document.getElementById("cell"+row+column);
  var tile = cell.className;
  if (tile!=last_tile) { 

        //Check if white tile is right
        if (column<n) {
          if ( document.getElementById("cell"+row+(column+1)).className==last_tile) {
            swapTiles("cell"+row+column,"cell"+row+(column+1));
            return;
          }
        }

        //Check if white tile is left
        if (column>1) {
          if ( document.getElementById("cell"+row+(column-1)).className==last_tile) {
            swapTiles("cell"+row+column,"cell"+row+(column-1));
            return;
          }
        }

        //Check if white tile is above
        if (row>1) {
          if ( document.getElementById("cell"+(row-1)+column).className==last_tile) {
            swapTiles("cell"+row+column,"cell"+(row-1)+column);
            return;
          }
        }

        //Check if white tile is below
        if (row<n) {
          if ( document.getElementById("cell"+(row+1)+column).className==last_tile) {
            swapTiles("cell"+row+column,"cell"+(row+1)+column);
            return;
          }
        } 
    }
  
}

