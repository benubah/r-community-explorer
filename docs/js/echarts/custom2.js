// always remember to vary the limit variable to show more or less words in the wordcloud. A high value for limit could make the wordcloud empty (an empty web page)

d3.text("data/rgsoc_project_titles.txt").then(function(data) {
//console.log(data);


      var text_string = data;

      drawWordCloud(text_string);

      function drawWordCloud(text_string){
        var common = "poop,i,me,my,myself,we,us,our,ours,ourselves,you,your,yours,yourself,yourselves,he,him,his,himself,she,her,hers,herself,it,its,itself,they,them,their,theirs,themselves,what,which,who,whom,whose,this,that,these,those,am,is,are,was,were,be,been,being,have,has,had,having,do,does,did,doing,will,would,should,can,could,ought,i'm,you're,he's,she's,it's,we're,they're,i've,you've,we've,they've,i'd,you'd,he'd,she'd,we'd,they'd,i'll,you'll,he'll,she'll,we'll,they'll,isn't,aren't,wasn't,weren't,hasn't,haven't,hadn't,doesn't,don't,didn't,won't,wouldn't,shan't,shouldn't,can't,cannot,couldn't,mustn't,let's,that's,who's,what's,here's,there's,when's,where's,why's,how's,a,an,the,and,but,if,or,because,as,until,while,of,at,by,for,with,about,and,against,between,into,through,during,before,after,above,below,to,from,up,upon,down,in,out,on,off,over,under,again,further,then,once,here,there,when,where,why,how,all,any,both,each,few,more,most,other,some,such,no,nor,not,only,own,same,so,than,too,very,say,says,said,shall,<br/>, http, href=, class=, //www, http://, https, >https, <a>,</a>, //, www, please, every, com, linkified, using, via"; // linkified is a css class for meetup

        var word_count = {};

var words = text_string.replace(/[&\/\\#,+()$~%.'":*?<>{}]/g, ' '); // replace all special characters with empty space

        var words = words.split(/[ '\-\(\)\*":;\[\]|{},.!?]+/); //split all words into an array of words

//console.log(words2)
limit = 3; // words less than this are removed from the cloud
          if (words.length == 1){
            word_count[words[0]] = 1;
          } else {
            words.forEach(function(word){
              var word = word.toLowerCase();
              if (word != "" && common.indexOf(word)==-1 && word.length>1){
                if (word_count[word]){
                  word_count[word]++;
                } else {
                  word_count[word] = 1;
                }
              }
            });

//console.log(word_count.data) // gives an array of words and their counts
// words with a count less than 'limit' are removed here. See https://stackoverflow.com/questions/41663341/word-cloud-set-max-words
 for (var word in word_count) {
    if (word_count[word] < limit) delete word_count[word];
  }

          }

        var svg_location = "#wordchart";
        var width = $(document).width();
        var height = $(document).height();

        var fill = d3.scale.category20();

        var word_entries = d3.entries(word_count);
//console.log(word_count)
//console.log(word_entries[0])
        var xScale = d3.scale.linear()
           .domain([0, d3.max(word_entries, function(d) {
              return d.value;
            })
           ])
           .range([10,80]); // vary range from 100 to 80 to reduce overall size of word-cloud

// plot bar chart of words and their frequency
  wrds = [];
countss = [];
word_entries.sort(function(x, y){
   return d3.descending(x.value, y.value);
});
   for(let i = 0, l = 20; i < l; i++) {
	wrds.push(word_entries[i].key);
     countss.push(word_entries[i].value);
}

var f1=echarts.init(document.getElementById("word_bar"));f1.setOption({title:{text:"Word Count",subtext:"Top 20 Words"},color:["#435f9c"],tooltip:{trigger:"axis"},calculable:!0,xAxis:[{type:"value", splitArea:{show:!0,areaStyle:{color:["#f3f9fc","#eef5f9"]}}
}],yAxis:[{type:"category",boundaryGap:!0, axisLabel:{ interval:0 }, data:wrds.reverse()  }],grid: { left: 100 },
series:[{name:"Word Count",type:"bar",smooth:!1, itemStyle:{},barWidth:12, data:countss.reverse()   }]})

      
var margin = {top: 10, right: 10, bottom: 10, left: 5},
    width = 850 - margin.left - margin.right,
    height = 450 - margin.top - margin.bottom;

// append the svg object to the body of the page
var svg = d3.select("#wordchart").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

// Constructs a new cloud layout instance. It run an algorithm to find the position of words that suits your requirements
// Wordcloud features that are different from one word to the other must be here
var layout = d3.layout.cloud()
  .size([width, height])
  .words(word_entries)
  .fontSize(function(d) { return xScale(+d.value); })
  .text(function(d) { return d.key; })
  .padding(5)        //space between words
.rotate(function() { return ~~(Math.random() * 2) * 90; })
 .on("end", draw);
layout.start();


function draw(words) {
  svg
    .append("g")
      .attr("transform", "translate(" + layout.size()[0] / 2 + "," + layout.size()[1] / 2 + ")")
      .selectAll("text")
        .data(words)
      .enter().append("text")
        .style("font-size", function(d) { return xScale(d.value) + "px"; })
        .style("fill", function(d, i) { return fill(i); })
        .attr("text-anchor", "middle")
        .style("font-family", "Impact")
        .attr("transform", function(d) {
          return "translate(" + [d.x, d.y] + ")rotate(" + d.rotate + ")";
        })
        .text(function(d) { return d.key; });
}

        
        d3.layout.cloud().stop();
      }
}); //d3.text call
   