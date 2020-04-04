   document.getElementById('lfmap').innerHTML = "<div id='map' style='width: 100%; height: 100%;'></div>";
// initialize the map
var activegroups  = L.layerGroup();
var inactivegroups  = L.layerGroup();
var unbegungroups  = L.layerGroup();

// initialize the map
  var map = L.map('map', {
		zoomControl: false,
		zoom: 10,
		layers: [activegroups, inactivegroups, unbegungroups]
	});
 // var map = L.map('map2')
map.setView([10, 20],2);
// add reset/home button to map for resetting the zoom
var zoomHome = L.Control.zoomHome();
zoomHome.addTo(map);
  // load a tile layer
L.tileLayer('https://{s}.tiles.mapbox.com/v3/ebrelsford.ho06j5h0/{z}/{x}/{y}.png',
    {
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
      maxZoom: 17,
      minZoom: 1
    }).addTo(map);

var slider = L.timelineSliderControl({
    formatOutput: function(date){
      return moment(date).format("YYYY-MM-DD");
    },
     autoPlay: true
  });
  map.addControl(slider);


  // load GeoJSON from an external file
 $.getJSON("data/rladies_map_data.geojson",function(data){
    
L.AwesomeMarkers.Icon.prototype.options.prefix = 'fa';
     var purpleMarker = L.AwesomeMarkers.icon({
icon: 'check-circle',
    markerColor: 'purple'
  });

var darkpurpleMarker = L.AwesomeMarkers.icon({
icon: 'user-times',
markerColor: 'darkpurple'
  });

var darkredMarker = L.AwesomeMarkers.icon({
icon: 'user-times',
    markerColor: 'orange'
  });

 var pins =   L.timeline(data,{
pointToLayer: function(feature,latlng){
past_events = feature.properties.past_events;
if ( past_events >= 0 & feature.properties.days_since_last_event <= 180 | feature.properties.upcoming_events > 0) {
      var marker = new L.marker(latlng, {icon: purpleMarker});

marker.bindPopup("<b>" + feature.properties.url + '</b>' + '<br/>' + 'Created: '+ feature.properties.created + '<br/>' +
 'Members: ' + feature.properties.members  + '<br/>' + 'Past Events: '+ feature.properties.past_events
  + '<br/>' + 'Upcoming Events: '+ feature.properties.upcoming_events + '<br/>' + 'Last Event Date: '+ feature.properties.last_event + '<br/>' + 'Active');
      return marker;
}
}  
}).addTo(activegroups);


 var pins2 =   L.timeline(data,{
pointToLayer: function(feature,latlng){
past_events = feature.properties.past_events;
if (feature.properties.days_since_last_event > 180 & past_events != 0 & feature.properties.upcoming_events == 0 ){
      var marker = new L.marker(latlng, {icon: darkpurpleMarker});
marker.bindPopup("<b>" + feature.properties.url + '</b>' + '<br/>' + 'Created: '+ feature.properties.created + '<br/>' +
 'Members: ' + feature.properties.members  + '<br/>' + 'Past Events: '+ feature.properties.past_events 
  + '<br/>' + 'Upcoming Events: '+ feature.properties.upcoming_events + '<br/>' + 'Last Event Date: '+ feature.properties.last_event + '<br/>'  + 'Months Inactive: '+ Math.round(feature.properties.days_since_last_event/30) + ' months' + '<br/>' + 'Inactive: <a href="https://rladies.org/about-us/help/"><b>Become an Organizer!</b></a>');
      return marker;
}
}  
}).addTo(inactivegroups);


var pins3 =   L.timeline(data,{
pointToLayer: function(feature,latlng){
past_events = feature.properties.past_events;
if ( past_events == 0 & feature.properties.upcoming_events == 0) {
      var marker = new L.marker(latlng, {icon: darkredMarker});

marker.bindPopup("<b>" + feature.properties.url + '</b>' + '<br/>' + 'Created: '+ feature.properties.created + '<br/>' +
 'Members: ' + feature.properties.members  + '<br/>' + 'Past Events: '+ feature.properties.past_events
  + '<br/>' + 'Upcoming Events: '+ feature.properties.upcoming_events + '<br/>' + 'Months Inactive: '+ Math.round((new Date() - new Date(feature.properties.created) ) /(30*60*60*24*1000)) + ' months' + '<br/>' + 'Unbegun: <a href="https://rladies.org/about-us/help/"><b>Become an Organizer!</b></a>');
      return marker;
}
}  
}).addTo(unbegungroups);

var baseLayers = {

};
var overlays = {
		"Active": activegroups,
		"Inactive": inactivegroups,
		"Unbegun": unbegungroups
			};

	L.control.layers(baseLayers, overlays,{collapsed:false}).addTo(map);

  pins.addTo(map);
  pins2.addTo(map);
  pins3.addTo(map);
  slider.addTimelines(pins,pins2,pins3);

    })



if($("#rlfmap").length){
	
   document.getElementById('rlfmap').innerHTML = "<div id='map' style='width: 100%; height: 100%;'></div>";
// initialize the map
var activegroups  = L.layerGroup();
var inactivegroups  = L.layerGroup();
var unbegungroups  = L.layerGroup();

// initialize the map
  var map = L.map('map', {
		zoomControl: false,
		zoom: 10,
		layers: [activegroups, inactivegroups, unbegungroups]
	});
 // var map = L.map('map2')
map.setView([10, 20],2);
// add reset/home button to map for resetting the zoom
var zoomHome = L.Control.zoomHome();
zoomHome.addTo(map);
  // load a tile layer
L.tileLayer('https://{s}.tiles.mapbox.com/v3/ebrelsford.ho06j5h0/{z}/{x}/{y}.png',
    {
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
      maxZoom: 17,
      minZoom: 1
    }).addTo(map);

var slider = L.timelineSliderControl({
    formatOutput: function(date){
      return moment(date).format("YYYY-MM-DD");
    },
     autoPlay: true
  });
  map.addControl(slider);


  // load GeoJSON from an external file
 $.getJSON("data/rugs_map_data.geojson",function(data){
    
L.AwesomeMarkers.Icon.prototype.options.prefix = 'fa';
     var purpleMarker = L.AwesomeMarkers.icon({
icon: 'check-circle',
    markerColor: 'blue'
  });

var darkpurpleMarker = L.AwesomeMarkers.icon({
icon: 'user-times',
markerColor: 'darkblue'
  });

var darkredMarker = L.AwesomeMarkers.icon({
icon: 'user-times',
    markerColor: 'orange'
  });

 var pins =   L.timeline(data,{
pointToLayer: function(feature,latlng){
past_events = feature.properties.past_events;
if ( past_events >= 0 & feature.properties.days_since_last_event <= 180 | feature.properties.upcoming_events > 0) {
      var marker = new L.marker(latlng, {icon: purpleMarker});

marker.bindPopup("<b>" + feature.properties.url + '</b>' + '<br/>' + 'Created: '+ feature.properties.created + '<br/>' +
 'Members: ' + feature.properties.members  + '<br/>' + 'Past Events: '+ feature.properties.past_events
  + '<br/>' + 'Upcoming Events: '+ feature.properties.upcoming_events + '<br/>' + 'Last Event Date: '+ feature.properties.last_event + '<br/>' + 'Active');
      return marker;
}
}  
}).addTo(activegroups);


 var pins2 =   L.timeline(data,{
pointToLayer: function(feature,latlng){
past_events = feature.properties.past_events;
if (feature.properties.days_since_last_event > 180 & past_events != 0 & feature.properties.upcoming_events == 0 ){
      var marker = new L.marker(latlng, {icon: darkpurpleMarker});
marker.bindPopup("<b>" + feature.properties.url + '</b>' + '<br/>' + 'Created: '+ feature.properties.created + '<br/>' +
 'Members: ' + feature.properties.members  + '<br/>' + 'Past Events: '+ feature.properties.past_events 
  + '<br/>' + 'Upcoming Events: '+ feature.properties.upcoming_events + '<br/>' + 'Last Event Date: '+ feature.properties.last_event + '<br/>'  + 'Months Inactive: '+ Math.round(feature.properties.days_since_last_event/30) + ' months' + '<br/>' + 'Inactive: <a href="https://www.r-consortium.org/projects/r-user-group-support-program"><b>Apply For RConsortium Grant</b></a>');
      return marker;
}
}  
}).addTo(inactivegroups);


var pins3 =   L.timeline(data,{
pointToLayer: function(feature,latlng){
past_events = feature.properties.past_events;
if ( past_events == 0 & feature.properties.upcoming_events == 0) {
      var marker = new L.marker(latlng, {icon: darkredMarker});

marker.bindPopup("<b>" + feature.properties.url + '</b>' + '<br/>' + 'Created: '+ feature.properties.created + '<br/>' +
 'Members: ' + feature.properties.members  + '<br/>' + 'Past Events: '+ feature.properties.past_events
  + '<br/>' + 'Upcoming Events: '+ feature.properties.upcoming_events + '<br/>' + 'Months Inactive: '+ Math.round((new Date() - new Date(feature.properties.created) ) /(30*60*60*24*1000)) + ' months' + '<br/>' + 'Unbegun: <a href="https://www.r-consortium.org/projects/r-user-group-support-program"><b>Apply For RConsortium Grant</b></a>');
      return marker;
}
}  
}).addTo(unbegungroups);

var baseLayers = {

};
var overlays = {
		"Active": activegroups,
		"Inactive": inactivegroups,
		"Unbegun": unbegungroups
			};

	L.control.layers(baseLayers, overlays,{collapsed:false}).addTo(map);

  pins.addTo(map);
  pins2.addTo(map);
  pins3.addTo(map);
  slider.addTimelines(pins,pins2,pins3);

    })


}
