// This is now arranged by mainbrat.js.
// var bratLocation = 'webapp/public/brat';

// Color names used
var personEntityColor = '#7ED8E1';
var foodEntityColor = '#E19630';
var eatingEventColor = '#BB8FCE';

head.js(
    // External libraries
    bratLocation + '/client/lib/jquery.min.js',
    bratLocation + '/client/lib/jquery.svg.min.js',
    bratLocation + '/client/lib/jquery.svgdom.min.js',

    // brat helper modules
    bratLocation + '/client/src/configuration.js',
    bratLocation + '/client/src/util.js',
    bratLocation + '/client/src/annotation_log.js',
    bratLocation + '/client/lib/webfont.js',

    // brat modules
    bratLocation + '/client/src/dispatcher.js',
    bratLocation + '/client/src/url_monitor.js',
    bratLocation + '/client/src/visualizer.js'
);

var webFontURLs = [
    bratLocation + '/static/fonts/Astloch-Bold.woff',
    bratLocation + '/static/fonts/PT_Sans-Caption-Web-Regular.woff',
    bratLocation + '/static/fonts/Liberation_Sans-Regular.woff'
];

var collData = {
    entity_types: [ {
            "type"   : "Person",
            "labels" : ["Person"],
            "bgColor": personEntityColor,
            "borderColor": "darken"
        },
        {
            "type"   : "Food",
            "labels" : ["Food"],
            "bgColor": foodEntityColor,
            "borderColor": "darken"
        }
    ],

    event_types: [ {
        "type": "Eating",
        "labels": ["Eating"],
        "bgColor": eatingEventColor,
        "borderColor": "darken",
        "arcs": [
          {"type": "Person", "labels": ["person"], "borderColor": "darken", "bgColor":"pink"},
          {"type": "Food", "labels": ["food"], "borderColor": "darken", "bgColor":"pink"}
         ]
      }
    ]
};

// docData is initially empty.
var docData = {};

// These two values and function are now global so that they can be called from other js files.
var syntaxLiveDispatcher = null;

var mentionsLiveDispatcher = null;

function formDone(data) {
    console.log(data);
    syntaxLiveDispatcher.post('requestRenderData', [$.extend({}, data.syntax)]);
    odinLiveDispatcher.post('requestRenderData', [$.extend({}, data.odin)]);
    document.getElementById("mentions").innerHTML = data.mentions;
    document.getElementById("parse").innerHTML = data.parse;
    document.getElementById("overlay").style.display = "none";
}

head.ready(function() {

    syntaxLiveDispatcher = Util.embed('syntax',
        $.extend({'collection': null}, collData),
        $.extend({}, docData),
        webFontURLs
    );
    odinLiveDispatcher = Util.embed('odin',
        $.extend({'collection': null}, collData),
        $.extend({}, docData),
        webFontURLs
    );

    $('form').submit(function (event) {

        // stop the form from submitting the normal way and refreshing the page
        event.preventDefault();

        // collect form data
        var formData = {
            'text': $('textarea[name=text]').val()
        }

        if (!formData.text.trim()) {
            alert("Please write something.");
            return;
        }

        // show spinner
        document.getElementById("overlay").style.display = "block";

        // process the form
        $.ajax({
            type: 'GET',
            url: 'parseText',
            data: formData,
            dataType: 'json',
            encode: true
        })
        .fail(function () {
            // hide spinner
            document.getElementById("overlay").style.display = "none";
            alert("error");
        })
        .done(formDone);

    });
});
