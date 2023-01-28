// This is now arranged by mainbrat.js.
// var bratLocation = 'webapp/public/brat';

// Color names used
var baseConceptColor = '#CCD1D1';
var increaseConceptColor = '#BBDC90';
var decreaseConceptColor = '#FC5C38';
var positiveChangeConceptColor = '#7ED8E1';
var negativeChangeConceptColor = '#E19630';
var quantifierColor = '#AED6F1';
var quantifiedConceptColor = '#85C1E9';
var causalEventColor = '#BB8FCE';
var correlationEventColor = '#F7DC6F';
var posAffectEventColor = '#7EE1C5';
var negAffectEventColor = '#EE86D1';
var timeExpressionColor = '#FFA500'
var geoLocationColor = '#FFA500'


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
        "type"   : "Quantifier",
        "labels" : ["Quantifier", "Quant"],
        // Blue is a nice colour for a person?
        "bgColor": quantifierColor,
        // Use a slightly darker version of the bgColor for the border
        "borderColor": "darken"
    },
    {
            "type"   : "Concept",
            "labels" : ["Concept", "Conc"],
            // Blue is a nice colour for a person?
            //"bgColor": "thistle",
            "bgColor": baseConceptColor,
            // Use a slightly darker version of the bgColor for the border
            "borderColor": "darken"
        },
        {
            "type"   : "Concept-Inc",
            "labels" : ["Concept", "Conc"],
            // Blue is a nice colour for a person?
            //"bgColor": "thistle",
            "bgColor": increaseConceptColor,
            // Use a slightly darker version of the bgColor for the border
            "borderColor": "darken"
        },
        {
            "type"   : "Concept-Dec",
            "labels" : ["Concept", "Conc"],
            // Blue is a nice colour for a person?
            //"bgColor": "thistle",
            "bgColor": decreaseConceptColor,
            // Use a slightly darker version of the bgColor for the border
            "borderColor": "darken"
        },
        {
            "type"   : "Concept-Pos",
            "labels" : ["Concept-Pos", "Conc"],
            // Blue is a nice colour for a person?
            //"bgColor": "thistle",
            "bgColor": positiveChangeConceptColor,
            // Use a slightly darker version of the bgColor for the border
            "borderColor": "darken"
        },
        {
            "type"   : "Concept-Neg",
            "labels" : ["Concept-Neg", "Conc"],
            // Blue is a nice colour for a person?
            //"bgColor": "thistle",
            "bgColor": negativeChangeConceptColor,
            // Use a slightly darker version of the bgColor for the border
            "borderColor": "darken"
        },
        {
            "type"   : "Concept-Quant",
            "labels" : ["Concept", "Conc"],
            // Blue is a nice colour for a person?
            //"bgColor": "thistle",
            "bgColor": quantifiedConceptColor,
            // Use a slightly darker version of the bgColor for the border
            "borderColor": "darken"
        },

	// --------------------------- Context -------------------------------------
	{
            "type"   : "TimeExpression",
            "labels" : ["TimeExpression", "TIMEX"],
            // Blue is a nice colour for a person?
            //"bgColor": "thistle",
            "bgColor": timeExpressionColor,
            // Use a slightly darker version of the bgColor for the border
            "borderColor": "darken"
        },
        {
            "type"   : "GeoLocation",
            "labels" : ["GeoLocation", "GEOLOC"],
            // Blue is a nice colour for a person?
            //"bgColor": "thistle",
            "bgColor": geoLocationColor,
            // Use a slightly darker version of the bgColor for the border
            "borderColor": "darken"
         },
     // --------------------------- Param -------------------------------------
     {
      "type": "FarmSize",
      "labels":  ["FarmSize_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
     },
     {
      "type": "FertilizerUse",
      "labels":  ["FertilizerUse_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
     },
     {
      "type": "FertilizerPrice",
      "labels":  ["FertilizerPrice_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
     },
     {
      "type": "HerdSize",
      "labels":  ["HerdSize_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
     },
     {
       "type": "HouseholdSize",
       "labels":  ["HouseholdSize_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
     },
     {
       "type": "Irrigation",
       "labels":  ["Irrigation_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
     },
     {
      "type": "Labor",
      "labels":  ["Labor_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
    },
     {
      "type": "PestDisease",
      "labels":  ["PestDisease_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
     },
     {
       "type": "Productivity",
       "labels":  ["Productivity_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
     },
     {
       "type": "Soil",
       "labels":  ["Soil_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
     },
     {
      "type": "Subsidy",
      "labels":  ["Subsidy_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
      },
      {
        "type": "Water",
        "labels":  ["Water_PARAM"],
        "bgColor": "yellow",
        "borderColor": "darken"
      },
      {
       "type": "Government",
       "labels":  ["Government_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
      },
      {
       "type": "Climate",
       "labels":  ["Climate_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
      },
      {
       "type": "Poverty",
       "labels":  ["Poverty_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
      },
      {
       "type": "Economy",
       "labels":  ["Economy_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
      },
      {
       "type": "Crop",
       "labels":  ["Crop_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
      },
      {
       "type": "Pollution",
       "labels":  ["Pollution_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
      },
      {
        "type": "Drought",
        "labels":  ["Drought_PARAM"],
        "bgColor": "yellow",
        "borderColor": "darken"
      },
      // --------------------------- AGROVOC -------------------------------------
     {
       "type": "Animal",
       "labels":  ["Animal_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Application",
       "labels":  ["Application_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Biological",
       "labels":  ["Biological_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Breeding",
       "labels":  ["Breeding_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Chemical",
       "labels":  ["Chemical_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Computer",
       "labels":  ["Computer_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Equipment",
       "labels":  ["Equipment_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Feeding",
       "labels":  ["Feeding_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Fishing",
       "labels":  ["Fishing_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Groups",
       "labels":  ["Groups_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "HumanResources",
       "labels":  ["HumanResources_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Information",
       "labels":  ["Information_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Measure",
       "labels":  ["Measure_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Natural",
       "labels":  ["Natural_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Plant",
       "labels":  ["Plant_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Population",
       "labels":  ["Population_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Research",
       "labels":  ["Research_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Site",
       "labels":  ["Site_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Stages",
       "labels":  ["Stages_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Technology",
       "labels":  ["Technology_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     }

    ],

    event_types: [
      {
        "type": "Increase",
        "labels": ["INC"],
        "bgColor": "lightgreen",
        "borderColor": "darken",
        "arcs": [
            {"type": "theme", "labels": ["theme"], "borderColor": "darken", "bgColor":"violet"},
            {"type": "quantifier", "labels": ["quant"], "borderColor": "darken", "bgColor":"violet"}
        ]
      },

      {
        "type": "Decrease",
        "labels": ["DEC"],
        "bgColor": "red",
        "borderColor": "darken",
        "arcs": [
            {"type": "theme", "labels": ["theme"], "borderColor": "darken", "bgColor":"violet"},
            {"type": "quantifier", "labels": ["quant"], "borderColor": "darken", "bgColor":"violet"}
        ]
      },

      {
        "type": "Causal",
        "labels": ["CAUSAL"],
        "bgColor": causalEventColor,
        "borderColor": "darken",
        "arcs": [
          {"type": "cause", "labels": ["cause"], "borderColor": "darken", "bgColor":"pink"},
          {"type": "effect", "labels": ["effect"], "borderColor": "darken", "bgColor":"pink"}
         ]
      },

      {
        "type": "Correlation",
        "labels": ["CORRELATION"],
        "bgColor": correlationEventColor,
        "borderColor": "darken",
        "arcs": [
          {"type": "cause", "labels": ["cause"], "borderColor": "darken", "bgColor":"pink"},
          {"type": "effect", "labels": ["effect"], "borderColor": "darken", "bgColor":"pink"}
         ]
      },
      {
          "type": "PositiveAffect",
          "labels": ["POS_AFFECT"],
          "bgColor": posAffectEventColor,
          "borderColor": "darken",
          "arcs": [
            {"type": "cause", "labels": ["cause"], "borderColor": "darken", "bgColor":"pink"},
            {"type": "effect", "labels": ["effect"], "borderColor": "darken", "bgColor":"pink"}
           ]
      },
      {
        "type": "NegativeAffect",
        "labels": ["NEG_AFFECT"],
        "bgColor": negAffectEventColor,
        "borderColor": "darken",
        "arcs": [
          {"type": "cause", "labels": ["cause"], "borderColor": "darken", "bgColor":"pink"},
          {"type": "effect", "labels": ["effect"], "borderColor": "darken", "bgColor":"pink"}
         ]
      }
    ]
};

// docData is initially empty.
var docData = {};

// These two values and function are now global so that they can be called from other js files.
var syntaxLiveDispatcher = null;

var eidosMentionsLiveDispatcher = null;

function formDone(data) {
    console.log(data);
    syntaxLiveDispatcher.post('requestRenderData', [$.extend({}, data.syntax)]);
    eidosMentionsLiveDispatcher.post('requestRenderData', [$.extend({}, data.eidosMentions)]);
    document.getElementById("groundedAdj").innerHTML = data.groundedAdj;
    document.getElementById("parse").innerHTML = data.parse;
    // hide spinner
    document.getElementById("overlay").style.display = "none";
}

head.ready(function() {

    syntaxLiveDispatcher = Util.embed('syntax',
        $.extend({'collection': null}, collData),
        $.extend({}, docData),
        webFontURLs
    );
    eidosMentionsLiveDispatcher = Util.embed('eidosMentions',
        $.extend({'collection': null}, collData),
        $.extend({}, docData),
        webFontURLs
    );


    $('form').submit(function (event) {

        // stop the form from submitting the normal way and refreshing the page
        event.preventDefault();

        // collect form data
        var formData = {
            'text': $('textarea[name=text]').val(),
            'cagRelevantOnly': $('input[name=cagRelevantOnly]').is(':checked')
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
