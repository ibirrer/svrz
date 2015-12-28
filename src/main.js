/*jslint node: true */
"use strict";

var scraper = require("svrz-scraper");
var $ = require('jquery');
var PouchDB = require('pouchdb');


var storeLeagueInfo = function(leagueInfo) {
    // _id needed by pouchdb
    leagueInfo._id = leagueInfo.leagueId;
    var db = new PouchDB(leagueInfo.leagueId);

    db.get(leagueInfo.leagueId)
        .then(function(doc) {
            // update
            leagueInfo._rev = doc._rev;
            db.put(leagueInfo)
                .then(function(response) {
                    console.log("updated");
                }).catch(function(err) {
                    console.log("error updating");
                });
        }).catch(function(err) {
            // insert
            db.put(leagueInfo)
                .then(function(response) {
                    console.log("inserted");
                }).catch(function(err) {
                    console.log("error inserting");
                });
        });
}

var loadLeagueInfo = function(leagueId, callback) {
    var db = new PouchDB(leagueId);

    db.get(leagueId)
        .then(function(leagueInfo) {
            // update
            callback(leagueInfo);
        }).catch(function(err) {
            console.log("failed to load leagueInfo. ", err);
        });
}


// start elm app
var app = Elm.fullscreen(Elm.Razfaz,
        { loadData:
            { leagueId: "0"
            , games: []
            , ranking: [] },
          urlHashChanged: ""
        });

// ----------------------------------------------------------------------------
// wire elm ports
// ----------------------------------------------------------------------------
window.onhashchange = function() {
    console.log(location.hash);
    app.ports.urlHashChanged.send(location.hash);
}


app.ports.scrapeSvrz.subscribe(function(leagueHtml) {
    console.log("scraping league...");
    var leagueInfo = scraper.scrape($, $.parseHTML(leagueHtml));
    storeLeagueInfo(leagueInfo);
    app.ports.loadData.send(leagueInfo);
});

app.ports.loadLeagueInfo.subscribe(function(leagueId) {
    loadLeagueInfo(leagueId, function(leagueInfo) {
        app.ports.loadData.send(leagueInfo);
    });
});

app.ports.urlHashChanged.send(location.hash);






