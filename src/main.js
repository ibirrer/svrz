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
            return db.put(leagueInfo)
                .then(function(response) {
                    console.log("inserted");
                }).catch(function(err) {
                    console.log("error inserting");
                });
        });
}

// start elm app
var app = Elm.fullscreen(Elm.Main,
        { scrapedLeagueHtml:
            { leagueId: "0"
            , games: []
            , ranking: [] },
          setLeagueData:
            { leagueId: "0"
            , games: []
            , ranking: [] },
          errorGetFromPouchDb: "none",
          scrapedGamesDetailsFromHtml: [],
          urlHashChanged: ""
        });

// ----------------------------------------------------------------------------
// wire elm ports
// ----------------------------------------------------------------------------
window.onhashchange = function() {
    app.ports.urlHashChanged.send(location.hash);
}


app.ports.scrapeLeagueFromHtml.subscribe(function(leagueHtml) {
    var leagueInfo = scraper.scrape($, $.parseHTML(leagueHtml));
    app.ports.scrapedLeagueHtml.send(leagueInfo);
});


app.ports.scrapeGamesDetailsFromHtml.subscribe(function(gamesDetailsHtml) {
    var result = gamesDetailsHtml.map(function(gameDetail) {
        try {
            return scraper.scrapeDetail($, $.parseHTML(gameDetail));
        } catch (err) {
            console.log(gameDetail);
            console.log(err);
            return "ERROR"
        }
        });
    app.ports.scrapedGamesDetailsFromHtml.send(result);
});

app.ports.getFromCouchDb.subscribe(function(leagueId) {
    var db = new PouchDB(leagueId);
    db.get(leagueId)
        .then(function(leagueInfo) {
            try {
                app.ports.setLeagueData.send(leagueInfo);
                return true;
            } catch (err) {
                return db.remove(leagueInfo)
                    .then(function() {
                        app.ports.errorGetFromPouchDb.send("leagueInfo schema changed");
                    });
            }
        })
        .catch(function(err) {
            app.ports.errorGetFromPouchDb.send(err.reason);
        });
});

app.ports.urlHashChanged.send(location.hash);
