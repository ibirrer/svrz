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

var loadLeagueInfo = function(leagueId) {
    var db = new PouchDB(leagueId);
    return db.get(leagueId);
}


// start elm app
var app = Elm.fullscreen(Elm.Razfaz,
        { scrapedLeagueHtml:
            { leagueId: "0"
            , games: []
            , ranking: [] },
          setLeagueData:
            { leagueId: "0"
            , games: []
            , ranking: [] },
          errorGetFromPouchDb: "none",
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
    // storeLeagueInfo(leagueInfo); // TODO: do this in elm
    app.ports.scrapedLeagueHtml.send(leagueInfo);
});


app.ports.scrapeGamesDetailsFromHtml.subscribe(function(gamesDetailsHtml) {
    // var result = scraper.scrapeDetail($, $.parseHTML(gamesDetailsHtml[7]))


    var result = gamesDetailsHtml.map(function(gameDetail) {
        try {
            return scraper.scrapeDetail($, $.parseHTML(gameDetail));
        } catch (err) {
            console.log(gameDetail);
            console.log(err);
            return "ERROR"
        }
        });

    console.log(result);
});

app.ports.getFromCouchDb.subscribe(function(leagueId) {
    loadLeagueInfo(leagueId)
        .then(function(leagueInfo) {
            app.ports.setLeagueData.send(leagueInfo);
        })
        .catch(function(err) {
            app.ports.errorGetFromPouchDb.send(err.reason);
        });
});

app.ports.urlHashChanged.send(location.hash);






