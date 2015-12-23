/*jslint node: true */
"use strict";

var scraper = require("svrz-scraper");
var $ = require('jquery');

var url = scraper.urlFromLeagueId("9446");

var app = Elm.fullscreen(Elm.Razfaz,
        { loadData:
            { leagueId: "0"
            , games: []
            , ranking: [] }
            });

app.ports.scrapeSvrz.subscribe(function(leagueHtml) {
    console.log("scraping league...");
    var leagueInfo = scraper.scrape($, $.parseHTML(leagueHtml));
    app.ports.loadData.send(leagueInfo);
});



