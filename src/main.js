/*jslint node: true */
"use strict";

var scraper = require("svrz-scraper");
var $ = require('jquery');

var url = scraper.urlFromLeagueId("9446");

$.get("https://crossorigin.me/" + url, function (body) {
    var leagueInfo = scraper.scrape($, $.parseHTML(body));

    var app = Elm.fullscreen(Elm.Razfaz,
        { loadData:
            { leagueId: "0"
            , games: []
            , ranking: [] }
            });

    app.ports.loadData.send(leagueInfo);
});


