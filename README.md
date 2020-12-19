# README

## The Worst Thing Ever to Happen to Music

Kód vytvořený na hackathonu [Hackuj stát v3.0](https://hackujstat.cz/) 
nad [playlisty Českého rozhlasu](https://data.irozhlas.cz/opendata/) a 
daty z [last.fm](https://www.last.fm/home) (hudební žánry) a [Wikipedie](https://cs.wikipedia.org/wiki/Hlavn%C3%AD_strana) (datum narození a úmrtí interpretů).  

Původně jsem chtěl udělat analýzu, co se na jaké stanici vysílá. 
Ale po zběžném pohledu na data jsem se rozhodl, že to radši nechci vědět.  

Místo toho jsem vytvořil [generátor náhodných playlistů](http://skvrnami.github.io/playlist/)
ze skladeb hraných na Českém rozhlasu. Defaultně to generuje skladby hrané
na [Radiu Wave](https://wave.rozhlas.cz/), ale pokud chcete trýznit sebe nebo 
někoho jiného, můžete si zkusit vygenerovat playlist třeba z [Dvojky](https://dvojka.rozhlas.cz/).  

### Usage

`Rscript 00_get_radio_data.R 2020-12-01 2020-12-07`  
`Rscript 00_get_spotify_data.R`  
`Rscript 00_get_wiki_data.R`
`Rscript 01_process_data.R`
`Rscript 02_best_new_music.R radiowave`
