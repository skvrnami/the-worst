function getRandomSubarray(arr, size) {
    var shuffled = arr.slice(0), i = arr.length, temp, index;
    while (i--) {
        index = Math.floor((i + 1) * Math.random());
        temp = {song: shuffled[index][1], spotify_id: shuffled[index][3]};
        shuffled[index] = shuffled[i];
        shuffled[i] = temp;
    }
    return shuffled.slice(0, size);
}

function removePreviousList() {
    var elem = document.getElementsByTagName("ul");
    if(elem.length > 0){
        elem[0].parentNode.removeChild(elem[0]);
    }
}

function appendPlaylist(arr, playlistId){
    removePreviousList();
    
    var ul;
    if(arr.length === 0){
        ul = document.createElement("ul");
        var noData = document.createTextNode("Nenalezena žádná data vyhovující zadaným podmínkám.");
        ul.appendChild(noData);
    }else{
        ul = document.createElement("ul");
    }
    
    document.getElementById(playlistId).appendChild(ul);
    for (i = 0; i < arr.length; i++) {
        var node = document.createElement("li");
        
        var a = document.createElement('a');
        var linkText = document.createTextNode(arr[i].song);
        a.appendChild(linkText);
        a.title = arr[i].song;
        a.href = "https://www.youtube.com/results?search_query=" + arr[i].song.replace(/&/g, "");
        a.target = "_blank";
        
        document.getElementsByTagName("ul")[0].appendChild(node).appendChild(a);
        
        if(arr[i].spotify_id != "NA"){
            var spotify_div = document.createElement("span");
            var spotify_a = document.createElement("a");
            var img = document.createElement('img');
            img.src = "spotify_icon_gray.svg";
            img.width = "20";
            img.height = "20";
            img.setAttribute("style", "padding-left: 5px;vertical-align: middle;");
            spotify_a.appendChild(img);
            spotify_a.title = arr[i].song;
            spotify_a.href = "https://open.spotify.com/track/" + arr[i].spotify_id;
            spotify_a.target = "_blank";
            spotify_div.appendChild(spotify_a);
            document.getElementsByTagName("ul")[0].appendChild(node).appendChild(spotify_div);
        }
    }
}
