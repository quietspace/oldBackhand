var sock;

window.onload = function ()
{
    sock = io();

    sock.on("msg", function (msg) { console.log(msg); });
};

function doSend()
{
    var box = document.getElementById("input");
    var msg = box.value;
    box.value = "";
    
    sock.emit("msg", msg);
    
}
