var sock;

window.onload = function ()
{
    sock = io();

    sock.on("msg", function (msg) { console.log(msg); });

    sock.on("error", function (errMsg) {
	console.log("Error: " + errMsg);
	alert("Error: " + errMsg);
	sock.disconnect();
    });
};

function doSend()
{
    var box = document.getElementById("input");
    var msg = box.value;
    box.value = "";
    
    sock.emit("msg", msg);
    
}
