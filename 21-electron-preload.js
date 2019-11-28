let remote = require("electron").remote

process.once("loaded", function () {
  window.electron = remote
})
