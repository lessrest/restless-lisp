let {protocol} = require("electron");

process.once("loaded", () => {
  // Allow window.fetch() to access app files
  // protocol.registerSchemesAsPrivileged([{
  //   scheme: "app", 
  //   privileges: {
  //     secure: true,
  //     bypassCSP: false,
  //     allowServiceWorkers: true,
  //     supportFetchAPI: true,
  //     corsEnabled: false,
  //   }
  // }])
})
