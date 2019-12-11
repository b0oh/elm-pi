export default function (app) {
  function log(message) {
    console.log(message)
  }

  app.ports.log.subscribe(log)
}
