import { Elm } from './Main.elm'
import logger from './logger.js'

window.addEventListener('load', function () {
  const app =
    Elm.Main.init({ node: document.getElementById('elm') })

  logger(app)
})
