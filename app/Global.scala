import play.api._

import models._
import anorm._

object Global extends GlobalSettings {

  override def onStart(app: Application) {

    if (Account.findAll.isEmpty) {
      Seq(
        Account(None, "alice@example.com", "secret", "Alice", Administrator),
        Account(None, "bob@example.com", "secret", "Bob", NormalUser),
        Account(None, "chris@example.com", "secret", "Chris", NormalUser)
      ) foreach Account.create
    }

  }

}
