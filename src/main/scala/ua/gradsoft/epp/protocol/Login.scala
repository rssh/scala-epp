package ua.gradsoft.epp.protocol



case class Login(
                  clId: String,
                  pw: String,
                  newPw: Option[String],
                  options: LoginOptions,
                  svcs: Svcs
                ) extends Command

object Login {




}

case class LoginOptions(
                          version: Option[String],
                          lang: Option[String],
                        )

case class Svcs(svcs:Seq[String])