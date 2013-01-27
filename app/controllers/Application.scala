package controllers

import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.mvc.RequestHeader
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._

import scala.reflect.{ClassTag, classTag}

import models._
import views._

import jp.t2v.lab.play20.auth._

object Application extends AuthController with LoginLogout {

  def index = MaybeAuthenticated { implicit userOrLogin => implicit request =>
    Ok("Your new application is ready.")
  }

  val loginForm = Form {
    mapping("name" -> text, "password" -> text)(Account.authenticate)(_.map(u => (u.name, "")))
      .verifying("Invalid name or password", result => result.isDefined)
  }

  /**
   * Login form
   */
  def login = Action { implicit request =>
    Ok(views.html.login(loginForm))
  }

  /**
   * Logout page
   */
  def logout = Action { implicit request =>
    gotoLogoutSucceeded.flashing(
      "success" -> "You've been logged out"
    )
  }

  /**
   * Handle login form submission.
   */
  def authenticate = Action { implicit request =>
    Logger.info("authenticating user")
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.login(formWithErrors)),
      user => gotoLoginSucceeded(user.get.id.get)
    )
  }
  
}

trait AuthConfigImpl extends AuthConfig {

  /** 
   * A type that is used to identify a user.
   * `String`, `Int`, `Long` and so on. 
   */
  type Id = Long

  /** 
   * A type that represents a user in your application.
   * `User`, `Account` and so on.
   */
  type User = Account

  /** 
   * A type that is defined by every action for authorization.
   * This sample uses the following trait:
   *
   * sealed trait Permission
   * case object Administrator extends Permission
   * case object NormalUser extends Permission
   */
  type Authority = User => Boolean

  /**
   * A `ClassManifest` is used to retrieve an id from the Cache API.
   * Use something like this:
   */
  val idManifest: ClassTag[Id] = classTag[Id]

  /**
   * The session timeout in seconds
   */
  val sessionTimeoutInSeconds: Int = 36000

  /**
   * A function that returns a `User` object from an `Id`.
   * You can alter the procedure to suit your application.
   */
  def resolveUser(id: Id): Option[User] = Account.findById(id)

  /**
   * Where to redirect the user after a successful login.
   */
  def loginSucceeded(request: RequestHeader): PlainResult = {
    Logger.info("login succeeded")
    val uri = request.session.get("access_uri").getOrElse(routes.Application.index.url.toString)
    Redirect(uri).withSession(request.session - "access_uri")
  }

  /**
   * Where to redirect the user after logging out
   */
  def logoutSucceeded(request: RequestHeader): PlainResult = Redirect(routes.Application.login)

  /**
   * If the user is not logged in and tries to access a protected resource then redirct them as follows:
   */
  def authenticationFailed(request: RequestHeader): PlainResult = {
    Logger.info("authentication failed")
    Redirect(routes.Application.login).withSession("access_uri" -> request.uri)
  }

  /**
   * If authorization failed (usually incorrect password) redirect the user as follows:
   */
  def authorizationFailed(request: RequestHeader): PlainResult = Forbidden("no permission")

  /**
   * A function that determines what `Authority` a user has.
   * You should alter this procedure to suit your application.
   */
  def authorize(user: User, authority: Authority): Boolean = 
    (user.permission, authority) match {
      case (Administrator, _) => true
      case (NormalUser, _) if(authority(user)) => true
      case _ => false
    }
  

}

trait AuthController extends Controller with Auth with AuthConfigImpl {

  protected def IsAdmin = authorizedAction(isAdmin) _

  private def isAdmin(account: Account): Boolean = 
    account.permission == Administrator

  protected def IsAuthenticated = authorizedAction((a: Account) => true) _

  //protected def MaybeAuthenticated = optionalUserAction(BodyParsers.parse.anyContent) _

  /*
  private def maybeAuthenticated(u: Option[User])(req: Request[AnyContent])(f: Either[Form[Option[Account]], Account] => Request[AnyContent] => Result): Result =>  {
    implicit val userOrLogin:Either[Form[Option[Account]], Account] = maybeUser match {
      case Some(user) => Right(user)
      case _ => Left(Application.loginForm)
    }

    optionalUserAction(BodyParsers.parse.anyContent)(f)
  }
  */

  private def maybeAuthenticated(f: Either[Form[Option[Account]], Account] => Request[AnyContent] => Result): Action[AnyContent] = {
    def userOrLogin(req: Request[AnyContent]) = restoreUser(req) match {
      case Some(user) => Right(user)
      case _ => Left(Application.loginForm)
    }

    Action(BodyParsers.parse.anyContent)(req => f(userOrLogin(req))(req))
  }

  protected def MaybeAuthenticated = maybeAuthenticated _
      

}

