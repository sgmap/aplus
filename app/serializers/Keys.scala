package serializers

/** Contains string values shared everywhere,
  * but which need to be the same across the code.
  * eg: a name value in the view and the corresponding key in the Mapping
  */
object Keys {

  //
  // By Model
  //

  object Application {
    val areaId: String = "areaId"
  }

  object User {
    val sharedAccount: String = "sharedAccount"
  }

  //
  // By Usage
  //

  object Session {
    val userId: String = "userId"
  }

  //.queryString .getQueryString
  object QueryParam {

    // Login

    val token: String = "token"
    val key: String = "key"
    val path: String = "path"
    val email: String = "email"

    // Application

    val areaId: String = "areaId"
    val action: String = "action"
    val uniquementFs: String = "uniquement-fs"
    val usefulness: String = "usefulness"
    val filterIsOpen: String = "filtre-ouverte"

    // Users

    val vue: String = "vue"
    val rows: String = "rows"
    val limit: String = "limit"
    val fromUserId: String = "fromUserId"
    val date: String = "date"
  }

}
