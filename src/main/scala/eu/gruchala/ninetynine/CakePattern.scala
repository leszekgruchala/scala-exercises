package eu.gruchala.ninetynine

object CakePattern {

  trait UserRepositoryComponent {

    def userRepository: UserRepository

    trait UserRepository {

      def find(name: String): String
    }

  }

  trait UserRepositoryImpl extends UserRepositoryComponent {

    def userRepository = new UserRepositoryImpl

    class UserRepositoryImpl extends UserRepository {

      override def find(name: String): String = "franek"
    }

  }

  trait UserAuthorisationComponent {

    def userAuthorisation: UserAuthorisation

    trait UserAuthorisation {

      def authorise(name: String): Boolean
    }

  }

  trait UserAuthorisationComponentImpl extends UserAuthorisationComponent {

    this: UserRepositoryComponent =>

    def userAuthorisation = new UserAuthorisationImpl

    class UserAuthorisationImpl extends UserAuthorisation {

      override def authorise(name: String): Boolean = {
        userRepository.find(name)
        true
      }
    }

  }

  val env = new UserAuthorisationComponentImpl with UserRepositoryImpl
  env.userAuthorisation.authorise("Franek")

}
