module Model.User where


type UserId = String

data User = User
	{ uName :: UserId
	, uHash :: String
	}
	deriving (Eq, Show)


