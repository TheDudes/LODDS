package studyproject.gui.mainWindow.usersList;

import javafx.collections.ObservableList;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class UsersListModel {
	
	private ObservableList<UserInfo> users;

	public ObservableList<UserInfo> getUsers() {
		return users;
	}

	public void setUsers(ObservableList<UserInfo> users) {
		this.users = users;
	}
	
	
}
