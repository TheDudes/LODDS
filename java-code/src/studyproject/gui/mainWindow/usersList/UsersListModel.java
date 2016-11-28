package studyproject.gui.mainWindow.usersList;

import javax.annotation.PostConstruct;


import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class UsersListModel {
	private ObservableList<UserInfo> users;
	private UserInfo selectedUser;
	
	@PostConstruct
	public void init(){
		users = FXCollections.observableArrayList();
		selectedUser = null;
	}
	
	
	
	public UserInfo getSelectedUser() {
		return selectedUser;
	}



	public void setSelectedUser(UserInfo selectedUser) {
		this.selectedUser = selectedUser;
	}



	public ObservableList<UserInfo> getUsers() {
		return users;
	}

	public void setUsers(ObservableList<UserInfo> users) {
		this.users = users;
	}
	
	
}
