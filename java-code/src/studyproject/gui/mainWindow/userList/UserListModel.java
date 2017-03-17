package studyproject.gui.mainWindow.userList;

import javax.annotation.PostConstruct;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.gui.introduction.IntroductionInterface;

public class UserListModel implements IntroductionInterface {
	private ObservableList<UserInfo> users;
	private ObjectProperty<UserInfo> selectedUser;

	@PostConstruct
	public void init() {
		users = FXCollections.observableArrayList();
		selectedUser = new SimpleObjectProperty<UserInfo>();
	}

	public ObservableList<UserInfo> getUsers() {
		return users;
	}

	public void setUsers(ObservableList<UserInfo> users) {
		this.users = users;
	}

	public ObjectProperty<UserInfo> getSelectedUser() {
		return selectedUser;
	}

	public void setSelectedUser(ObjectProperty<UserInfo> selectedUser) {
		this.selectedUser = selectedUser;
	}

	@Override
	public String getViewDiscription() {
		return "This is the User List Discription";
	}

	@Override
	public String getViewTitle() {
		return "User List";
	}
}
