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
		return "This is a simple list of users which are connected to the "
				+ "same network. You can simply select one user with a mouse "
				+ "click to see its shared folders in the FilesTree. "
				+ "You can also send single files to one specific user, "
				+ "just click the send icon next to the users name. "
				+ "There is a number which indicates the network-load of one "
				+ "user, users with a high number are already sending or "
				+ "downloading a big amount of bytes.";
	}

	@Override
	public String getViewTitle() {
		return "User List";
	}
}
