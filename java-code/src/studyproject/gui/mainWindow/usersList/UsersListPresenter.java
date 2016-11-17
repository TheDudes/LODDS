package studyproject.gui.mainWindow.usersList;

import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class UsersListPresenter implements Initializable{

	@FXML TextField usersSearch;
	@FXML ListView<UserInfo> usersListView;
	@Inject UsersListModel userListsModel;
	
	@Override
	public void initialize(URL location, ResourceBundle resources) {
		ObservableList<UserInfo> users = FXCollections.observableArrayList();
		userListsModel.setUsers(users);
	}

}
