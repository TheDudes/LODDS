package studyproject.gui.mainWindow.usersList;

import java.net.URL;
import java.util.ResourceBundle;


import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class UsersListPresenter implements Initializable{

	@FXML TextField usersSearch;
	@FXML ListView<UserInfo> usersListView;
	
	@Override
	public void initialize(URL location, ResourceBundle resources) {
		// TODO Auto-generated method stub
		
	}

}
