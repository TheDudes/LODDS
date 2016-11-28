package studyproject.gui.mainWindow.usersList;

import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;


import javafx.beans.property.ListProperty;
import javafx.beans.property.SimpleListProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.gui.mainWindow.MainWindowModel;

public class UsersListPresenter implements Initializable{

	@FXML TextField usersSearch;
	@FXML ListView<UserInfo> usersListV;
	@Inject UsersListModel userListsModel;
	@Inject MainWindowModel mainwindowModel;
	private ObservableList<UserInfo> users;
	protected ListProperty<UserInfo> listProperty = new SimpleListProperty<>();
	@Override
	public void initialize(URL location, ResourceBundle resources) {
		users = FXCollections.observableArrayList(mainwindowModel.getLodds().getUsers());
		
		userListsModel.setUsers(users);
		listProperty.set(users);
		usersListV.itemsProperty().bind(listProperty);
		//		usersListView.setCellFactory(param -> new ListCell<UserInfo>() {
//		    @Override
//		    protected void updateItem(UserInfo item, boolean empty) {
//		        super.updateItem(item, empty);
//
//		        if (empty || item == null || item.getIpAddress() == null || item.getUserName() == null) {
//		            setText(null);
//		        } else {
//		            setText(item.toString());
//		        }
//		    }
//		});
//		usersListView.setItems(userListsModel.getUsers());
	}

}
