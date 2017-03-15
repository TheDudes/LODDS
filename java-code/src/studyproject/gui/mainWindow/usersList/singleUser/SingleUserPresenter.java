package studyproject.gui.mainWindow.usersList.singleUser;

import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.image.ImageView;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.gui.mainWindow.usersList.UsersListModel;

public class SingleUserPresenter implements Initializable {
	@Inject
	UsersListModel usersListModel;

	@FXML
	TableView<UserInfo> userTableView;

	@FXML
	TableColumn<UserInfo, Button> sendFileCol;
	@FXML
	TableColumn<UserInfo, String> nameCol;
	@FXML
	TableColumn<UserInfo, Long> LoadCol;
	@FXML
	TableColumn<UserInfo, ImageView> iconCol;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		System.out.println("is initialized");
		userTableView.setItems(usersListModel.getUsers());
		nameCol.setCellValueFactory(new PropertyValueFactory<UserInfo, String>("userName"));
	}

}
