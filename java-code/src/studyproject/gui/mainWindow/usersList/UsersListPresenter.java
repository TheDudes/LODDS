package studyproject.gui.mainWindow.usersList;

import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;

import javafx.application.Platform;
import javafx.beans.property.ListProperty;
import javafx.beans.property.SimpleListProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.gui.mainWindow.MainWindowModel;

public class UsersListPresenter implements Initializable {

	@FXML
	TextField usersSearch;
	@FXML
	ListView<UserInfo> usersListV;
	@Inject
	UsersListModel userListModel;
	@Inject
	MainWindowModel mainWindowModel;
	private ObservableList<UserInfo> users;
	protected ListProperty<UserInfo> listProperty = new SimpleListProperty<>();

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		users = FXCollections.observableArrayList();
		users.addAll(mainWindowModel.getLodds().getLoddsModel().getClientList());
		userListModel.setUsers(users);
		linkLoddsUserList();
		usersListV.setItems(users);
		addSelectionListener();
	}

	private void addSelectionListener() {
		usersListV.getSelectionModel().selectedItemProperty().addListener(new ChangeListener<UserInfo>() {
			public void changed(ObservableValue<? extends UserInfo> observable, UserInfo oldValue, UserInfo newValue) {
				userListModel.setSelectedUser(newValue);
			}
		});
	}

	private void linkLoddsUserList() {
		mainWindowModel.getLodds().getLoddsModel().getClientList().addListener(new ListChangeListener<UserInfo>() {

			@Override
			public void onChanged(Change<? extends UserInfo> c) {

				while (c.next()) {
					for (UserInfo client : c.getAddedSubList()) {
						Platform.runLater(new Runnable() {
							@Override
							public void run() {
								users.add(client);
							}
						});
					}

					for (UserInfo client : c.getRemoved()) {
						Platform.runLater(new Runnable() {
							@Override
							public void run() {
								users.remove(client);
							}
						});
					}

				}
			}

		});
	}

}
