package studyproject.gui.mainWindow.usersList;

import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;

import javafx.application.Platform;
import javafx.collections.ListChangeListener;
import javafx.collections.transformation.FilteredList;
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

	private FilteredList<UserInfo> filteredList;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		linkLoddsUserList();
		addUserListMouseClickSelection();
		filteredList = new FilteredList<UserInfo>(userListModel.getUsers(), s -> true);
		usersListV.setItems(filteredList);
		addUsersSearchListener();
	}

	private void addUserListMouseClickSelection() {
		usersListV.setOnMouseClicked(e -> {
			if (userListModel.getSelectedUser().get() == null || !userListModel.getSelectedUser().get()
					.equals(usersListV.getSelectionModel().getSelectedItem())) {
				userListModel.getSelectedUser().set(usersListV.getSelectionModel().getSelectedItem());
			}
		});
	}

	private void linkLoddsUserList() {
		mainWindowModel.getLodds().getLoddsModel().getClientList().addListener(new ListChangeListener<UserInfo>() {

			@Override
			public void onChanged(Change<? extends UserInfo> c) {

				while (c.next()) {
					for (UserInfo client : c.getRemoved()) {
						if (!userListModel.getUsers().contains(client))
							continue;
						Platform.runLater(new Runnable() {
							@Override
							public void run() {
								userListModel.getUsers().remove(client);
							}
						});
					}
					for (UserInfo client : c.getAddedSubList()) {
						Platform.runLater(new Runnable() {
							@Override
							public void run() {
								userListModel.getUsers().add(client);
							}
						});
					}

				}
			}

		});
	}

	private void addUsersSearchListener() {
		usersSearch.textProperty().addListener(l -> {
			if (usersSearch.textProperty().get() == null || usersSearch.textProperty().get().isEmpty()) {
				filteredList.setPredicate(s -> true);
			} else {
				filteredList.setPredicate(s -> s.getUserName().contains(usersSearch.textProperty().get()));
			}
		});
	}

}
