package studyproject.gui.mainWindow.usersList;

import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
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
		addSelectionListener();
		filteredList = new FilteredList<UserInfo>(userListModel.getUsers(), s -> true);
		usersListV.setItems(filteredList);
		addUsersSearchListener();
	}

	private void addSelectionListener() {
		usersListV.getSelectionModel().selectedItemProperty().addListener(new ChangeListener<UserInfo>() {
			public void changed(ObservableValue<? extends UserInfo> observable, UserInfo oldValue, UserInfo newValue) {
				userListModel.getSelectedUser().set(newValue);
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
			if (usersSearch.textProperty().get() == null | usersSearch.textProperty().get().isEmpty()) {
				filteredList.setPredicate(s -> true);
			} else {
				filteredList.setPredicate(s -> s.getUserName().contains(usersSearch.textProperty().get()));
			}
		});
	}

}
