package studyproject.gui.mainWindow.usersList;

import java.net.URL;
import java.util.ResourceBundle;
import javax.inject.Inject;
import javafx.application.Platform;
import javafx.collections.ListChangeListener;
import javafx.collections.transformation.FilteredList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import studyproject.App;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.gui.mainWindow.MainWindowModel;
import studyproject.gui.mainWindow.topMenu.TopMenuPresenter;
import studyproject.gui.mainWindow.topMenu.TopMenuView;

public class UsersListPresenter implements Initializable {

	@FXML
	TextField usersSearch;
	@FXML
	Button refreshUsers;
	@FXML
	ListView<UserInfo> usersListV;
	@Inject
	UsersListModel userListModel;
	@Inject
	MainWindowModel mainWindowModel;

	private FilteredList<UserInfo> filteredUserList;

	private final Image refreshImage = new Image(getClass().getResourceAsStream("/studyproject/resources/reload.png"),
			16, 16, true, true);
	private TopMenuPresenter topMenuPresenter = (TopMenuPresenter) (new TopMenuView()).getPresenter();

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		linkLoddsUserList();
		addUserListMouseClickSelection();
		filteredUserList = new FilteredList<UserInfo>(userListModel.getUsers(), s -> true);
		usersListV.setItems(filteredUserList);
		addUsersSearchListener();
		refreshUsers.setOnAction(e -> refreshUsers());
		if (Boolean.valueOf(App.properties.getProperty("icons"))) {
			addListViewCellFactory();
			refreshUsers.setGraphic(new ImageView(refreshImage));
			refreshUsers.setText(null);
		}
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

	private void addListViewCellFactory() {
		usersListV.setCellFactory(param -> new UsersListCell(topMenuPresenter));
	}

	private void addUsersSearchListener() {
		usersSearch.textProperty().addListener(l -> {
			if (usersSearch.textProperty().get() == null || usersSearch.textProperty().get().isEmpty()) {
				filteredUserList.setPredicate(s -> true);
			} else {
				filteredUserList.setPredicate(s -> s.getUserName().contains(usersSearch.textProperty().get()));
			}
		});
	}

	private void refreshUsers() {
		mainWindowModel.getLodds().getLoddsModel().getClientList().clear();
	}

}
