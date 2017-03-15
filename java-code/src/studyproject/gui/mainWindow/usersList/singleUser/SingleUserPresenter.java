package studyproject.gui.mainWindow.usersList.singleUser;

import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;

import javafx.application.Platform;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.transformation.FilteredList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.util.Callback;
import studyproject.App;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.gui.mainWindow.MainWindowModel;
import studyproject.gui.mainWindow.topMenu.TopMenuPresenter;
import studyproject.gui.mainWindow.topMenu.TopMenuView;
import studyproject.gui.mainWindow.usersList.UsersListModel;

public class SingleUserPresenter implements Initializable {
	@Inject
	UsersListModel usersListModel;

	@Inject
	MainWindowModel mainWindowModel;

	@FXML
	TableView<UserInfo> userTableView;

	@FXML
	TableColumn<UserInfo, Boolean> sendFileCol;
	@FXML
	TableColumn<UserInfo, String> nameCol;
	@FXML
	TableColumn<UserInfo, Long> loadCol;
	@FXML
	TableColumn<UserInfo, Boolean> iconCol;
	@FXML
	TextField usersSearch;
	@FXML
	Button refreshUsers;

	private TopMenuPresenter topMenuPresenter;
	private Image sendFileImage;
	private Image userIconImage;
	private Image refreshImage;

	private FilteredList<UserInfo> filteredUserList;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		userTableView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
		addUserListMouseClickSelection();
		linkLoddsUserList();
		topMenuPresenter = (TopMenuPresenter) (new TopMenuView()).getPresenter();
		sendFileImage = null;
		userIconImage = null;
		filteredUserList = new FilteredList<UserInfo>(usersListModel.getUsers(), s -> true);
		if (Boolean.valueOf(App.properties.getProperty("icons"))) {
			sendFileImage = new Image(getClass().getResourceAsStream(App.ICON_PATH + "send-file.png"), 20, 20, true,
					true);
			userIconImage = new Image(getClass().getResourceAsStream(App.ICON_PATH + "user.png"), 20, 20, true, true);
			refreshImage = new Image(getClass().getResourceAsStream(App.ICON_PATH + "reload.png"), 16, 16, true, true);
			refreshUsers.setGraphic(new ImageView(refreshImage));
			refreshUsers.setText(null);
		}
		refreshUsers.setOnAction(e -> refreshUsers());
		userTableView.setItems(filteredUserList);
		nameCol.setCellValueFactory(new PropertyValueFactory<UserInfo, String>("userName"));
		loadCol.setCellValueFactory(new PropertyValueFactory<UserInfo, Long>("load"));
		setIconFactory();
		setSendFileFactory();

		addUsersSearchListener();
	}

	private void addUserListMouseClickSelection() {
		userTableView.setOnMouseClicked(e -> {
			if (usersListModel.getSelectedUser().getValue() == null || !usersListModel.getSelectedUser().get()
					.equals(userTableView.getSelectionModel().getSelectedItem())) {
				usersListModel.getSelectedUser().set(userTableView.getSelectionModel().getSelectedItem());
			}
		});
	}

	private void addUsersSearchListener() {
		usersSearch.textProperty().addListener(l -> {
			if (usersSearch.textProperty().get() == null || usersSearch.textProperty().get().isEmpty()) {
				filteredUserList.setPredicate(s -> true);
			} else {
				filteredUserList.setPredicate(
						s -> s.getUserName().toLowerCase().contains(usersSearch.textProperty().get().toLowerCase()));
				userTableView.refresh();
			}
		});
	}

	private void refreshUsers() {
		mainWindowModel.getLodds().getLoddsModel().getClientList().clear();
	}

	private void setIconFactory() {
		iconCol.setCellValueFactory(
				new Callback<TableColumn.CellDataFeatures<UserInfo, Boolean>, ObservableValue<Boolean>>() {
					@Override
					public ObservableValue<Boolean> call(TableColumn.CellDataFeatures<UserInfo, Boolean> p) {
						return new SimpleBooleanProperty(p.getValue() != null);
					}
				});
		iconCol.setCellFactory(new Callback<TableColumn<UserInfo, Boolean>, TableCell<UserInfo, Boolean>>() {
			@Override
			public TableCell<UserInfo, Boolean> call(TableColumn<UserInfo, Boolean> param) {
				return new TableCell<UserInfo, Boolean>() {
					private ImageView imageView = new ImageView(userIconImage);

					@Override
					protected void updateItem(Boolean userInfo, boolean empty) {
						super.updateItem(userInfo, empty);
						if (!empty && userInfo != null && imageView != null) {
							setGraphic(imageView);
						}
					}
				};
			}
		});

	}

	private void setSendFileFactory() {
		sendFileCol.setCellValueFactory(
				new Callback<TableColumn.CellDataFeatures<UserInfo, Boolean>, ObservableValue<Boolean>>() {
					@Override
					public ObservableValue<Boolean> call(TableColumn.CellDataFeatures<UserInfo, Boolean> p) {
						return new SimpleBooleanProperty(p.getValue() != null);
					}
				});
		sendFileCol.setCellFactory(new Callback<TableColumn<UserInfo, Boolean>, TableCell<UserInfo, Boolean>>() {
			@Override
			public TableCell<UserInfo, Boolean> call(TableColumn<UserInfo, Boolean> p) {
				return new SendFileButtonCell(sendFileImage, topMenuPresenter);
			}
		});

	}

	private void linkLoddsUserList() {
		mainWindowModel.getLodds().getLoddsModel().getClientList().addListener(new ListChangeListener<UserInfo>() {
			@Override
			public void onChanged(Change<? extends UserInfo> c) {

				while (c.next()) {
					for (UserInfo client : c.getRemoved()) {
						if (!usersListModel.getUsers().contains(client))
							continue;
						Platform.runLater(() -> {
							usersListModel.getUsers().remove(client);
							userTableView.refresh();
						});
					}
					for (UserInfo client : c.getAddedSubList()) {
						Platform.runLater(() -> {
							usersListModel.getUsers().add(client);
							userTableView.refresh();
						});
					}

				}
			}

		});
	}

}
