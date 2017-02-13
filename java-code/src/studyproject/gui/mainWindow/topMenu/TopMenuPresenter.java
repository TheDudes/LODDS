package studyproject.gui.mainWindow.topMenu;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.ResourceBundle;

import javax.inject.Inject;

import javafx.concurrent.Task;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.stage.DirectoryChooser;
import javafx.stage.Modality;
import javafx.stage.Stage;
import studyproject.App;
import studyproject.API.Core.File.FileInfo;
import studyproject.gui.Core.Utils;
import studyproject.gui.mainWindow.MainWindowModel;
import studyproject.gui.mainWindow.usersList.UsersListModel;
import studyproject.gui.settingsWindow.SettingsWindowView;

public class TopMenuPresenter implements Initializable {

	@FXML
	Menu fileMenu;
	@FXML
	MenuItem settingsItem;
	@FXML
	MenuItem shareFolder;
	@FXML
	MenuItem sendFilesToUser;
	@FXML
	MenuItem sendFolderToUser;
	@Inject
	MainWindowModel mainWindowModel;
	@Inject
	UsersListModel usersListModel;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		// Menus
		fileMenu.setOnShowing(e -> fileMenuPressed());

		// Items
		settingsItem.setOnAction(e -> settingsItemPressed());
		shareFolder.setOnAction(e -> shareFolderPressed());
		sendFilesToUser.setOnAction(e -> sendFilesToUser());
		sendFolderToUser.setOnAction(e -> sendFolderToUser());
	}

	private void fileMenuPressed() {
		if (usersListModel.getSelectedUser().get() == null) {
			sendFilesToUser.setDisable(true);
			sendFolderToUser.setDisable(true);
		} else {
			sendFilesToUser.setDisable(false);
			sendFolderToUser.setDisable(false);
		}
	}

	private void shareFolderPressed() {
		Stage stage = new Stage();
		DirectoryChooser directoryChooser = new DirectoryChooser();
		File chosenFolder = directoryChooser.showDialog(stage);
		if (chosenFolder == null)
			return;

		Task<Void> shareFolderTask = new Task<Void>() {
			@Override
			protected Void call() throws Exception {
				mainWindowModel.getLodds().shareFolder(chosenFolder.getAbsolutePath());
				return null;
			}
		};
		stage.hide();

		Thread thread = new Thread(shareFolderTask);
		thread.setDaemon(true);
		thread.start();
	}

	private void settingsItemPressed() {
		Stage stage = new Stage();
		SettingsWindowView settingsView = new SettingsWindowView();
		stage.setScene(new Scene(settingsView.getView()));
		stage.initModality(Modality.APPLICATION_MODAL);
		stage.show();
	}
	
	private void sendFilesToUser() {
		String userName = usersListModel.getSelectedUser().getName();
		int timeout = Integer.parseInt((String) App.properties.get("getPermissionTimeout"));
		List<File> fileList = Utils.getChoosenMultipleFiles("Select File or Folder to share");
		for (File f : fileList) {
			sendSingleFileToUser(userName, timeout, f);
		}
	}
	
	private void sendFolderToUser() {
		String userName = usersListModel.getSelectedUser().getName();
		int timeout = Integer.parseInt((String) App.properties.get("getPermissionTimeout"));
		File chosenFolder = new File(Utils.getChoosenDirPath("Select File or Folder to share"));
		if (chosenFolder.isDirectory()) {
			sendDirectoryToUser(userName, timeout, chosenFolder);
		}
	}
	
	private void sendDirectoryToUser(String userName, long timeout, File directoryFolder) {
		for (File f : directoryFolder.listFiles()) {
			if (f.isDirectory()) {
				sendDirectoryToUser(userName, timeout, f);
			} else {
				sendSingleFileToUser(userName, timeout, f);
			}
		}
	}
	
	private void sendSingleFileToUser(String userName, long timeout, File file) {
		FileInfo fileInfo;
		try {
			fileInfo = new FileInfo(file.getPath(), file.getPath());
			mainWindowModel.getLodds().sendFileWP(userName, timeout, fileInfo);
		} catch (NoSuchAlgorithmException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}