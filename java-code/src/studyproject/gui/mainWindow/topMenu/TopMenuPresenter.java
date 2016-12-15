package studyproject.gui.mainWindow.topMenu;

import java.io.File;
import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;

import javafx.concurrent.Task;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.MenuItem;
import javafx.stage.DirectoryChooser;
import javafx.stage.Modality;
import javafx.stage.Stage;
import studyproject.gui.mainWindow.MainWindowModel;
import studyproject.gui.settingsWindow.SettingsWindowView;

public class TopMenuPresenter implements Initializable {

	@FXML
	MenuItem settingsItem;
	@FXML
	MenuItem shareFolder;
	@Inject
	MainWindowModel mainWindowModel;

	public static Stage stage;
	private File chosenFolder;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		settingsItem.setOnAction(e -> settingsItemPressed());
		shareFolder.setOnAction(e -> shareFolderPressed());
	}

	private void shareFolderPressed() {
		stage = new Stage();
		DirectoryChooser directoryChooser = new DirectoryChooser();

		chosenFolder = directoryChooser.showDialog(stage);
		stage.hide();
		if (chosenFolder == null)
			return;

		Task<Void> shareFolderTask = new Task<Void>() {
			@Override
			protected Void call() throws Exception {
				mainWindowModel.getLodds().shareFolder(chosenFolder.getAbsolutePath());
				return null;
			}
		};

		Thread thread = new Thread(shareFolderTask);
		thread.setDaemon(true);
		thread.start();
	}

	public void settingsItemPressed() {
		stage = new Stage();
		SettingsWindowView settingsView = new SettingsWindowView();
		stage.setScene(new Scene(settingsView.getView()));
		stage.initModality(Modality.APPLICATION_MODAL);
		stage.show();
	}

}
