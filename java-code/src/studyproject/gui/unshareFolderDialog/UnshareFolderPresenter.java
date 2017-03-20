package studyproject.gui.unshareFolderDialog;

import java.net.URL;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.inject.Inject;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import javafx.scene.control.SelectionMode;
import studyproject.API.Errors.ErrorFactory;
import studyproject.gui.mainWindow.MainWindowModel;
import studyproject.logging.LogKey;

public class UnshareFolderPresenter implements Initializable {

	@FXML
	Button unshareButton;
	@FXML
	Button okButton;
	@FXML
	ListView<String> sharedFolderList;
	@Inject
	UnshareFolderModel unshareFolderModel;
	@Inject
	MainWindowModel mainWindowModel;

	@Override
	public void initialize(URL arg0, ResourceBundle arg1) {
		unshareButton.setOnAction(e -> unshareButtonPressed());
		okButton.setOnAction(e -> ((Button) e.getSource()).getScene().getWindow().hide());

		sharedFolderList.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		fillList();
	}
	
	private void fillList() {
		unshareFolderModel.getSharedFolderList().clear();
		for (String folderPath : mainWindowModel.getLodds().getWatchService().getWatchedDirectories()) {
			unshareFolderModel.getSharedFolderList().add(folderPath);
		}
		sharedFolderList.setItems(unshareFolderModel.getSharedFolderList());
	}

	private void unshareButtonPressed() {
		for (String folderPath : sharedFolderList.selectionModelProperty().get().getSelectedItems()) {
			System.out.println("unshare following path: " + folderPath);
			mainWindowModel.getLodds().getWatchService().unwatchDirectory(folderPath);
			Logger.getGlobal().log(ErrorFactory.build(Level.INFO, LogKey.sharedFiles, "Unshared Folder " + folderPath));
		}
		fillList();
	}

}
