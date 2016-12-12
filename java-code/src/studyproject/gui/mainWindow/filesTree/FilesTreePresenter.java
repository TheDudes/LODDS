package studyproject.gui.mainWindow.filesTree;

import java.io.File;
import java.net.URL;
import java.util.ResourceBundle;
import java.util.logging.Level;

import javax.inject.Inject;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.stage.DirectoryChooser;
import javafx.stage.Stage;
import studyproject.App;
import studyproject.API.Errors.ErrLog;
import studyproject.API.Lvl.Mid.Core.FileCoreInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.gui.mainWindow.MainWindowModel;
import studyproject.gui.mainWindow.usersList.UsersListModel;
import studyproject.logging.APILvl;
import studyproject.logging.LogKey;

public class FilesTreePresenter implements Initializable {

	@FXML
	TreeView<FileCoreInfo> filesTreeView;
	@FXML
	TextField filesTreeSearch;
	@Inject
	UsersListModel userListModel;
	@Inject
	MainWindowModel mainWindowModel;
	@FXML
	Button downloadButton;

	private final TreeItem<FileCoreInfo> root = new TreeItem<>();

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		filesTreeView.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		filesTreeView.setShowRoot(false);
		filesTreeView.setRoot(root);
		downloadButton.setOnAction(e -> downloadPressed());
		userListModel.getSelectedUser().addListener(new ChangeListener<UserInfo>() {
			@Override
			public void changed(ObservableValue<? extends UserInfo> observable, UserInfo oldValue, UserInfo newValue) {
				createTree(newValue);
			}
		});
		filesTreeView.getSelectionModel().selectedItemProperty()
				.addListener(new ChangeListener<TreeItem<FileCoreInfo>>() {

					@Override
					public void changed(ObservableValue<? extends TreeItem<FileCoreInfo>> observable,
							TreeItem<FileCoreInfo> oldValue, TreeItem<FileCoreInfo> newValue) {
						System.out.println(newValue.getValue().getFileName() + newValue.getValue().getChecksum());
					}
				});
	}

	public void createTree(UserInfo userInfo) {
		root.getChildren().clear();
		String[] subpaths = null;
		FileCoreInfo infoToAdd = null;
		int startIndex = 0;
		for (String path : userInfo.getPathToFileInfo().keySet()) {
			infoToAdd = userInfo.getPathToFileInfo().get(path);
			subpaths = path.split("/");
			if (subpaths[0].isEmpty())
				startIndex = 1;
			addTreeItem(infoToAdd, subpaths, startIndex, root);
		}
	}

	private void addTreeItem(FileCoreInfo infoToAdd, String[] subPaths, int index, TreeItem<FileCoreInfo> parent) {
		TreeItem<FileCoreInfo> folderToAdd = null;

		for (TreeItem<FileCoreInfo> item : parent.getChildren()) {
			if (subPaths[index].equals(item.getValue().getFileName())) {
				addTreeItem(infoToAdd, subPaths, ++index, item);
				return;
				// size = addTreeItem(infoToAdd, subPaths, ++index, item);
				// item.getValue().increment size

			}
		}
		if (index == subPaths.length - 1) {
			// ADD THE FILE
			parent.getChildren().add(new TreeItem<FileCoreInfo>(infoToAdd));
			// return child.size();
		} else {
			// ADD new folder and recur with it
			folderToAdd = new TreeItem<FileCoreInfo>(new FileCoreInfo(subPaths[index]));
			parent.getChildren().add(folderToAdd);
			addTreeItem(infoToAdd, subPaths, ++index, folderToAdd);
		}

	}

	private void downloadPressed() {
		ObservableList<TreeItem<FileCoreInfo>> itemsList = filesTreeView.getSelectionModel().getSelectedItems();
		Stage stage = new Stage();
		DirectoryChooser directoryChooser = new DirectoryChooser();
		String absolutePath;
		String defaultSavePath = (String) App.properties.get("defaultSavePath");
		File chosenFolder;
		File savePathFile = new File(defaultSavePath);
		
		if (savePathFile.exists()) {
			directoryChooser.setInitialDirectory(savePathFile);
		}
		chosenFolder = directoryChooser.showDialog(stage);
		stage.hide();

		if (chosenFolder == null) {
			ErrLog.log(Level.INFO, LogKey.info, APILvl.mid, "downloadPressed",
					"Non folder choosen. Download aborted.");
			return;
		}
		System.out.println("before replace: " + chosenFolder.getAbsolutePath());
		absolutePath = chosenFolder.getAbsolutePath().replace("\\", "/");
		System.out.println("absolutePath: " + absolutePath);
		for (TreeItem<FileCoreInfo> treeItem : itemsList) {
			FileCoreInfo fileCoreInfo = treeItem.getValue();
			if (fileCoreInfo.getChecksum() == null) {
				continue;
			}
			mainWindowModel.getLodds().getFile(userListModel.getSelectedUser().get(), fileCoreInfo.getChecksum(),
					absolutePath);
		}
		return;
	}

}
