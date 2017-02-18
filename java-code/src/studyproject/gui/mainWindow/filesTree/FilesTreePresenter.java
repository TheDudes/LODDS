package studyproject.gui.mainWindow.filesTree;

import java.io.File;
import java.net.URL;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.inject.Inject;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import studyproject.API.Errors.ErrorFactory;
import studyproject.API.Lvl.Mid.Core.FileCoreInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.gui.Core.Utils;
import studyproject.gui.mainWindow.MainWindowModel;
import studyproject.gui.mainWindow.tasksList.TasksListModel;
import studyproject.gui.mainWindow.usersList.UsersListModel;
import studyproject.logging.LogKey;

public class FilesTreePresenter implements Initializable {

	@FXML
	TreeView<FileCoreInfo> filesTreeView;
	@FXML
	TextField filesTreeSearch;
	@FXML
	MenuItem reloadTreeCM;
	@Inject
	UsersListModel userListModel;
	@Inject
	MainWindowModel mainWindowModel;
	@Inject
	TasksListModel tasksListModel;
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
		reloadTreeCM.setOnAction(e -> createTree(userListModel.getSelectedUser().get()));
	}

	public void createTree(UserInfo userInfo) {
		if (userInfo == null)
			return;
		root.getChildren().clear();
		String[] subPaths = null;
		FileCoreInfo infoToAdd = null;
		int startIndex = 0;
		for (String path : userInfo.getPathToFileInfo().keySet()) {
			infoToAdd = userInfo.getPathToFileInfo().get(path);
			subPaths = path.split("/");
			if (subPaths[0].isEmpty())
				startIndex = 1;
			addTreeItem(infoToAdd, subPaths, startIndex, root);
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
		String absolutePath;
		File chosenFolder;
		try {
			chosenFolder = new File(Utils.getChoosenDirPath("Choose folder to save files in"));
		} catch (NullPointerException e) {
			Logger.getGlobal().log(ErrorFactory.build(Level.INFO, LogKey.info, "No folder chosen. Download aborted."));
			return;
		}
		absolutePath = chosenFolder.getAbsolutePath().replace("\\", "/");
		if (absolutePath.endsWith("/")) {
			absolutePath.substring(0, absolutePath.length() - 1);
		}
		for (TreeItem<FileCoreInfo> treeItem : itemsList) {
			FileCoreInfo fileCoreInfo = treeItem.getValue();
			if (fileCoreInfo.getChecksum() == null) {
				// continue because this item is a folder. it has no checksum.
				continue;
			}
			mainWindowModel.getLodds().getFile(userListModel.getSelectedUser().get(), fileCoreInfo.getChecksum(),
					absolutePath + fileCoreInfo.getFilePath());
		}
		return;
	}

}
