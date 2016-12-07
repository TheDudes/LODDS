package studyproject.gui.mainWindow.filesTree;

import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import studyproject.API.Lvl.Mid.Core.FileCoreInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.gui.mainWindow.usersList.UsersListModel;

public class FilesTreePresenter implements Initializable {

	@FXML
	TreeView<FileCoreInfo> filesTreeView;
	@FXML
	TextField filesTreeSearch;
	@Inject
	UsersListModel userListModel;

	private final TreeItem<FileCoreInfo> root = new TreeItem<>();

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		// TODO Auto-generated method stub
		filesTreeView.setShowRoot(false);
		// final TreeItem<FileCoreInfo> test = new TreeItem<FileCoreInfo>(new
		// FileCoreInfo("aslkdgoiaweqo", 20000));
		// root.getChildren().add(test);
		filesTreeView.setRoot(root);
		userListModel.getSelectedUser().addListener(new ChangeListener<UserInfo>() {
			@Override
			public void changed(ObservableValue<? extends UserInfo> observable, UserInfo oldValue, UserInfo newValue) {
//				createTree(newValue);
				System.out.println(newValue);
			}
		});
	}

	public void createTree(UserInfo userInfo) {
		System.out.println("create tree");
		root.getChildren().clear();
		String[] subpaths = null;
		FileCoreInfo infoToAdd = null;
		System.out.println("before getUserinfoslist");
		for (String path : userInfo.getPathToFileInfo().keySet()) {
			infoToAdd = userInfo.getPathToFileInfo().get(path);
			subpaths = path.split("/");
			addTreeItem(infoToAdd, subpaths, 0, root);
		}
		System.out.println(userInfo.getPathToFileInfo());

	}

	private void addTreeItem(FileCoreInfo infoToAdd, String[] subPaths, int index, TreeItem<FileCoreInfo> parent) {
		boolean found = false;
		TreeItem<FileCoreInfo> folderToAdd = null;

		found = false;
		for (TreeItem<FileCoreInfo> item : parent.getChildren()) {
			if (subPaths.equals(item.getValue().getFileName())) {
				found = true;
				folderToAdd = item;
			}
		}
		if (!found) {
			folderToAdd = new TreeItem<FileCoreInfo>(new FileCoreInfo(subPaths[index]));
			parent.getChildren().add(folderToAdd);
			if (!subPaths.equals(infoToAdd.getFileName())) {
				addTreeItem(infoToAdd, subPaths, index++, folderToAdd);
			}
		} else {
			if (subPaths.equals(infoToAdd.getFileName())) {
				parent.getChildren().add(new TreeItem<FileCoreInfo>(infoToAdd));
			}
			addTreeItem(infoToAdd, subPaths, index++, folderToAdd);
		}

	}

}
