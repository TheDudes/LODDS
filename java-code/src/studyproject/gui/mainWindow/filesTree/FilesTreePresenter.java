package studyproject.gui.mainWindow.filesTree;

import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;

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
	}

	public void createTree(UserInfo userInfo) {
		root.getChildren().clear();
		String[] subpaths = null;
		String fileName = "default";
		FileCoreInfo infoToAdd = null;
		for (String path : userInfo.getPathToFileInfo().keySet()) {
			infoToAdd = userInfo.getPathToFileInfo().get(path);
			subpaths = path.split("/");
			fileName = subpaths[subpaths.length - 1];
			System.out.println(fileName + infoToAdd.toString());

		}
	}

}
