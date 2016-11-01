package studyproject.gui.mainWindow;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.layout.AnchorPane;
import studyproject.gui.mainWindow.filesTree.FilesTreeView;
import studyproject.gui.mainWindow.logArea.LogAreaView;
import studyproject.gui.mainWindow.tasksList.TasksListView;
import studyproject.gui.mainWindow.topMenu.TopMenuView;
import studyproject.gui.mainWindow.usersList.UsersListView;

public class MainWindowPresenter implements Initializable {

	@FXML
	AnchorPane usersListAnchor;
	@FXML
	AnchorPane filesTreeAnchor;
	@FXML
	AnchorPane tasksListAnchor;
	@FXML
	AnchorPane topMenuAnchor;
	@FXML
	AnchorPane logAreaAnchor;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		FilesTreeView filesTreeView = new FilesTreeView();
		filesTreeAnchor.getChildren().addAll(filesTreeView.getView());
		
		LogAreaView logAreaView = new LogAreaView();
		logAreaAnchor.getChildren().addAll(logAreaView.getView());
		
		UsersListView usersListView = new UsersListView();
		usersListAnchor.getChildren().addAll(usersListView.getView());
		
		TasksListView tasksListView = new TasksListView();
		tasksListAnchor.getChildren().addAll(tasksListView.getView());
		
		TopMenuView topMenuView = new TopMenuView();
		topMenuAnchor.getChildren().addAll(topMenuView.getView());
		
		setAllAnchorPoints(filesTreeView.getView(), 0.0);
		setAllAnchorPoints(logAreaView.getView(), 0.0);
		setAllAnchorPoints(usersListView.getView(), 0.0);
		setAllAnchorPoints(tasksListView.getView(), 0.0);
		setAllAnchorPoints(topMenuView.getView(), 0.0);

	}
	
	private void setAllAnchorPoints(Node child, double value) {
		AnchorPane.setBottomAnchor(child, value);
		AnchorPane.setTopAnchor(child, value);
		AnchorPane.setLeftAnchor(child, value);
		AnchorPane.setRightAnchor(child, value);
	}

}
