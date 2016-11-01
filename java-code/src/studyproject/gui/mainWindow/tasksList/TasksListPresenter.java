package studyproject.gui.mainWindow.tasksList;

import java.io.File;
import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;

import org.controlsfx.control.TaskProgressView;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.image.ImageView;
import javafx.scene.layout.AnchorPane;

public class TasksListPresenter implements Initializable {

	@FXML
	AnchorPane tasksListAnchor;
	
	@FXML Button addButton;

	@Inject TasksListModel taskModel;
	
	
	private TaskProgressView<Test> tasksProgressView;
	
	@Override
	public void initialize(URL location, ResourceBundle resources) {
		tasksProgressView = new TaskProgressView<>();
		tasksListAnchor.getChildren().add(tasksProgressView);
		setAllAnchorPoints(tasksProgressView, 0.0);
		
		tasksProgressView.setGraphicFactory(task -> {
			return new ImageView(new File("D:/Documents/EclipseWorkspace/studyproject/ico.png").toURI().toString());
		});
		taskModel.setTasks(tasksProgressView.getTasks());
		
		addButton.setOnAction(e -> setAddButtonAction());

	}

	private void setAddButtonAction(){
		tasksProgressView.getTasks().add(new Test());
	}
	
	
	private void setAllAnchorPoints(Node child, double value) {
		AnchorPane.setBottomAnchor(child, value);
		AnchorPane.setTopAnchor(child, value);
		AnchorPane.setLeftAnchor(child, value);
		AnchorPane.setRightAnchor(child, value);
	}
}
