package studyproject.gui.mainWindow.tasksList;

import java.net.URL;
import java.util.ResourceBundle;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadLocalRandom;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.inject.Inject;

import org.controlsfx.control.TaskProgressView;

import javafx.concurrent.Task;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.image.ImageView;
import javafx.scene.layout.AnchorPane;
import studyproject.API.Errors.ErrorFactory;
import studyproject.logging.LogKey;

public class TasksListPresenter implements Initializable {

	@FXML
	AnchorPane tasksListAnchor;

	@FXML
	Button addButton;

	@Inject
	TasksListModel taskModel;

	private TaskProgressView<Task<Void>> tasksProgressView;
	private ExecutorService executorService;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		executorService = Executors.newCachedThreadPool();
		tasksProgressView = new TaskProgressView<>();
		tasksListAnchor.getChildren().add(tasksProgressView);
		setAllAnchorPoints(tasksProgressView, 0.0);

		tasksProgressView.setGraphicFactory(task -> {
			return new ImageView("file:ico.png");
		});
		taskModel.setTasks(tasksProgressView.getTasks());

		addButton.setOnAction(e -> setAddButtonAction());

	}

	private void setAddButtonAction() {
		Task<Void> task = task();
		executorService.submit(task);
		tasksProgressView.getTasks().add(task);

		Logger.getGlobal().log(ErrorFactory.build(Level.SEVERE, LogKey.filetransferInit, 0));
	}

	private void setAllAnchorPoints(Node child, double value) {
		AnchorPane.setBottomAnchor(child, value);
		AnchorPane.setTopAnchor(child, value);
		AnchorPane.setLeftAnchor(child, value);
		AnchorPane.setRightAnchor(child, value);
	}

	public Task<Void> task() {
		return new Task<Void>() {
			@Override
			protected Void call() throws InterruptedException {
				try {
					Thread.sleep(ThreadLocalRandom.current().nextInt(200, 2000));
				} catch (InterruptedException e) {
					// e.printStackTrace();
				}
				updateTitle("This is a title");
				updateMessage("Finding friends . . .");
				try {
					Thread.sleep(ThreadLocalRandom.current().nextInt(200, 2000));
				} catch (InterruptedException e) {
					// e.printStackTrace();
				}
				updateProgress(0, 100);
				for (int i = 0; i < 100; i++) {
					if (isCancelled()) {
						updateMessage("Cancelled");
						try {
							Thread.sleep(ThreadLocalRandom.current().nextInt(200, 2000));
						} catch (InterruptedException e) {
							// e.printStackTrace();
						}
						break;
					}

					updateProgress(i + 1, 100);

					updateMessage("Found " + (i + 1) + " friends!");

					try {
						Thread.sleep(300);
					} catch (InterruptedException interrupted) {
						if (isCancelled()) {
							updateMessage("Cancelled");
							break;
						}
					}
				}

				updateMessage("Found all.");
				try {
					Thread.sleep(ThreadLocalRandom.current().nextInt(200, 2000));
				} catch (InterruptedException e) {
					// e.printStackTrace();
				}

				return null;
			}
		};
	}

}
