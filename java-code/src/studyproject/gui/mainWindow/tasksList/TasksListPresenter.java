package studyproject.gui.mainWindow.tasksList;

import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;

import javafx.application.Platform;
import javafx.collections.ListChangeListener;
import javafx.scene.control.ListView;
import javafx.scene.image.Image;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import studyproject.App;
import studyproject.API.Lvl.Mid.ThreadMonitoring.MonitoredThread;
import studyproject.gui.mainWindow.MainWindowModel;
import studyproject.gui.mainWindow.tasksList.singleTask.SingleTaskListCell;

public class TasksListPresenter implements Initializable {

	@FXML
	ListView<MonitoredThread> monitoredThreadListView;
	@Inject
	TasksListModel tasksListModel;
	@Inject
	MainWindowModel mainWindowModel;

	private final Image cancelButtonImage = new Image(getClass().getResourceAsStream(App.ICON_PATH + "x.png"));

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		bindLoddsModelTaskToTasksList();
		monitoredThreadListView.setItems(tasksListModel.getTasks());
		monitoredThreadListView.setCellFactory(monitoredThreadListView1 -> new SingleTaskListCell(cancelButtonImage));
	}

	private void bindLoddsModelTaskToTasksList() {
		mainWindowModel.getLodds().getLoddsModel().getTasksList()
				.addListener(new ListChangeListener<MonitoredThread>() {
					@Override
					public void onChanged(Change<? extends MonitoredThread> change) {
						while (change.next()) {
							for (MonitoredThread thread : change.getRemoved()) {
								if (thread.isOneOfMultiple())
									return;
								if (!tasksListModel.getTasks().contains(thread))
									continue;
								Platform.runLater(() -> {
									tasksListModel.getTasks().remove(thread);
									monitoredThreadListView.refresh();
								});
							}
							for (MonitoredThread thread : change.getAddedSubList()) {
								if (thread.isOneOfMultiple()) {
									return;
								}
								Platform.runLater(() -> {
									tasksListModel.getTasks().add(thread);
									monitoredThreadListView.refresh();
								});
							}

						}
					}
				});
	}

}
