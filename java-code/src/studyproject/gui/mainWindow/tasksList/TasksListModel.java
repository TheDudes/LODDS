package studyproject.gui.mainWindow.tasksList;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Parent;
import studyproject.API.Lvl.Mid.ThreadMonitoring.MonitoredThread;
import studyproject.gui.mainWindow.tasksList.singleTask.SingleTaskView;

import javax.annotation.PostConstruct;

public class TasksListModel {
	private ObservableList<MonitoredThread> tasks;

	@PostConstruct
	private void init(){
		tasks = FXCollections.observableArrayList();
		}

	public ObservableList<MonitoredThread> getTasks() {
		return tasks;
	}

	public void setTasks(ObservableList<MonitoredThread> tasks) {
		this.tasks = tasks;
	}


}

	

