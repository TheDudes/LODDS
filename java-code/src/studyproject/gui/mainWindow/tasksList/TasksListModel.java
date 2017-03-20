package studyproject.gui.mainWindow.tasksList;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import studyproject.API.Lvl.Mid.ThreadMonitoring.MonitoredThread;
import studyproject.gui.introduction.IntroductionInterface;

import javax.annotation.PostConstruct;

public class TasksListModel implements IntroductionInterface {
	private ObservableList<MonitoredThread> tasks;

	@PostConstruct
	private void init() {
		tasks = FXCollections.observableArrayList();
	}

	public ObservableList<MonitoredThread> getTasks() {
		return tasks;
	}

	public void setTasks(ObservableList<MonitoredThread> tasks) {
		this.tasks = tasks;
	}

	@Override
	public String getViewDiscription() {
		return "In the TaskList you can see all on-going downloads. "
				+ "If you need, you can interrupt these downloads by clicking "
				+ "at the red 'X'.";
	}

	@Override
	public String getViewTitle() {
		return "Task List";
	}

}
