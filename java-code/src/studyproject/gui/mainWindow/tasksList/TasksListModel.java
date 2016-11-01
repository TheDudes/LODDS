package studyproject.gui.mainWindow.tasksList;

import javafx.collections.ObservableList;
import javafx.concurrent.Task;

public class TasksListModel {
	private ObservableList<Task<Void>> tasks;

	public ObservableList<Task<Void>> getTasks() {
		return tasks;
	}

	public void setTasks(ObservableList<Task<Void>> observableList) {
		this.tasks = observableList;
	}
	
	
}
