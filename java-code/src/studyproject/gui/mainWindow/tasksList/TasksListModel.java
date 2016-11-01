package studyproject.gui.mainWindow.tasksList;

import javafx.collections.ObservableList;

public class TasksListModel {
	private ObservableList<Test> tasks;

	public ObservableList<Test> getTasks() {
		return tasks;
	}

	public void setTasks(ObservableList<Test> tasks) {
		this.tasks = tasks;
	}
	
	
}
