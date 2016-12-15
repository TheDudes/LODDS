package studyproject.API.Lvl.Mid.Lodds;

import javax.annotation.PostConstruct;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.API.Lvl.Mid.ThreadMonitoring.MonitoredThread;

public class LoddsModel {
	private ObservableList<UserInfo> clientList;

	private ObservableList<MonitoredThread> tasksList;

	@PostConstruct
	public void init() {
		clientList = javafx.collections.FXCollections.synchronizedObservableList(FXCollections.observableArrayList());
		tasksList = javafx.collections.FXCollections.synchronizedObservableList(FXCollections.observableArrayList());
	}

	public synchronized ObservableList<MonitoredThread> getTasksList() {
		return tasksList;
	}

	public synchronized void setTasksList(ObservableList<MonitoredThread> tasksList) {
		this.tasksList = tasksList;
	}

	public synchronized ObservableList<UserInfo> getClientList() {
		return clientList;
	}

	public synchronized void setClientList(ObservableList<UserInfo> clientList) {
		this.clientList = clientList;
	}

}
