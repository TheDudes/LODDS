package studyproject.API.Lvl.Mid.Lodds;

import javax.annotation.PostConstruct;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import studyproject.API.Core.Request.GetPermissionRequest;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.API.Lvl.Mid.ThreadMonitoring.MonitoredThread;

public class LoddsModel {
	private ObservableList<UserInfo> clientList;
	private ObservableList<MonitoredThread> tasksList;
	private ObservableList<GetPermissionRequest> permissionList;

	@PostConstruct
	public void init() {
		clientList = javafx.collections.FXCollections.synchronizedObservableList(FXCollections.observableArrayList());
		tasksList = javafx.collections.FXCollections.synchronizedObservableList(FXCollections.observableArrayList());
		permissionList = javafx.collections.FXCollections
				.synchronizedObservableList(FXCollections.observableArrayList());
	}

	public synchronized ObservableList<MonitoredThread> getTasksList() {
		return tasksList;
	}

	public synchronized ObservableList<UserInfo> getClientList() {
		return clientList;
	}

	public synchronized ObservableList<GetPermissionRequest> getPermissionList() {
		return permissionList;
	}
}
