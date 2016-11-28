package studyproject.API.Lvl.Mid.Lodds;

import javax.annotation.PostConstruct;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class LoddsModel {
	private ObservableList<UserInfo> clientList;

	@PostConstruct
	public void init() {
		clientList = javafx.collections.FXCollections.synchronizedObservableList(FXCollections.observableArrayList());
	}

	public ObservableList<UserInfo> getClientList() {
		return clientList;
	}

	public void setClientList(ObservableList<UserInfo> clientList) {
		this.clientList = clientList;
	}

}
