package studyproject.gui.sendPermissionDialog;

import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Timer;

import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleStringProperty;
import studyproject.API.Core.Request.GetPermissionRequest;

public class SendPermissionModel {

	private Timer timer = null;
	private HashMap<Socket, ArrayList<GetPermissionRequest>> senderMap = new HashMap<Socket, ArrayList<GetPermissionRequest>>();
	private SimpleStringProperty dialogLabel = new SimpleStringProperty();
	private SimpleDoubleProperty progressValue = new SimpleDoubleProperty();

	public Timer getTimer() {
		return timer;
	}

	public HashMap<Socket, ArrayList<GetPermissionRequest>> getSenderMap() {
		return senderMap;
	}

	public void addRequestedFile(GetPermissionRequest permissionReq) {
		ArrayList<GetPermissionRequest> requestList = senderMap.get(permissionReq.socket);
		if (requestList != null) {
			requestList.add(permissionReq);
		} else {
			requestList = new ArrayList<GetPermissionRequest>();
			requestList.add(permissionReq);
		}
		senderMap.put(permissionReq.socket, requestList);
	}

	public ArrayList<GetPermissionRequest> getFileListFromSender(Socket socket) {
		return senderMap.get(socket);
	}

	public SimpleStringProperty getDialogLabel() {
		return dialogLabel;
	}

	public void setDialogLabel(SimpleStringProperty dialogLabel) {
		this.dialogLabel = dialogLabel;
	}
	
	public SimpleDoubleProperty getProgressValue() {
		return progressValue;
	}
	
	public void setProgressValue(SimpleDoubleProperty progressValue) {
		this.progressValue = progressValue;
	}
}
