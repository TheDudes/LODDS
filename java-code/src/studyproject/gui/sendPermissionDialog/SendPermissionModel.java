package studyproject.gui.sendPermissionDialog;

import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Timer;
import java.util.TimerTask;

import javafx.beans.property.SimpleStringProperty;
import studyproject.API.Core.Request.GetPermissionRequest;
import studyproject.gui.mainWindow.MainWindowPresenter;

public class SendPermissionModel {

	private Timer timer = null;
	private HashMap<Socket, ArrayList<GetPermissionRequest>> senderMap = new HashMap<Socket, ArrayList<GetPermissionRequest>>();
	private SimpleStringProperty dialogLabel = new SimpleStringProperty();

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

	public void initTimer(MainWindowPresenter presenter, long timerDelay) {
		Timer timer = new Timer(true);
		timer.schedule(new TimerTask() {
			@Override
			public void run() {
				presenter.timerRanOff();
			}
		}, timerDelay);
		setTimer(timer);
	}

	public void setTimer(Timer timer) {
		this.timer = timer;
	}

	public void clearTimer() {
		timer = null;
	}

	public SimpleStringProperty getDialogLabel() {
		return dialogLabel;
	}

	public void setDialogLabel(SimpleStringProperty dialogLabel) {
		this.dialogLabel = dialogLabel;
	}
}
