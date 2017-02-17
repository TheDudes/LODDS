package studyproject.gui.mainWindow;

import java.net.URL;
import java.util.ArrayList;
import java.util.ResourceBundle;
import java.util.logging.Level;

import javax.inject.Inject;

import javafx.application.Platform;
import javafx.collections.ListChangeListener;
import javafx.collections.ListChangeListener.Change;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Modality;
import javafx.stage.Stage;
import studyproject.App;
import studyproject.API.Core.Request.GetPermissionRequest;
import studyproject.API.Errors.ErrLog;
import studyproject.API.Lvl.Low.Broadcast;
import studyproject.gui.mainWindow.filesTree.FilesTreeView;
import studyproject.gui.mainWindow.logArea.LogAreaView;
import studyproject.gui.mainWindow.tasksList.TasksListView;
import studyproject.gui.mainWindow.topMenu.TopMenuView;
import studyproject.gui.mainWindow.usersList.UsersListView;
import studyproject.gui.selectedInterface.SelectedInterfaceView;
import studyproject.gui.sendPermissionDialog.SendPermissionModel;
import studyproject.gui.sendPermissionDialog.SendPermissionView;
import studyproject.logging.APILvl;
import studyproject.logging.LogKey;

public class MainWindowPresenter implements Initializable {

	private static final long TIMER_DELAY = 2000;
	private Stage permissionStage;

	@FXML
	AnchorPane usersListAnchor;
	@FXML
	AnchorPane filesTreeAnchor;
	@FXML
	AnchorPane tasksListAnchor;
	@FXML
	AnchorPane topMenuAnchor;
	@FXML
	AnchorPane logAreaAnchor;
	@FXML
	Label permissionMsg;
	@FXML
	Button acceptButton;
	@Inject
	SendPermissionModel sendPermissionModel;
	@Inject
	MainWindowModel mainWindowModel;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		FilesTreeView filesTreeView = new FilesTreeView();
		filesTreeAnchor.getChildren().addAll(filesTreeView.getView());

		LogAreaView logAreaView = new LogAreaView();
		logAreaAnchor.getChildren().addAll(logAreaView.getView());

		UsersListView usersListView = new UsersListView();
		usersListAnchor.getChildren().addAll(usersListView.getView());

		TasksListView tasksListView = new TasksListView();
		tasksListAnchor.getChildren().addAll(tasksListView.getView());

		TopMenuView topMenuView = new TopMenuView();
		topMenuAnchor.getChildren().addAll(topMenuView.getView());

		setAllAnchorPoints(filesTreeView.getView(), 0.0);
		setAllAnchorPoints(logAreaView.getView(), 0.0);
		setAllAnchorPoints(usersListView.getView(), 0.0);
		setAllAnchorPoints(tasksListView.getView(), 0.0);
		setAllAnchorPoints(topMenuView.getView(), 0.0);

		mainWindowModel.getLodds().getLoddsModel().getPermissionList()
				.addListener(new ListChangeListener<GetPermissionRequest>() {
					@Override
					public void onChanged(Change<? extends GetPermissionRequest> c) {
						permissionListChanged(c);
					}
				});
	}

	private void setAllAnchorPoints(Node child, double value) {
		AnchorPane.setBottomAnchor(child, value);
		AnchorPane.setTopAnchor(child, value);
		AnchorPane.setLeftAnchor(child, value);
		AnchorPane.setRightAnchor(child, value);
	}

	private void permissionListChanged(Change<? extends GetPermissionRequest> c) {
		if (sendPermissionModel.getTimer() == null) {
			sendPermissionModel.initTimer(this, TIMER_DELAY);
		}
		while (c.next()) {
			for (GetPermissionRequest permissionReq : c.getAddedSubList()) {
				sendPermissionModel.addRequestedFile(permissionReq);
			}
		}
	}

	public void timerRanOff() {
		sendPermissionModel.clearTimer();
		Platform.runLater(new Runnable() {
			@Override
			public void run() {
				permissionStage = new Stage();
				SendPermissionView sendPermissionView = new SendPermissionView();
				permissionStage.setScene(new Scene(sendPermissionView.getView()));
				permissionStage.setTitle("Permission Request");
				sendPermissionModel.getDialogLabel().setValue(createPermissionText());
				permissionStage.show();
			}
		});
	}

	private String createPermissionText() {
		int numberOfUsers = sendPermissionModel.getSenderMap().keySet().size();
		int numberOfFiles = sendPermissionModel.getSenderMap().size();
		return numberOfUsers + " Users want to send " + numberOfFiles + " files to you";
	}

	public void loadInterface() {
		ArrayList<String> interfaces = new ArrayList<String>();
		int err;
		if ((err = Broadcast.getNetworkAddresses(interfaces)) != 0) {
			ErrLog.log(Level.SEVERE, LogKey.error, APILvl.gui, err, "loadInterface");
		}
		String interf = (String) App.properties.get("defaultInterface");
		if ((interf == null) || interf.isEmpty() || (!interfaces.contains(interf))) {
			SelectedInterfaceView selectedInterfaceView = new SelectedInterfaceView();
			Stage interfaceStage = new Stage();
			interfaceStage.setMinWidth(300);
			interfaceStage.setMinHeight(200);
			interfaceStage.setTitle("Startup...");
			interfaceStage.setScene(new Scene(selectedInterfaceView.getView()));
			interfaceStage.initModality(Modality.APPLICATION_MODAL);
			interfaceStage.showAndWait();
		} else {
			mainWindowModel.getLodds().startUp(interf, (String) App.properties.get("userName"));
		}
	}

}
