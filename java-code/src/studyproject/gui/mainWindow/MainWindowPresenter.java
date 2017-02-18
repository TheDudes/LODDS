package studyproject.gui.mainWindow;

import java.net.URL;
import java.util.ArrayList;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

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
import studyproject.API.Errors.ErrorFactory;
import studyproject.API.Lvl.Low.Broadcast;
import studyproject.gui.mainWindow.filesTree.FilesTreeView;
import studyproject.gui.mainWindow.logArea.LogAreaView;
import studyproject.gui.mainWindow.tasksList.TasksListView;
import studyproject.gui.mainWindow.topMenu.TopMenuView;
import studyproject.gui.mainWindow.usersList.UsersListView;
import studyproject.gui.selectedInterface.SelectedInterfaceView;
import studyproject.gui.sendPermissionDialog.SendPermissionDialog;
import studyproject.gui.sendPermissionDialog.SendPermissionModel;
import studyproject.gui.sendPermissionDialog.SendPermissionView;
import studyproject.logging.LogKey;

public class MainWindowPresenter implements Initializable {

	private SendPermissionDialog permissionStage;

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

	private Logger logger;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		logger = Logger.getGlobal();
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
						permissionListChanged2(c);
					}
				});
	}

	private void setAllAnchorPoints(Node child, double value) {
		AnchorPane.setBottomAnchor(child, value);
		AnchorPane.setTopAnchor(child, value);
		AnchorPane.setLeftAnchor(child, value);
		AnchorPane.setRightAnchor(child, value);
	}

	private void permissionListChanged2(Change<? extends GetPermissionRequest> c) {
		Platform.runLater(new Runnable() {
			@Override
			public void run() {
				while (c.next()) {
					for (GetPermissionRequest permissionReq : c.getAddedSubList()) {
						permissionStage = new SendPermissionDialog(permissionReq);
						SendPermissionView sendPermissionView = new SendPermissionView();
						permissionStage.setScene(new Scene(sendPermissionView.getView()));
						permissionStage.setTitle("Permission Request");
						String newText = permissionReq.socket.getInetAddress().toString() + " wants to send a File: "
								+ permissionReq.fileName + " (" + permissionReq.fileSize + " Bytes)";
						sendPermissionModel.getDialogLabel().setValue(newText);
						permissionStage.show();
						mainWindowModel.getLodds().getLoddsModel().getPermissionList().remove(permissionReq);
					}
				}
			}
		});
	}

	public void loadInterface() {
		ArrayList<String> interfaces = new ArrayList<String>();
		int err;
		if ((err = Broadcast.getNetworkAddresses(interfaces)) != 0) {
			logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, err));
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
