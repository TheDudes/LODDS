package studyproject.gui.sendPermissionDialog;

import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import studyproject.API.Core.Request.GetPermissionRequest;
import studyproject.gui.mainWindow.MainWindowModel;
import studyproject.gui.Core.Utils;

public class SendPermissionPresenter implements Initializable {

	@FXML
	Label permissionMsg;
	@FXML
	Button acceptButton;
	@FXML
	Button declineButton;
	@Inject
	MainWindowModel mainWindowModel;
	@Inject
	SendPermissionModel sendPermissionModel;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		acceptButton.setOnAction(e -> acceptPressed());
		declineButton.setOnAction(e -> declinePressed());
		permissionMsg.textProperty().bindBidirectional(sendPermissionModel.getDialogLabel());
	}

	private void acceptPressed() {
		SendPermissionDialog sendPermissionDialog = (SendPermissionDialog) acceptButton.getScene().getWindow();
		GetPermissionRequest getPermissionRequest = sendPermissionDialog.getPermissionRequest();
		String path = Utils.getChoosenDirPath("Choose folder to save files in");
		mainWindowModel.getLodds().getFileWP(getPermissionRequest.socket, path, getPermissionRequest.fileName,
				getPermissionRequest.fileSize);
		acceptButton.getScene().getWindow().hide();
	}

	private void declinePressed() {
		disableButtons();
		declineButton.getScene().getWindow().hide();
	}

	private void disableButtons() {
		acceptButton.setDisable(true);
		declineButton.setDisable(true);
	}
}
