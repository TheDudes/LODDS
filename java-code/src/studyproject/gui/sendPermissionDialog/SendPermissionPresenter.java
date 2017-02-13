package studyproject.gui.sendPermissionDialog;

import java.net.Socket;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
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

	private void declinePressed() {
		disableButtons();
		declineButton.getScene().getWindow().hide();
	}

	private void acceptPressed() {
		disableButtons();
		Iterator<Socket> keyIterator = sendPermissionModel.getSenderMap().keySet().iterator();
		String path = Utils.getChoosenDirPath("Choose folder to save files in");
		while (keyIterator.hasNext()) {
			ArrayList<GetPermissionRequest> permissionRequestList = sendPermissionModel
					.getFileListFromSender(keyIterator.next());
			for (GetPermissionRequest permissionReq : permissionRequestList) {
				mainWindowModel.getLodds().getFileWP(permissionReq.socket, path, permissionReq.fileName,
						permissionReq.fileSize);
			}
		}
		acceptButton.getScene().getWindow().hide();
	}

	private void disableButtons() {
		acceptButton.setDisable(true);
		declineButton.setDisable(true);
	}
}
