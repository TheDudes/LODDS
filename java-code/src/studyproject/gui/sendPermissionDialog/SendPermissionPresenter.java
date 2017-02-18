package studyproject.gui.sendPermissionDialog;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.inject.Inject;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import studyproject.API.Core.Request.GetPermissionRequest;
import studyproject.API.Errors.ErrorFactory;
import studyproject.gui.mainWindow.MainWindowModel;
import studyproject.logging.LogKey;
import studyproject.gui.Core.Utils;

public class SendPermissionPresenter implements Initializable {

	@FXML
	Label permissionMsg;
	@FXML
	Button acceptButton;
	@FXML
	Button declineButton;
	@FXML
	ProgressBar permissionProgress;
	@Inject
	MainWindowModel mainWindowModel;
	@Inject
	SendPermissionModel sendPermissionModel;

	private Logger logger;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		logger = Logger.getGlobal();
		acceptButton.setOnAction(e -> acceptPressed());
		declineButton.setOnAction(e -> declinePressed());
	}

	public void permissionStageOnShown() {
		SendPermissionDialog sendPermissionDialog = (SendPermissionDialog) acceptButton.getScene().getWindow();
		permissionMsg.textProperty().bindBidirectional(sendPermissionDialog.getLabelText());
		permissionProgress.progressProperty().bindBidirectional(sendPermissionDialog.getProgressDouble());
		long timeout = sendPermissionDialog.getPermissionRequest().timeout;
		Timer timer = sendPermissionDialog.getTimer();
		timer.scheduleAtFixedRate(new TimerTask() {
			@Override
			public void run() {
				Platform.runLater(() -> {
					sendPermissionDialog.setProgressDouble(permissionProgress.getProgress() - (1.0 / timeout));
					if (permissionProgress.getProgress() <= 0.0) {
						permissionProgress.getScene().getWindow().hide();
						timer.cancel();
					}
				});
			}
		}, 0, 1000);

	}

	private void acceptPressed() {
		disableButtons();
		GetPermissionRequest getPermissionRequest = ((SendPermissionDialog) acceptButton.getScene().getWindow())
				.getPermissionRequest();
		String path = Utils.getChoosenDirPath("Choose folder to save files in");
		mainWindowModel.getLodds().getFileWP(getPermissionRequest.socket, path, getPermissionRequest.fileName,
				getPermissionRequest.fileSize);
		acceptButton.getScene().getWindow().hide();
		logger.log(ErrorFactory.build(Level.INFO, LogKey.error, "File " + getPermissionRequest.fileName + " from "
				+ getPermissionRequest.socket.getInetAddress() + " accepted"));
	}

	private void declinePressed() {
		disableButtons();
		GetPermissionRequest getPermissionRequest = ((SendPermissionDialog) acceptButton.getScene().getWindow())
				.getPermissionRequest();
		try {
			((SendPermissionDialog) acceptButton.getScene().getWindow()).getPermissionRequest().socket.close();
		} catch (IOException e) {
			logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, e));
		}
		declineButton.getScene().getWindow().hide();
		logger.log(ErrorFactory.build(Level.INFO, LogKey.error, "File " + getPermissionRequest.fileName + " from "
				+ getPermissionRequest.socket.getInetAddress() + " declined"));
	}

	private void disableButtons() {
		acceptButton.setDisable(true);
		declineButton.setDisable(true);
	}
}
