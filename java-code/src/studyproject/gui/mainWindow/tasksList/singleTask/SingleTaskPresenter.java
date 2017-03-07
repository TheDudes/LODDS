package studyproject.gui.mainWindow.tasksList.singleTask;

import javafx.application.Platform;
import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import studyproject.API.Lvl.Mid.Lodds.LoddsModel;
import studyproject.API.Lvl.Mid.ThreadMonitoring.MonitoredThread;
import studyproject.gui.mainWindow.MainWindowModel;

import javax.inject.Inject;
import java.net.URL;
import java.util.ResourceBundle;

public class SingleTaskPresenter implements Initializable {

	@FXML
	private Label folderFileNameLabel;

	@FXML
	private ProgressBar progressBar;

	@FXML
	private Label currentFileLabel;

	@FXML
	private Label progressLabel;

	@FXML
	private Button cancel;

	private MonitoredThread monitoredThread;

	@Inject
	MainWindowModel mainWindowModel;

	@Override
	public void initialize(URL url, ResourceBundle resourceBundle) {
	}

	public void setMonitoredThread(MonitoredThread monitoredThread) {
		this.monitoredThread = monitoredThread;
		cancel.setOnAction((e) -> this.monitoredThread.setRunning(false));
		progressBar.progressProperty().bindBidirectional(this.monitoredThread.getProgress());
		currentFileLabel.textProperty().bindBidirectional(this.monitoredThread.getCurrentFileName());
		folderFileNameLabel.setText(this.monitoredThread.getNameToDisplay());
		this.monitoredThread.getDoneSize().addListener((observable, oldValue, newValue) -> {
			Platform.runLater(() -> progressLabel.textProperty()
					.setValue((newValue.longValue() >> 20) + "/" + (monitoredThread.getWholeSize() >> 20) + "MB"));
		});
		this.monitoredThread.isRunning().addListener((obs, oldValue, newValue) -> {
			if (newValue == false) {
				Platform.runLater(() -> progressLabel.textProperty().setValue("Stopped..."));
			} else if (newValue == true) {
				Platform.runLater(() -> progressLabel.textProperty().setValue("Running..."));
			}

		});
		this.monitoredThread.isFinished().addListener((obs, oldValue, newValue) -> {
			if (newValue == true)
				mainWindowModel.getLodds().getLoddsModel().getTasksList().remove(this.monitoredThread);
		});

	}

}
