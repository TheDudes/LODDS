package studyproject.gui.mainWindow.tasksList.singleTask;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import studyproject.API.Lvl.Mid.ThreadMonitoring.MonitoredThread;

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

	private MonitoredThread monitoredThread;

	@Override
	public void initialize(URL url, ResourceBundle resourceBundle) {
	}

	public void setMonitoredThread(MonitoredThread monitoredThread) {
		this.monitoredThread = monitoredThread;
		progressBar.progressProperty().bindBidirectional(monitoredThread.getProgress());
		currentFileLabel.textProperty().bindBidirectional(monitoredThread.getCurrentFileName());
		folderFileNameLabel.setText(monitoredThread.getNameToDisplay());
		monitoredThread.getDoneSize().addListener((observable, oldValue, newValue) -> {
			progressLabel.textProperty().setValue(newValue + "/" + monitoredThread.getWholeSize());
		});
	}

}
