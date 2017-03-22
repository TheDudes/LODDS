package studyproject.gui.mainWindow.tasksList.singleTask;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
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

	private ImageView cancelButtonIV;

	@Override
	public void initialize(URL url, ResourceBundle resourceBundle) {
		cancelButtonIV = new ImageView();
		cancelButtonIV.setPreserveRatio(true);
		cancelButtonIV.setFitWidth(12.0);
		cancelButtonIV.setFitHeight(12.0);
	}

	public void setMonitoredThread(MonitoredThread monitoredThread, Image cancelButtonImage) {
		this.monitoredThread = monitoredThread;
		cancel.setOnAction((e) -> this.monitoredThread.setRunning(false));

		cancel.setText(null);
		cancelButtonIV.setImage(cancelButtonImage);
		cancel.setGraphic(cancelButtonIV);

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
				mainWindowModel.getLodds().getLoddsModel().getTasksList().remove(this.monitoredThread);
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
