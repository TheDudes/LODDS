package studyproject.gui.aboutWindow;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import studyproject.App;

public class AboutWindowPresenter implements Initializable {

	@FXML
	private Label currentVersion;

	@FXML
	private Hyperlink homepage;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		currentVersion.setText(App.properties.getProperty("currentVersion"));
	}

	@FXML
	public void showHomepage() {
		App.hostServices.showDocument(homepage.getText());
	}

}