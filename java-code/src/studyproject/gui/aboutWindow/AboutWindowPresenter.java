package studyproject.gui.aboutWindow;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import studyproject.App;

public class AboutWindowPresenter implements Initializable {

	@FXML
	private Label currentVersion;

	@FXML
	private Hyperlink homepage;

	@FXML
	private ImageView iconView;

	private Image iconImage;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		currentVersion.setText(App.properties.getProperty("currentVersion"));
		if (Boolean.valueOf(App.properties.getProperty("icons"))) {
			iconImage = new Image(getClass().getResourceAsStream(App.ICON_PATH + "lodds.png"));
			iconView.setFitHeight(64.0);
			iconView.setFitWidth(64.0);
			iconView.setImage(iconImage);
		}

	}

	@FXML
	public void showHomepage() {
		if (App.hostServices != null)
			App.hostServices.showDocument(homepage.getText());
	}

}