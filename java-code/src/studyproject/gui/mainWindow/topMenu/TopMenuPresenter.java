package studyproject.gui.mainWindow.topMenu;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.MenuItem;
import javafx.stage.Modality;
import javafx.stage.Stage;
import studyproject.gui.settingsWindow.SettingsWindowView;

public class TopMenuPresenter implements Initializable {

	public static Stage stage;
	
	@FXML
	MenuItem settingsItem;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		settingsItem.setOnAction(e -> settingsItemPressed());
	}

	public void settingsItemPressed() {
		stage = new Stage();
		SettingsWindowView settingsView = new SettingsWindowView();
		stage.setScene(new Scene(settingsView.getView()));
		stage.initModality(Modality.APPLICATION_MODAL);
		stage.show();
	}

}
