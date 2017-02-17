package studyproject.gui.settingsWindow;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.ResourceBundle;

import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import studyproject.App;
import studyproject.API.Errors.ErrLog;
import studyproject.logging.APILvl;
import studyproject.logging.LogKey;

/**
 * Settings window. Load and edit properties
 * 
 * @author chris
 *
 */
public class SettingsWindowPresenter implements Initializable {

	@FXML
	Button applyButton;
	@FXML
	Button okButton;
	@FXML
	Button cancelButton;
	@FXML
	GridPane settingsGrid;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		okButton.setOnAction(ok -> okSettings());
		applyButton.setOnAction(apply -> applySettings());
		cancelButton.setOnAction(cancel -> cancelSettings());
		loadSettings();
	}

	/**
	 * Load Key-Value pairs from the properties file
	 */
	private void loadSettings() {
		int numberOfRows = 0;
		for (Entry<Object, Object> entry : App.properties.entrySet()) {
			if ((String) entry.getKey() == "pathToUserProperties") {
				continue;
			}
			settingsGrid.addRow(numberOfRows++, new Label((String) entry.getKey()),
					new TextField((String) entry.getValue()));
		}
		for (Node node : settingsGrid.getChildren()) {
			GridPane.setVgrow(node, Priority.ALWAYS);
		}
	}

	/**
	 * Action that happens when pressing the 'OK' button. Executes functionality
	 * of the 'Apply' and 'Cancel' buttons
	 */
	private void okSettings() {
		applySettings();
		cancelSettings();
	}

	/**
	 * Action that happens when pressing the 'Apply' button. Saves the Key-Value
	 * pairs to the properties file
	 */
	private void applySettings() {
		ObservableList<Node> observList = settingsGrid.getChildren();
		Label label;
		TextField textField;

		for (Node l : observList) {
			if (l.getClass() != Label.class) {
				continue;
			}
			label = (Label) l;

			// find the value holding textField which is next to the Label
			for (Node tf : observList) {
				if (GridPane.getRowIndex(tf) == GridPane.getRowIndex(l)
						&& GridPane.getColumnIndex(tf) == GridPane.getColumnIndex(l) + 1) {
					textField = (TextField) tf;
					App.properties.setProperty(label.getText(), textField.getText());
					break;
				}
			}
		}
		try {
			App.properties.store(new FileOutputStream(App.pathToProperties), null);
			ErrLog.log(Level.INFO, LogKey.info, APILvl.gui, "applySettings",
					"Saved properties to " + App.pathToProperties);
			App.properties.load(new FileInputStream(new File(App.pathToProperties)));
		} catch (FileNotFoundException e) {
			ErrLog.log(Level.SEVERE, LogKey.error, APILvl.gui, getClass().getName() + "applySettings()",
					"FileNotFoundException thrown: " + e.getStackTrace());
		} catch (IOException e) {
			ErrLog.log(Level.SEVERE, LogKey.error, APILvl.gui, getClass().getName() + "applySettings()",
					"IOException thrown: " + e.getStackTrace());
		}
	}

	/**
	 * Action that happens when pressing the 'Cancel' button. Close the
	 * 'Settings' window
	 */
	private void cancelSettings() {
		settingsGrid.getScene().getWindow().hide();
	}

}
