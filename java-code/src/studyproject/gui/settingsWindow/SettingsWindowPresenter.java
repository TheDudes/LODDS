package studyproject.gui.settingsWindow;

import java.io.*;
import java.net.URL;
import java.util.Map.Entry;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import studyproject.API.Errors.ErrorFactory;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.App;
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

	private Logger logger;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		logger = Logger.getGlobal();
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
		if (applySettings())
			cancelSettings();
	}

	/**
	 * Action that happens when pressing the 'Apply' button. Saves the Key-Value
	 * pairs to the properties file
	 * Returns true if input is valid and data was saved successfully, otherwise false
	 */
	private Boolean applySettings() {
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

					// Show error message if username is invalid
					if (label.getText().equals("userName") && UserInfo.validateUserName(textField.getText()) == false) {
						showInputError("Please make sure to choose a valid username.");
						return false;
					}

					App.properties.setProperty(label.getText(), textField.getText());
					break;

				}
			}
		}
		try {
			App.properties.store(new FileOutputStream(App.pathToProperties), null);
			logger.log(ErrorFactory.build(Level.INFO, LogKey.info, "Saved properties to " + App.pathToProperties));
			App.properties.load(new FileInputStream(new File(App.pathToProperties)));
			return true;
		} catch (FileNotFoundException e) {
			logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, "FileNotFoundException thrown: ", e));
		} catch (IOException e) {
			logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, "IOException thrown: ", e));
		}

		return false;
	}

	private void showInputError(String msg) {
		Alert alert = new Alert(AlertType.ERROR);
		alert.setTitle("Error");
		alert.setHeaderText("Invalid input");
		alert.setContentText(msg);
		alert.showAndWait();
	}

	/**
	 * Action that happens when pressing the 'Cancel' button. Close the
	 * 'Settings' window
	 */
	private void cancelSettings() {
		settingsGrid.getScene().getWindow().hide();
	}

}
