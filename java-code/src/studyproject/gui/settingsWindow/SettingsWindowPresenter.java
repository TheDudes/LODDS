package studyproject.gui.settingsWindow;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Map.Entry;
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

/**
 * Settings window. Load and edit properties
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
		okButton.setOnAction(ok -> okPressed());
		applyButton.setOnAction(apply -> applyPressed());
		cancelButton.setOnAction(cancel -> cancelPressed());
		loadSettings();
	}

	/**
	 * Load Key-Value pairs from the properties file 
	 */
	private void loadSettings() {
		int numberOfRows = 0;
		// load pathToUserProperties only from the deafaultProperties
		settingsGrid.addRow(numberOfRows++, new Label("pathToUserProperties"), 
				new TextField(App.defaultProperties.getProperty("pathToUserProperties")));
		// load other properties from the userProperties
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
	 * Action that happens when pressing the 'OK' button.
	 * Executes functionality of the 'Apply' and 'Cancel' buttons
	 */
	private void okPressed() {
		applyPressed();
		cancelPressed();
	}

	/**
	 * Action that happens when pressing the 'Apply' button.
	 * Saves the Key-Value pairs to the properties file
	 */
	private void applyPressed() {
		ObservableList<Node> observList = settingsGrid.getChildren();
		Label label;
		TextField textField;

		for (Node l : observList) {
			if (l.getClass() != Label.class)
				continue;
			label = (Label) l;

			// find the value holding textField which is next to the Label
			for (Node tf : observList) {
				if (GridPane.getRowIndex(tf) == GridPane.getRowIndex(l)
						&& GridPane.getColumnIndex(tf) == GridPane.getColumnIndex(l) + 1) {
					textField = (TextField) tf;
					
					// write pathToUserProperties only to the defaultProperties
					// This should not be included in the userProperties
					if (label.getText() == "pathToUserProperties") {
						App.defaultProperties.setProperty(label.getText(), textField.getText());
					} else {
						App.properties.setProperty(label.getText(), textField.getText());
					}
					break;
				}
			}
		}
		try {
			FileOutputStream out = new FileOutputStream(App.pathToProperties);
			App.defaultProperties.store(out, null);
			out.close();
		} catch (FileNotFoundException e) {
			// TODO Error handling
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Error handling
			e.printStackTrace();
		}
		try {
			String path = (String) App.defaultProperties.get("pathToUserProperties");
			FileOutputStream out = new FileOutputStream(path);
			App.properties.store(out, null);
			out.close();
		} catch (FileNotFoundException e) {
			// TODO Error handling
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Error handling
			e.printStackTrace();
		}
	}

	/**
	 * Action that happens when pressing the 'Cancel' button.
	 * Close the 'Settings' window
	 */
	private void cancelPressed() {
		settingsGrid.getScene().getWindow().hide();
	}

}
