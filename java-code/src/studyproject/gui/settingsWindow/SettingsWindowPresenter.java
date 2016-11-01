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
		for (Entry<Object, Object> entry : App.properties.entrySet()) {
			settingsGrid.addRow(numberOfRows++, new Label((String) entry.getKey()),
					new TextField((String) entry.getValue()));
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

			for (Node tf : observList) {
				if (GridPane.getColumnIndex(tf) == GridPane.getColumnIndex(l) + 1
						&& GridPane.getRowIndex(tf) == GridPane.getRowIndex(l)) {
					textField = (TextField) tf;
					App.properties.setProperty(label.getText(), textField.getText());
				}
			}
		}
		try {
			App.properties.store(new FileOutputStream(App.pathToProperties), null);
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
