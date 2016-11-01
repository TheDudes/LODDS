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

	private int numberOfRows = 0;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		System.out.println("SettingsWindow");
		okButton.setOnAction(ok -> okPressed());
		applyButton.setOnAction(apply -> applyPressed());
		cancelButton.setOnAction(cancel -> cancelPressed());
		loadSettings();
	}

	private void loadSettings() {
		for (Entry<Object, Object> entry : App.properties.entrySet()) {
			settingsGrid.addRow(numberOfRows++, new Label((String) entry.getKey()),
					new TextField((String) entry.getValue()));
		}
	}

	private void okPressed() {
		applyPressed();
		cancelPressed();
	}

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

	private void cancelPressed() {
		System.out.println("Pressed Cancle. Nothing happens");
		settingsGrid.getScene().getWindow().hide();
	}

}
