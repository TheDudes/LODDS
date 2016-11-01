package studyproject.gui.mainWindow.logArea;

import java.net.URL;
import java.util.ResourceBundle;
import java.util.logging.Level;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import studyproject.API.Errors.Error;
import studyproject.logging.APILvl;
import studyproject.logging.LogKey;

public class LogAreaPresenter implements Initializable {

	@FXML
	CheckBox errorCB;
	@FXML
	CheckBox infoCB;
	@FXML
	CheckBox getRecCB;
	@FXML
	CheckBox getSentCB;
	@FXML
	CheckBox broadcastCB;
	@FXML
	TableView<Error> logTableView;
	@FXML
	TableColumn<Error, String> logKeyCol;
	@FXML
	TableColumn<Error, String> apiLvlCol;
	@FXML
	TableColumn<Error, String> thrownByCol;
	@FXML
	TableColumn<Error, String> msgCol;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		logKeyCol.setCellValueFactory(new PropertyValueFactory<Error, String>("logKey"));
		apiLvlCol.setCellValueFactory(new PropertyValueFactory<Error, String>("apiLvl"));
		thrownByCol.setCellValueFactory(new PropertyValueFactory<Error, String>("thrownBy"));
		msgCol.setCellValueFactory(new PropertyValueFactory<Error, String>("msg"));

		ObservableList<Error> errors = FXCollections.observableArrayList(
				new Error(Level.SEVERE, LogKey.getReceived, APILvl.gui, "init", "hello there son"));
		logTableView.setItems(errors);
	}

}
