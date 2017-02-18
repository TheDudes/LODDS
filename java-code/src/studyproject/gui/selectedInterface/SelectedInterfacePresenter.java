package studyproject.gui.selectedInterface;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.inject.Inject;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ListView;
import studyproject.App;
import studyproject.API.Errors.ErrorFactory;
import studyproject.API.Lvl.Low.Broadcast;
import studyproject.gui.mainWindow.MainWindowModel;
import studyproject.logging.LogKey;

public class SelectedInterfacePresenter implements Initializable {

	@FXML
	Button okBut;
	@FXML
	Button cancelBut;
	@FXML
	ListView<String> interfaceList;
	@FXML
	CheckBox defaultCB;
	@Inject
	SelectedInterfaceModel selectedInterfaceModel;

	@Inject
	MainWindowModel mainWindowModel;

	private ArrayList<String> interfaces;
	private ObservableList<String> interfacesObs;
	private Logger logger;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		logger = Logger.getGlobal();
		interfaces = new ArrayList<>();
		interfacesObs = FXCollections.observableArrayList();
		okBut.setOnAction(e -> okButClicked());
		cancelBut.setOnAction(e -> interfaceList.getScene().getWindow().hide());
		int errorCode;
		if ((errorCode = Broadcast.getNetworkAddresses(interfaces)) != 0) {
			logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, errorCode));
		}
		interfacesObs.addAll(interfaces);
		selectedInterfaceModel.setAvailableInterfaces(interfacesObs);
		interfaceList.setItems(interfacesObs);

	}

	private void okButClicked() {
		String selectedInterface = interfaceList.getSelectionModel().getSelectedItem();
		if (selectedInterface == null || selectedInterface.isEmpty())
			return;
		if (defaultCB.isSelected()) {
			App.properties.setProperty("defaultInterface", selectedInterface);
			try {
				App.properties.store(new FileOutputStream(new File(App.pathToProperties)), null);
			} catch (IOException e) {
				logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, "IOException thrown: ", e));
			}
		}
		mainWindowModel.getLodds().startUp(selectedInterface, (String) App.properties.get("userName"));
		interfaceList.getScene().getWindow().hide();
	}

}
