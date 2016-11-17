package studyproject.gui.selectedInterface;

import java.net.URL;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.inject.Inject;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import studyproject.API.Lvl.Low.Broadcast;
import studyproject.API.Lvl.Mid.LODDS;
import studyproject.gui.mainWindow.MainWindowModel;

public class SelectedInterfacePresenter implements Initializable {

	@FXML
	Button okBut;
	@FXML
	Button cancelBut;
	@FXML
	ListView<String> interfaceList;

	@Inject
	SelectedInterfaceModel selectedInterfaceModel;

	@Inject
	MainWindowModel mainWindowModel;

	private ArrayList<String> interfaces;
	private ObservableList<String> interfacesObs;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		interfaces = new ArrayList<>();
		interfacesObs = FXCollections.observableArrayList();
		okBut.setOnAction(e -> okButClicked());
		cancelBut.setOnAction(e -> interfaceList.getScene().getWindow().hide());

		Broadcast.getNetworkAddresses(interfaces);
		interfacesObs.addAll(interfaces);

		interfaceList.setItems(interfacesObs);

	}

	private void okButClicked() {
		String selectedInterface = interfaceList.getSelectionModel().getSelectedItem();
		if (selectedInterface == null || selectedInterface.isEmpty())
			return;
		Broadcast.getLocalIp(selectedInterface, selectedInterfaceModel.getNetworkAddress());
		Broadcast.getBroadcastAddress(selectedInterface, selectedInterfaceModel.getBroadcastAddress());
		LODDS lodds = new LODDS(selectedInterface, "Nintinugga");
		mainWindowModel.setLodds(lodds);
		mainWindowModel.getLodds().startAdvertising();
		interfaceList.getScene().getWindow().hide();
	}

}
