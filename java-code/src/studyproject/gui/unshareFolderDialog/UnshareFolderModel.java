package studyproject.gui.unshareFolderDialog;


import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class UnshareFolderModel {
	
	private ObservableList<String> sharedFolderList = FXCollections.observableArrayList();
	
	public ObservableList<String> getSharedFolderList() {
		return sharedFolderList;
	}
	
}
