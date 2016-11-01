package studyproject.gui.mainWindow.filesTree;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeView;
import studyproject.API.Core.File.FileInfo;

public class FilesTreePresenter implements Initializable{

	@FXML TreeView<FileInfo> filesTreeView;
	@FXML TextField filesTreeSearch;
	
	@Override
	public void initialize(URL location, ResourceBundle resources) {
		// TODO Auto-generated method stub
		
	}

}
