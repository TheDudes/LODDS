package studyproject.gui.Core;

import java.io.File;
import java.util.List;

import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import studyproject.App;

/**
 * Multiple used functions for GUI Lvl
 * 
 * @author Chris
 */
public class Utils {

	/**
	 * 
	 * @return choosen directorypath
	 */
	public static String getChoosenDirPath(String title) {
		DirectoryChooser directoryChooser = new DirectoryChooser();
		File savePathFile = new File((String) App.properties.get("defaultSavePath"));

		if (savePathFile.exists()) {
			directoryChooser.setInitialDirectory(savePathFile);
		}
		directoryChooser.setTitle(title);
		File chosenFolder = directoryChooser.showDialog(new Stage());

		if (chosenFolder == null) {
			return null;
		}
		return chosenFolder.getPath();
	}

	public static List<File> getChoosenMultipleFiles(String title) {
		FileChooser fileChooser = new FileChooser();
		fileChooser.setTitle(title);
		return fileChooser.showOpenMultipleDialog(new Stage());
	}

	public static File getChoosenFile(String title) {
		FileChooser fileChooser = new FileChooser();
		fileChooser.setTitle(title);
		return fileChooser.showOpenDialog(new Stage());
	}

}
