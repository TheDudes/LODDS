package studyproject.gui.Core;

import java.io.File;
import java.util.List;

import javax.inject.Inject;

import javafx.scene.Node;
import javafx.scene.image.Image;
import javafx.scene.layout.AnchorPane;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.Modality;
import javafx.stage.Stage;
import studyproject.App;
import studyproject.gui.mainWindow.MainWindowModel;

/**
 * Multiple used functions for GUI Lvl
 * 
 * @author Chris
 */
public class Utils {
	@Inject
	MainWindowModel mainWindowModel;

	/**
	 * Open a FileChooser to select one single directory
	 * 
	 * @param title
	 *            The title of the FileChooser window
	 * @return String representing the path of the chosen directory or null if
	 *         no directory has been selected
	 */
	public static String getChoosenDirPath(String title) {
		DirectoryChooser directoryChooser = new DirectoryChooser();
		File savePathFile = new File((String) App.properties.get("defaultSavePath"));

		if (savePathFile.exists()) {
			directoryChooser.setInitialDirectory(savePathFile);
		}
		directoryChooser.setTitle(title);
		Stage stage = new Stage();
		stage.initModality(Modality.APPLICATION_MODAL);
		File chosenFolder = directoryChooser.showDialog(stage);

		if (chosenFolder == null) {
			return null;
		}
		return chosenFolder.getPath();
	}

	/**
	 * Open a FileChooser to select multiple files from one directory
	 * 
	 * @param title
	 *            The title of the FileChooser window
	 * @return List of selected files or null if no files were selected
	 */
	public static List<File> getChoosenMultipleFiles(String title) {
		FileChooser fileChooser = new FileChooser();
		fileChooser.setTitle(title);
		Stage stage = new Stage();
		stage.initModality(Modality.APPLICATION_MODAL);
		return fileChooser.showOpenMultipleDialog(stage);
	}

	/**
	 * Open a FileChooser to select one single file
	 * 
	 * @param title
	 *            The title of the FileChooser window
	 * @return The selected File or null if no file has been selected
	 */
	public static File getChoosenFile(String title) {
		FileChooser fileChooser = new FileChooser();
		fileChooser.setTitle(title);
		Stage stage = new Stage();
		stage.initModality(Modality.APPLICATION_MODAL);
		return fileChooser.showOpenDialog(stage);
	}

	public static void setAllAnchorPoints(Node child, double value) {
		AnchorPane.setBottomAnchor(child, value);
		AnchorPane.setTopAnchor(child, value);
		AnchorPane.setLeftAnchor(child, value);
		AnchorPane.setRightAnchor(child, value);
	}

	public static Boolean osIsMac() {
		return System.getProperty("os.name").toLowerCase().indexOf("mac") >= 0;
	}

	public static void addLoddsImageToStage(Stage stage) {
		stage.getIcons().add(new Image(Utils.class.getResourceAsStream(App.ICON_PATH + "lodds.png")));
	}
}
