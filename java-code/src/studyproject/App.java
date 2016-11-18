package studyproject;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.inject.Inject;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.stage.Modality;
import javafx.stage.Stage;
import studyproject.API.Errors.ErrLog;
import studyproject.gui.mainWindow.MainWindowView;
import studyproject.gui.selectedInterface.SelectedInterfaceModel;
import studyproject.gui.selectedInterface.SelectedInterfaceView;
import studyproject.logging.APILvl;
import studyproject.logging.FileLogHandler;
import studyproject.logging.LogConsoleHandler;
import studyproject.logging.LogKey;

public class App extends Application{

	private static Logger logger;
	public static Properties properties;
	public static String pathToProperties;
	private MainWindowView mainView;
	private SelectedInterfaceView selectedInterfaceView;
	
	@Inject SelectedInterfaceModel selectedInterfaceModel;
	
	public void configureLogging() {
		logger = Logger.getGlobal();
		logger.setUseParentHandlers(false);
		logger.addHandler(new LogConsoleHandler(Level.ALL));
		logger.addHandler(new FileLogHandler(properties.getProperty("pathToLogFile")));
		logger.setLevel(Level.ALL);
	}

	public int loadProperties(String pathToUserProperties) {
		properties = new Properties();
		
		try {
			properties.load(getClass().getResourceAsStream("resources/lodds.properties"));
		} catch (FileNotFoundException e) {
			return 4;
		} catch (IOException e) {
			return 4;
		}
		return 0;
	}
	
	
	
	@Override
	public void start(Stage mainStage) throws Exception {
		selectedInterfaceView = new SelectedInterfaceView();
		Stage interfaceStage = new Stage();
		interfaceStage.setTitle("Startup...");
		interfaceStage.setScene(new Scene(selectedInterfaceView.getView()));
		interfaceStage.initModality(Modality.APPLICATION_MODAL);
		interfaceStage.showAndWait();
		
		mainStage.setTitle("Local Open Distributed Data Sharing");
		mainView = new MainWindowView();
		Scene mainScene = new Scene(mainView.getView());
		mainStage.setScene(mainScene);
		mainStage.show();
	}
	

	public static void main(String... args) {
		App application = new App();
		pathToProperties = args[0];
		int errorCode;
		if ((errorCode = application.loadProperties(pathToProperties)) > 0) {
			ErrLog.log(Level.SEVERE, LogKey.error, APILvl.gui,errorCode, "loadProperties");
		}
		application.configureLogging();
		launch(args);

	}
	
	
	
}
