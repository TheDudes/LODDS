package studyproject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.inject.Inject;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.stage.Stage;
import studyproject.API.Errors.ErrorFactory;
import studyproject.gui.mainWindow.MainWindowPresenter;
import studyproject.gui.mainWindow.MainWindowView;
import studyproject.gui.selectedInterface.SelectedInterfaceModel;
import studyproject.logging.FileLogHandler;
import studyproject.logging.LogConsoleHandler;
import studyproject.logging.LogKey;

public class App extends Application {

	private static Logger logger;
	public static Properties properties;
	public static String pathToProperties = System.getProperty("user.home") + System.getProperty("file.separator")
			+ ".lodds" + System.getProperty("file.separator") + "config.properties";
	private MainWindowView mainView;

	@Inject
	SelectedInterfaceModel selectedInterfaceModel;

	public void configureLogging() {
		logger = Logger.getGlobal();
		logger.setUseParentHandlers(false);
		logger.addHandler(new LogConsoleHandler(Level.ALL));
		logger.addHandler(new FileLogHandler(properties.getProperty("pathToLogFile")));
		logger.setLevel(Level.ALL);
	}

	/**
	 * Create a properties file if not existent in the home directory of the
	 * user Load default properties first then load properties changed by the
	 * user
	 * 
	 * @return ErrorFactory value
	 */
	public int loadProperties() {
		File propertiesFile = new File(pathToProperties);
		Logger logger = Logger.getGlobal();
		properties = new Properties();
		try {
			properties.load(getClass().getResourceAsStream("resources/lodds.properties"));
			if (!propertiesFile.exists()) {
				Files.createDirectories(Paths.get(propertiesFile.getParent()));
				propertiesFile.getParentFile().mkdirs();
				propertiesFile.createNewFile();
				
				// Todo strip all invalid characters
				properties.put("userName", System.getProperty("user.name"));
				properties.store(new FileOutputStream(propertiesFile), null);
				logger.log(ErrorFactory.build(Level.INFO, LogKey.info,
						"new propertiesfile created at " + propertiesFile.getAbsolutePath()));
			}
			properties.load(new FileInputStream(propertiesFile));
			// properties.store(new FileOutputStream(propertiesFile), null);
			logger.log(ErrorFactory.build(Level.INFO, LogKey.info,
					"using properties from " + propertiesFile.getAbsolutePath()));
		} catch (IOException e) {
			logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, e));
			return 1;
		}

		return 0;
	}

	@Override
	public void start(Stage mainStage) throws Exception {
		mainStage.setTitle("Local Open Distributed Data Sharing");
		mainStage.setMinHeight(400);
		mainStage.setMinWidth(600);
		mainView = new MainWindowView();
		Scene mainScene = new Scene(mainView.getView());
		mainStage.setScene(mainScene);
		mainStage.show();
		MainWindowPresenter mainWindowPresenter = (MainWindowPresenter) mainView.getPresenter();
		mainWindowPresenter.loadInterface();
	}

	public static void main(String... args) {
		App application = new App();

		application.loadProperties();

		application.configureLogging();
		launch(args);

	}
}