package studyproject;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import studyproject.API.Errors.ErrLog;
import studyproject.logging.APILvl;
import studyproject.logging.LogConsoleHandler;
import studyproject.logging.LogKey;

public class App {

	private static Logger logger;
	public static Properties properties;

	public void configureLogging() {
		logger = Logger.getGlobal();
		logger.setUseParentHandlers(false);
		logger.addHandler(new LogConsoleHandler(Level.ALL));
		logger.setLevel(Level.ALL);
	}

	public int loadProperties(String pathToProperties) {
		properties = new Properties();
		try {
			properties.load(new FileInputStream(pathToProperties));
		} catch (FileNotFoundException e) {
			return 4;
		} catch (IOException e) {
			return 4;
		}
		return 0;
	}

	public static void main(String... args) {
		App application = new App();
		application.configureLogging();
		int errorCode;
		if ((errorCode = application.loadProperties(args[0])) > 0) {
			ErrLog.log(Level.SEVERE, LogKey.error, APILvl.gui,errorCode, "loadProperties");
		}
	

	}
}
