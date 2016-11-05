package studyproject.logging;

import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

/**
 * Logs logs to the specified file or to "LoddsLogFile.txt" if no file was
 * specified
 * 
 * @author ninti
 *
 */
public class FileLogHandler extends Handler {

	private FileOutputStream fileOutputStream;
	private PrintWriter printWriter;
	private Logger logger = Logger.getGlobal();

	public FileLogHandler(String filename) {
		super();

		if (filename == null || filename.isEmpty())
			filename = "LoddsLogFile.txt";

		try {
			// initialize file
			if (!Files.exists(Paths.get(filename))) {
				Files.createFile(Paths.get(filename));
			}
			fileOutputStream = new FileOutputStream(filename);
			printWriter = new PrintWriter(fileOutputStream);
			setFormatter(new LogRecFormatter());
		} catch (Exception e) {
			logger.log(Level.SEVERE, e.getMessage());
		}
	}

	@Override
	public void publish(LogRecord record) {
		if (!isLoggable(record))
			return;

		printWriter.println(getFormatter().format(record));
	}

	@Override
	public void flush() {
		printWriter.flush();
	}

	@Override
	public void close() throws SecurityException {
		printWriter.close();
	}
}