package studyproject.API.Lvl.Mid;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.logging.Level;
import java.util.logging.Logger;

import studyproject.API.Errors.ErrorFactory;
import studyproject.API.Lvl.Low.Responses;
import studyproject.logging.LogKey;

/**
 * This Thread is used to receive a file if another user sent a "get-send
 * permission" request. Use Thread.start() to send the "OK" flag and start the
 * file transfer.
 * 
 * @author ninti
 *
 */
public class GetFileWPThread extends Thread {

	private Socket socket;
	private String pathToSaveTo;
	private long fileSize;
	private String fileName;
	private Logger logger = Logger.getGlobal();

	/**
	 * The constructor to create a new get file with permission thread
	 * 
	 * @param socket
	 *            the socket where the request was sent to
	 * @param pathToSaveTo
	 *            the path to save the file to
	 * @param fileName
	 *            the name of the file which will be received
	 * @param fileSize
	 *            the size of the file which will be received
	 */
	public GetFileWPThread(Socket socket, String pathToSaveTo, String fileName, long fileSize) {
		this.socket = socket;
		this.pathToSaveTo = pathToSaveTo;
		this.fileSize = fileSize;
		this.fileName = fileName;
	}

	@Override
	public void run() {
		FileOutputStream fileOutStream = null;
		int error;
		try {
			// Create the parentDirectory and the file, if it does not exist
			if (!Files.exists(Paths.get(pathToSaveTo + "/" + fileName))) {
				Files.createDirectories(Paths.get(pathToSaveTo));
				Files.createFile(Paths.get(pathToSaveTo + "/" + fileName));
			}
			// create the fileoutputstream to write the file to the filesystem
			fileOutStream = new FileOutputStream((Paths.get(pathToSaveTo).resolve(fileName)).toString());

			if ((error = Responses.respondSendPermission(socket, fileOutStream, fileSize)) != 0)
				logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, error));

		} catch (FileNotFoundException e) {
			logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, "FileNotFoundException thrown ", e));
		} catch (IOException e) {
			logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, "IOException thrown ", e));

		} finally {
			try {
				if (fileOutStream != null)
					fileOutStream.close();
			} catch (IOException e) {
				logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, "IOException thrown ", e));
			}
		}
	}
}
